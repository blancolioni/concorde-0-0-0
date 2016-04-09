with WL.Random;

with Memor;

with Concorde.Empires.Logging;
with Concorde.Empires.Relations;

with Concorde.Galaxy;

with Concorde.Ships.Db;
with Concorde.Ships.Lists;

package body Concorde.AI.Default is

   Max_Battle_Damage : constant := 0.1;
   Max_Defence_Damage : constant := 0.2;
   Max_Explore_Damage : constant := 0.6;

   procedure Shuffle (V : in out Destination_Vectors.Vector);

   type Root_Default_AI_Type is
     new Root_AI_Type with
      record
         Opportunity_Destinations : Destination_Vectors.Vector;
      end record;

   overriding procedure Allocate_Ships
     (AI : in out Root_Default_AI_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class);

   overriding function Awake (AI : Root_Default_AI_Type) return Boolean;

   overriding function Minimum_Attack_Factor
     (AI : Root_Default_AI_Type)
      return Non_Negative_Real;

   overriding procedure Order_Ship
     (AI     : in out Root_Default_AI_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      Ship   : in out Concorde.Ships.Root_Ship_Type'Class);

   overriding procedure Start
     (AI     : in out Root_Default_AI_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class);

   overriding procedure System_Acquired
     (AI           : in out Root_Default_AI_Type;
      Empire       : in out Concorde.Empires.Root_Empire_Type'Class;
      System       : Concorde.Systems.Root_Star_System_Type'Class;
      Former_Owner : Concorde.Empires.Empire_Type);

   overriding procedure System_Lost
     (AI        : in out Root_Default_AI_Type;
      Empire    : in out Concorde.Empires.Root_Empire_Type'Class;
      System    : Concorde.Systems.Root_Star_System_Type'Class;
      New_Owner : Concorde.Empires.Empire_Type);

   overriding procedure Update_Focus
     (AI     : in out Root_Default_AI_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class);

   overriding procedure Wake (AI : in out Root_Default_AI_Type);

   procedure Update_Ship_Destination
     (Ship        : Concorde.Ships.Ship_Type;
      Destination : Concorde.Systems.Star_System_Type);

   --------------------
   -- Allocate_Ships --
   --------------------

   overriding procedure Allocate_Ships
     (AI     : in out Root_Default_AI_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class)
   is
      use type Concorde.Systems.Star_System_Type;

      Total_Systems      : constant Natural := Empire.Current_Systems;
      Total_Ships        : constant Natural := Empire.Current_Ships;

      function Destination_Planned_Offensive
        (Ship : Concorde.Ships.Ship_Type)
         return Boolean
      is (Ship.Has_Destination
          and then AI.Attack_From /= null
          and then Ship.Destination.Index = AI.Attack_From.Index
          and then Ship.Damage < 0.3);

   begin
      if Total_Ships = 0 or else Total_Systems = 0 then
         return;
      end if;

      AI.Owned_Systems := Total_Systems;
      AI.Owned_Ships   := Total_Ships;
      AI.Threat_Systems     := 0;
      AI.Border_Systems     := 0;
      AI.Unexplored_Systems := 0;
      AI.Frontier_Systems   := 0;
      AI.Internal_Systems   := 0;
      AI.Battle_Systems     := 0;
      AI.Enemy_Strength     := 0;

      AI.Defense_Destinations.Clear;
      AI.Explore_Destinations.Clear;
      AI.Attack_Destinations.Clear;
      AI.Opportunity_Destinations.Clear;
      AI.Battle_Destinations.Clear;

      AI.Available_Strength := 0;
      AI.Required_Strength  := 0;

      for I in 1 .. Concorde.Galaxy.System_Count loop
         declare
            System : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Galaxy.Get_System (I);
         begin

            Empire.Set_Required (System, 0);

            if Empire.Has_Battle (System) then
               declare
                  use type Memor.Database_Reference;
                  List : Concorde.Ships.Lists.List;
                  Us   : Non_Negative_Real := 0.0;
                  Them : Non_Negative_Real := 0.0;
               begin
                  Empires.Logging.Log
                    (Empire,
                     "continuing battle at " & System.Name);

                  AI.Battle_Systems := AI.Battle_Systems + 1;
                  AI.Battle_Destinations.Append (System);

                  System.Get_Ships (List);
                  for Ship of List loop
                     if Ship.Owner.Reference = Empire.Reference then
                        Us := Us + 1.0 - Ship.Damage;
                     elsif Concorde.Empires.Relations.At_War
                       (Empire, Ship.Owner.all)
                     then
                        Them := Them + 1.0 - Ship.Damage;
                     end if;
                  end loop;

                  if Them > Us * 0.8 then
                     Empire.Set_Required
                       (System, 1 + Natural (Them - Us * 0.8));
                     AI.Reinforce_Ships :=
                       AI.Reinforce_Ships + Empire.Required (System);
                  end if;
               end;
            elsif Empire.Is_Neighbour (System) then
               if System.Owned then
                  AI.Threat_Systems := AI.Threat_Systems + 1;
                  declare
                     Enemy_Ships  : constant Natural :=
                                      System.Owner.Current_Ships;
                     Threat_Ships : Natural := System.Ships;
                  begin
                     if Threat_Ships = 0 then
                        AI.Opportunity_Destinations.Append
                          (System);
                     end if;
                     if Enemy_Ships < Empire.Current_Ships then
                        Threat_Ships := Threat_Ships
                          * Enemy_Ships / Empire.Current_Ships;
                     end if;
                     AI.Enemy_Strength := AI.Enemy_Strength + Threat_Ships;
                  end;
                  AI.Attack_Destinations.Append (System);
               else
                  AI.Unexplored_Systems := AI.Unexplored_Systems + 1;
                  AI.Explore_Destinations.Append (System);
                  Empire.Change_Required (System, 1);
                  Empire.Set_Claim (System, True);
               end if;
            elsif Empire.Is_Border (System) then
               AI.Border_Systems := AI.Border_Systems + 1;
               declare
                  Required : Natural := 1;
               begin
                  for N of Galaxy.Neighbours (System.Index) loop
                     if N.Owned and then not Empire.Owned_System (N) then
                        Concorde.Empires.Logging.Log
                          (Empire,
                           N.Name & " owned by " & N.Owner.Name
                           & " threatens " & System.Name
                           & " with" & Natural'Image (N.Ships)
                           & " ships");
                        Required := Required + N.Ships;
                     end if;
                  end loop;

                  if Required > 0 then
                     AI.Defense_Destinations.Append (System);
                     Empire.Change_Required (System, Required);

                     Concorde.Empires.Logging.Log
                       (Empire,
                        System.Name & ": threat level"
                        & Required'Img);
                  end if;
               end;
            elsif Empire.Is_Frontier (System) then
               AI.Frontier_Systems := AI.Frontier_Systems + 1;
               Empire.Set_Required (System, -System.Ships);
            elsif Empire.Is_Internal (System) then
               AI.Internal_Systems := AI.Internal_Systems + 1;
               Empire.Set_Required (System, -System.Ships);
            else
               Empire.Set_Required (System, -System.Ships);
            end if;
         end;
      end loop;

      Concorde.Empires.Logging.Log
        (Empire,
         "contested systems:" & AI.Battle_Systems'Img
         & "; border systems:" & AI.Border_Systems'Img
         & "; frontier systems:" & AI.Frontier_Systems'Img
         & "; threat systems:" & AI.Threat_Systems'Img
         & "; unexplored systems:" & AI.Unexplored_Systems'Img
         & "; internal systems:" & AI.Internal_Systems'Img);

      AI.Nominal_Defense_Ships := AI.Enemy_Strength;
      AI.Defense_Ships :=
        Natural'Min (AI.Enemy_Strength * 4 / 3, Total_Ships);

      if AI.Nominal_Defense_Ships = 0 then
         AI.Defense_Destinations.Clear;
      end if;

      AI.Nominal_Reinforce_Ships := AI.Reinforce_Ships;

      if AI.Defense_Ships + AI.Reinforce_Ships > Total_Ships then
         AI.Defense_Ships :=
           Total_Ships * AI.Defense_Ships
             / (AI.Defense_Ships + AI.Reinforce_Ships);
         AI.Reinforce_Ships := Total_Ships - AI.Defense_Ships;
      end if;

      if AI.Nominal_Reinforce_Ships > 0 then
         for I in 1 .. AI.Battle_Destinations.Last_Index loop
            declare
               Info : Concorde.Systems.Star_System_Type renames
                        AI.Battle_Destinations (I);
            begin
               Empire.Set_Required
                 (Info,
                  Empire.Required (Info)
                  * AI.Reinforce_Ships / AI.Nominal_Reinforce_Ships);
               Concorde.Empires.Logging.Log
                 (Empire,
                  Info.Name
                  & " allocated"
                  & Integer'Image (Empire.Required (Info))
                  & " ships for reinforcement; currently has"
                  & Natural'Image (Info.Ships));
            end;
         end loop;
      end if;

      for I in 1 .. AI.Defense_Destinations.Last_Index loop
         declare
            Info : Concorde.Systems.Star_System_Type renames
                     AI.Defense_Destinations (I);
         begin
            Empire.Set_Required
              (Info,
               Empire.Required (Info)
               * AI.Defense_Ships / AI.Nominal_Defense_Ships);
            Concorde.Empires.Logging.Log
              (Empire,
               Info.Name
               & " allocated"
               & Integer'Image (Empire.Required (Info))
               & " ships for defence; currently has"
               & Natural'Image (Info.Ships));
            Empire.Change_Required
              (Info, -Info.Ships);
         end;
      end loop;

      AI.Exploration_Ships :=
        Natural'Min
          (Total_Ships - AI.Defense_Ships,
           AI.Unexplored_Systems);
      AI.Offense_Ships :=
        Total_Ships - AI.Defense_Ships - AI.Exploration_Ships;

      Concorde.Empires.Logging.Log
        (Empire,
         "total ships:" & Total_Ships'Img
         & "; enemy strength:" & AI.Enemy_Strength'Img
         & "; defense ships:" & AI.Defense_Ships'Img
         & "; exploration:" & AI.Exploration_Ships'Img
         & "; offensive ships:" & AI.Offense_Ships'Img);

      Shuffle (AI.Battle_Destinations);
      Shuffle (AI.Defense_Destinations);
      Shuffle (AI.Explore_Destinations);
      Shuffle (AI.Attack_Destinations);

      if AI.Planned_Offensive then
         if Empire.Owned_System (AI.Target)
           or else not Empire.Owned_System (AI.Attack_From)
           or else (AI.Attack_From /= Empire.Capital
                    and then Empire.Next_Path_Node_Index
                      (Empire.Capital, AI.Attack_From) = 0)
           or else Empire.Next_Path_Node_Index
             (AI.Attack_From, AI.Target) = 0
           or else AI.Target.Ships > AI.Offense_Ships
         then
            Concorde.Empires.Logging.Log
              (Empire,
               "stop " & (if AI.Launch_Offensive then "active" else "planned")
               & " offensive against "
               & AI.Target.Owner.Name
               & " at "
               & AI.Attack_From.Name
               & " because "
               & (if Empire.Owned_System (AI.Target)
                 then "we own the system"
                 elsif not Empire.Owned_System (AI.Attack_From)
                 then "we have lost control of launch site "
                 & AI.Attack_From.Name
                 elsif AI.Attack_From /= Empire.Capital
                 and then Empire.Next_Path_Node_Index
                   (Empire.Capital, AI.Attack_From) = 0
                 then "we are not connected to the launch site "
                 & AI.Attack_From.Name
                   elsif Empire.Next_Path_Node_Index
                   (AI.Attack_From, AI.Target) = 0
                 then "the launch site is no longer connected to the target"
                 elsif AI.Target.Ships > AI.Offense_Ships
                 then "we need at least" & Natural'Image (AI.Target.Ships)
                 & " but have only" & Natural'Image (AI.Offense_Ships)
                 else raise Constraint_Error with
                   "unknown reason for canceling offensive"));
            AI.Planned_Offensive := False;
            AI.Launch_Offensive := False;
            Empire.Set_Attack_Target (AI.Target.all, False);
         end if;
      end if;

      if not AI.Planned_Offensive
        and then not AI.Attack_Destinations.Is_Empty
        and then AI.Offense_Ships > 0
      then

         declare
            Found             : Boolean := False;
            Easiest_System    : Concorde.Systems.Star_System_Type := null;
            Closest_System    : Concorde.Systems.Star_System_Type := null;
            Shortest_Distance : Natural := Natural'Last;
            Least_Opposition  : Natural := Natural'Last;
         begin
            for Info of AI.Attack_Destinations loop
               declare
                  use Concorde.Empires;
                  Relation_Factor : constant Natural :=
                                      Integer
                                        (Empire.Relationship
                                           (Info.Owner.all))
                                      - Minimum_Relationship;
                  D               : constant Natural :=
                                      Empire.Path_Length
                                        (Empire.Capital, Info)
                                        + Relation_Factor;
                  Opp             : constant Natural :=
                                      Info.Ships
                                        + Relation_Factor;
               begin
                  if D < Shortest_Distance then
                     Shortest_Distance := D;
                     Closest_System := Info;
                     Found := True;
                  end if;
                  if Opp < Least_Opposition then
                     Least_Opposition := Opp;
                     Easiest_System := Info;
                     Found := True;
                  end if;
               end;
            end loop;

            if Found then
               if Empire.Path_Length (Empire.Capital, Easiest_System)
                 > Shortest_Distance + 3
               then
                  AI.Target := Closest_System;
               else
                  AI.Target := Easiest_System;
               end if;

               Found := False;
               Closest_System := null;
               Shortest_Distance := Natural'Last;

               if Galaxy.Neighbours (AI.Target, Empire.Capital) then
                  Closest_System := Empire.Capital;
                  Found := True;
               else
                  for N of Galaxy.Neighbours (AI.Target) loop
                     if Empire.Owned_System (N) then
                        declare
                           D    : constant Natural :=
                                    Empire.Path_Length
                                      (Empire.Capital, N);
                        begin
                           if D < Shortest_Distance then
                              Shortest_Distance := D;
                              Closest_System := N;
                              Found := True;
                           end if;
                        end;
                     end if;
                  end loop;
               end if;

               if Found then
                  AI.Attack_From := Closest_System;

                  for N of Galaxy.Neighbours (AI.Attack_From) loop
                     declare
                        OK : Boolean := True;
                     begin
                        for M of Galaxy.Neighbours (N) loop
                           if not Empire.Owned_System (M) then
                              OK := False;
                              exit;
                           end if;
                        end loop;

                        if OK then
                           AI.Attack_From := N;
                           exit;
                        end if;
                     end;
                  end loop;

                  AI.Planned_Offensive := True;
                  AI.Launch_Offensive := False;
               end if;
            end if;
         end;

         if AI.Planned_Offensive then
            Concorde.Empires.Logging.Log
              (Empire,
               "planning attack on " & AI.Target.Name
               & " owned by " & AI.Target.Owner.Name
               & " from " & AI.Attack_From.Name
               & " (distance"
               & Natural'Image
                 (Empire.Path_Length (AI.Attack_From, AI.Target))
               & ")");

            Empire.Set_Attack_Target (AI.Target.all, True);
         end if;
      end if;

      if AI.Planned_Offensive then
         declare
            Available_Ships     : Concorde.Ships.Lists.List;
            Committed_Ships     : Integer := 0;
            Opposition          : Concorde.Ships.Lists.List;
            Opposition_Strength : Non_Negative_Real := 0.0;
         begin
            AI.Attack_From.Get_Ships (Available_Ships);
            AI.Target.Get_Ships (Opposition);

            for Ship of Available_Ships loop
               if Ship.Damage < Max_Battle_Damage then
                  Committed_Ships := Committed_Ships + 1;
               end if;
            end loop;

            for Ship of Opposition loop
               Opposition_Strength := Opposition_Strength +
                 1.0 - Ship.Damage;
            end loop;

            AI.Available_Strength :=
              Committed_Ships +
                Concorde.Ships.Count_Ships
                  (Destination_Planned_Offensive'Access);
            AI.Local_Strength := Committed_Ships;
            AI.Required_Strength :=
              Natural'Max
                (Natural (Opposition_Strength * AI.Minimum_Attack_Factor)
                 + Empire.Required (AI.Attack_From) + 1,
                 5);

            if Committed_Ships >= AI.Required_Strength then
               AI.Launch_Offensive := True;
               Concorde.Empires.Logging.Log
                 (Empire,
                  "launching attack from "
                  & AI.Attack_From.Name
                  & " with"
                  & Natural'Image (Committed_Ships)
                  & " on " & AI.Target.Name
                  & " with"
                  & Natural'Image (Natural (Opposition_Strength))
                  & " owned by " & AI.Target.Owner.Name);
               for Ship of Available_Ships loop
                  if Ship.Damage < Max_Battle_Damage then
                     Update_Ship_Destination (Ship, AI.Target);
                     --  Ship.Set_Destination (AI.Target);
                  end if;
               end loop;

               declare
                  use type Concorde.Empires.Empire_Relationship_Range;
               begin
                  Empire.Set_Relationship (AI.Target.Owner.all, -100);
               end;

            else
               Concorde.Empires.Logging.Log
                 (Empire,
                  "building attack from "
                  & AI.Attack_From.Name
                  & " on " & AI.Target.Name
                  & ": potential strength"
                  & Natural'Image (AI.Attack_From.Ships)
                  & ": active strength"
                  & Natural'Image (Committed_Ships)
                  & "; required"
                  & Natural'Image (AI.Required_Strength)
                  & "; opposition"
                  & Natural'Image (Natural (Opposition_Strength)));
            end if;
         end;
      end if;

      AI.Awake := False;

   end Allocate_Ships;

   -----------
   -- Awake --
   -----------

   overriding function Awake (AI : Root_Default_AI_Type) return Boolean is
   begin
      return AI.Awake;
   end Awake;

   ----------------
   -- Default_AI --
   ----------------

   function Default_AI return AI_Type is
   begin
      return new Root_Default_AI_Type;
   end Default_AI;

   ---------------------------
   -- Minimum_Attack_Factor --
   ---------------------------

   overriding function Minimum_Attack_Factor
     (AI : Root_Default_AI_Type)
      return Non_Negative_Real
   is
   begin
      return AI.Current_Attack_Factor;
   end Minimum_Attack_Factor;

   ----------------
   -- Order_Ship --
   ----------------

   overriding procedure Order_Ship
     (AI     : in out Root_Default_AI_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      Ship   : in out Concorde.Ships.Root_Ship_Type'Class)
   is

      procedure Choose_Destination
        (V : in out Destination_Vectors.Vector;
         Stop : out Boolean);

      ------------------------
      -- Choose_Destination --
      ------------------------

      procedure Choose_Destination
        (V : in out Destination_Vectors.Vector;
         Stop : out Boolean)
      is
         Closest_Index : Natural := 0;
         Minimum_Hops  : Natural := Natural'Last;
      begin
         Stop := False;

         for I in 1 .. V.Last_Index loop
            declare
               Dest : Concorde.Systems.Star_System_Type renames
                        V.Element (I);
               Hops : constant Natural :=
                        Empire.Path_Length
                          (Ship.System, Dest);
            begin
               if Empire.Required (Dest) > 0
                 and then Hops < Minimum_Hops
               then
                  Closest_Index := I;
                  Minimum_Hops  := Hops;
               end if;
            end;
         end loop;

         if Closest_Index /= 0 then
            Stop := True;
            if Minimum_Hops > 0 then
               declare
                  Dest : Concorde.Systems.Star_System_Type renames
                           V (Closest_Index);
               begin
                  Ship.Set_Destination (Dest);
                  Empire.Change_Required (Dest, -1);

                  Empires.Logging.Log
                    (Empire,
                     "ordering " & Ship.Name & " to "
                     & Dest.Name
                     & " at distance"
                     & Minimum_Hops'Img);
               end;
            else
               Empires.Logging.Log
                 (Empire,
                  "ordering " & Ship.Name & " to remain at "
                  & Ship.System.Name);
            end if;
         end if;

      end Choose_Destination;

      Stop : Boolean := False;

      Opportunity_Attack : Boolean := False;

   begin

      Empires.Logging.Log
        (Empire, Ship.Short_Description);

      if Empire.Has_Battle (Ship.System) then
         Ship.Clear_Destination;
         Stop := True;
         Empires.Logging.Log
           (Empire,
            Ship.Name & " fighting at "
            & AI.Target.Name);
      else
         for Opportunity of AI.Opportunity_Destinations loop

            if Galaxy.Neighbours (Ship.System, Opportunity)
              and then Empires.Relations.At_War (Empire, Opportunity.Owner.all)
              and then not Empire.Is_Opportunity_Target (Opportunity)
              and then not Ship.Has_Destination
            then
               Empire.Set_Opportunity_Target (Opportunity, True);
               Ship.Set_Destination (Opportunity);
               Opportunity_Attack := True;
               Empires.Logging.Log
                 (Empire,
                  Ship.Short_Description & " sees attack opportunity at "
                  & Opportunity.Name);
               exit;
            end if;
         end loop;

         if Ship.Has_Destination and then not Opportunity_Attack
           and then (Ship.System = Ship.Destination
                     or else Empire.Next_Path_Node_Index
                       (Ship.System, Ship.Destination) = 0
                     or else (not Empire.Owned_System (Ship.Destination)
                              and then Ship.Destination /= AI.Target
                              and then Ship.Destination.Owned))
         then
            Ship.Clear_Destination;
         end if;
      end if;

      if Stop or else Ship.Has_Destination then
         null;
      elsif AI.Planned_Offensive
        and then Ship.System = AI.Attack_From
      then
         if AI.Launch_Offensive then
            Empires.Logging.Log
              (Empire,
               Ship.Name & " delays offensive because "
               & " no attack order was given");
         else
            Empires.Logging.Log
              (Empire,
               Ship.Name & " waits at "
               & Ship.System.Name
               & " for attack order");
         end if;
         Ship.Clear_Destination;
      elsif Empire.Required (Ship.System) > 0 then
         Empire.Change_Required (Ship.System, -1);
         Ship.Clear_Destination;
         Stop := True;
         Empires.Logging.Log
           (Empire,
            Ship.Name & " required to remain at "
            & Ship.System.Name);
      else

         if AI.Launch_Offensive
           and then Ship.System = AI.Attack_From
           and then Ship.Damage < Max_Battle_Damage
         then
            Empires.Logging.Log
              (Empire,
               Ship.Name & " joins attack on "
               & AI.Target.Name);
            Ship.Set_Destination (AI.Target);
            Stop := True;
         end if;

         if AI.Launch_Offensive
           and then Ship.System = AI.Target
         then
            Empires.Logging.Log
              (Empire,
               Ship.Name & " continues the battle at "
               & AI.Target.Name);
            Ship.Clear_Destination;
            Stop := True;
         end if;

         if not Stop and then Ship.Damage < Max_Battle_Damage then
            Choose_Destination (AI.Battle_Destinations, Stop);
         end if;

         if not Stop and then Ship.Damage < Max_Defence_Damage then
            Choose_Destination (AI.Defense_Destinations, Stop);
         end if;

         if not Stop and then Ship.Damage < Max_Explore_Damage then
            Choose_Destination (AI.Explore_Destinations, Stop);
         end if;

         if not Stop and then AI.Planned_Offensive then
            Empires.Logging.Log
              (Empire,
               "checking planned offensive: required ="
               & AI.Required_Strength'Img
               & "; available ="
               & AI.Available_Strength'Img
               & "; connected: "
               & (if Ship.System = AI.Attack_From
                 then "yes"
                 elsif Empire.Next_Path_Node_Index
                   (Ship.System, AI.Attack_From) /= 0
                 then "yes" else "no"));

            if AI.Required_Strength > AI.Available_Strength
              and then (Ship.System = AI.Attack_From
                        or else Empire.Next_Path_Node_Index
                          (Ship.System, AI.Attack_From) /= 0)
            then
               Stop := True;
               if Ship.System = AI.Attack_From then
                  null;
               else
                  Empires.Logging.Log
                    (Empire,
                     Ship.Name & " heads to "
                     & AI.Attack_From.Name
                     & " for offensive against "
                     & AI.Target.Name);
                  Ship.Set_Destination (AI.Attack_From);
               end if;
               AI.Available_Strength := AI.Available_Strength + 1;
            end if;
         end if;

         if not Stop
           and then Ship.System /= Empire.Capital
         then
            Stop := True;
            Ship.Set_Destination (Empire.Capital);
            Empires.Logging.Log
              (Empire,
               Ship.Name & " returns to capital " & Empire.Capital.Name);
         end if;

         if Stop then
            if Empire.Required (Ship.System) < 0 then
               Empire.Change_Required (Ship.System, 1);
            end if;
         elsif Ship.Damage > 0.0 then
            Empires.Logging.Log
              (Empire,
               Ship.Name & " making repairs at " & Ship.System.Name);
         else
            Empires.Logging.Log
              (Empire,
               Ship.Name & " has no orders");
         end if;
      end if;
   end Order_Ship;

   -------------
   -- Shuffle --
   -------------

   procedure Shuffle (V : in out Destination_Vectors.Vector) is
   begin
      for From_Index in 1 .. V.Last_Index loop
         declare
            To_Index : constant Positive :=
                         WL.Random.Random_Number (1, V.Last_Index);
         begin
            if From_Index /= To_Index then
               declare
                  T : constant Concorde.Systems.Star_System_Type :=
                        V.Element (From_Index);
               begin
                  V.Replace_Element (From_Index, V.Element (To_Index));
                  V.Replace_Element (To_Index, T);
               end;
            end if;
         end;
      end loop;
   end Shuffle;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (AI     : in out Root_Default_AI_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class)
   is
      pragma Unreferenced (AI);

      function Owner
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is (System.Owner /= null
          and then System.Owner.Name = Empire.Name);

      procedure Start_System
        (System : Concorde.Systems.Star_System_Type);

      ------------------
      -- Start_System --
      ------------------

      procedure Start_System
        (System : Concorde.Systems.Star_System_Type)
      is
         Ns : constant Concorde.Galaxy.Array_Of_Star_Systems :=
                Concorde.Galaxy.Neighbours (System);
      begin
         for N of Ns loop
            Empire.Add_Focus (N);
         end loop;
      end Start_System;

   begin
      Concorde.Galaxy.Iterate (Owner'Access, Start_System'Access);
   end Start;

   ---------------------
   -- System_Acquired --
   ---------------------

   overriding procedure System_Acquired
     (AI           : in out Root_Default_AI_Type;
      Empire       : in out Concorde.Empires.Root_Empire_Type'Class;
      System       : Concorde.Systems.Root_Star_System_Type'Class;
      Former_Owner : Concorde.Empires.Empire_Type)
   is
      use Concorde.Empires;
   begin
      if Former_Owner /= null then
         AI.Update_Attack_Factor (Empire, Former_Owner);
      end if;

      if AI.Planned_Offensive
        and then AI.Target.Index = System.Index
      then
         AI.Planned_Offensive := False;
         AI.Launch_Offensive := False;
      end if;

      Empire.Set_Attack_Target (System, False);
      AI.Awake := True;

   end System_Acquired;

   -----------------
   -- System_Lost --
   -----------------

   overriding procedure System_Lost
     (AI        : in out Root_Default_AI_Type;
      Empire    : in out Concorde.Empires.Root_Empire_Type'Class;
      System    : Concorde.Systems.Root_Star_System_Type'Class;
      New_Owner : Concorde.Empires.Empire_Type)
   is
   begin
      AI.Update_Attack_Factor (Empire, New_Owner, Can_Decrease => False);

      if AI.Planned_Offensive
        and then AI.Attack_From.Index = System.Index
      then
         AI.Planned_Offensive := False;
         AI.Launch_Offensive := False;
         Empire.Set_Attack_Target (System, False);
      end if;

      AI.Awake := True;

   end System_Lost;

   ------------------
   -- Update_Focus --
   ------------------

   overriding procedure Update_Focus
     (AI     : in out Root_Default_AI_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class)
   is
      use type Memor.Database_Reference;
      use type Concorde.Empires.Empire_Type;

      function Other_Owner
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is (System.Owner /= null
          and then System.Owner.Reference /= Empire.Reference);

      procedure Add_Focus
        (System : Concorde.Systems.Star_System_Type);

      function Border_System
        (System : Concorde.Systems.Star_System_Type)
         return Boolean;

      function Interior
        (System : Concorde.Systems.Star_System_Type)
         return Boolean;

      function Outnumbered_Defenders
        (System : Concorde.Systems.Star_System_Type)
         return Boolean;

      function Unexplored
        (System : Concorde.Systems.Star_System_Type)
         return Boolean;

      ---------------
      -- Add_Focus --
      ---------------

      procedure Add_Focus
        (System : Concorde.Systems.Star_System_Type)
      is
      begin
         Empire.Add_Focus (System);
      end Add_Focus;

      -------------------
      -- Border_System --
      -------------------

      function Border_System
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is
         use Concorde.Galaxy;
         Ns : constant Array_Of_Star_Systems :=
                Galaxy.Neighbours (System);
      begin
         if not System.Owned_By (Empire) then
            return False;
         end if;

         for N of Ns loop
            if System.Owned and then not System.Owned_By (Empire) then
               return True;
            end if;
         end loop;
         return False;
      end Border_System;

      --------------
      -- Interior --
      --------------

      function Interior
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is
         use Concorde.Galaxy;
         Ns : constant Array_Of_Star_Systems :=
                Galaxy.Neighbours (System);
      begin
         if not System.Owned_By (Empire) then
            return False;
         end if;

         for N of Ns loop
            if N.Owned and then not N.Owned_By (Empire) then
               return False;
            end if;
         end loop;
         return True;
      end Interior;

      ---------------------------
      -- Outnumbered_Defenders --
      ---------------------------

      function Outnumbered_Defenders
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is
         use Concorde.Galaxy;
         Ns : constant Array_Of_Star_Systems :=
                Galaxy.Neighbours (System);
         Defenders : Natural := 0;
         Attackers : Natural := 0;
      begin
         if not System.Owned or else System.Owned_By (Empire) then
            return False;
         end if;

         Defenders := System.Ships;
         for N of Ns loop
            if N.Owned_By (Empire) then
               Attackers := Attackers + N.Ships;
            elsif N.Owner = System.Owner then
               Defenders := Defenders + N.Ships / 2;
            end if;
         end loop;

         if Attackers /= 0 and then Defenders /= 0 then
            declare
               Ratio : constant Real :=
                         Real (Attackers) / Real (Defenders);
               Min   : constant Real :=
                         Root_Default_AI_Type'Class (AI).Minimum_Attack_Factor;
            begin
               if Ratio > Min then
--                    Concorde.Empires.Logging.Log
--                      (Empire,
--                       "ordering attack on " & System.Name
--                       & "; attackers" & Attackers'Img
--                       & "; defenders" & Defenders'Img
--                       & "; ratio = "
--                       & Lui.Approximate_Image (Ratio)
--                       & "; minimum = "
--                       & Lui.Approximate_Image (Min));
                  return True;
               else
                  return False;
               end if;
            end;
         else
            return False;
         end if;
      end Outnumbered_Defenders;

      ----------------
      -- Unexplored --
      ----------------

      function Unexplored
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is
      begin
         if System.Owner /= null then
            return False;
         end if;

         for N of Galaxy.Neighbours (System) loop
            if N.Owned_By (Empire) then
               return True;
            end if;
         end loop;

         return False;

      end Unexplored;

   begin
      Empire.Remove_Focus (Other_Owner'Access);
      Empire.Remove_Focus (Interior'Access);

      Galaxy.Iterate (Border_System'Access, Add_Focus'Access);
      Galaxy.Iterate (Outnumbered_Defenders'Access, Add_Focus'Access);
      Galaxy.Iterate (Unexplored'Access, Add_Focus'Access);
   end Update_Focus;

   procedure Update_Ship_Destination
     (Ship        : Concorde.Ships.Ship_Type;
      Destination : Concorde.Systems.Star_System_Type)
   is
      procedure Update (Ship : in out Concorde.Ships.Root_Ship_Type'Class);

      ------------
      -- Update --
      ------------

      procedure Update (Ship : in out Concorde.Ships.Root_Ship_Type'Class) is
      begin
         Ship.Set_Destination (Destination);
      end Update;

   begin
      Concorde.Ships.Db.Update (Ship.Reference, Update'Access);
   end Update_Ship_Destination;

   ----------
   -- Wake --
   ----------

   overriding procedure Wake (AI : in out Root_Default_AI_Type) is
   begin
      AI.Awake := True;
   end Wake;

end Concorde.AI.Default;
