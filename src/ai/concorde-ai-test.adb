with WL.Random;

with Concorde.Factions.Logging;
with Concorde.Galaxy;

with Concorde.Ships.Lists;

package body Concorde.AI.Test is

   Max_Battle_Damage : constant := 0.1;
   Max_Defence_Damage : constant := 0.2;
   Max_Explore_Damage : constant := 0.6;

   procedure Shuffle (V : in out Destination_Vectors.Vector);

   type Root_Test_AI_Type is
     new Root_AI_Type with
      record
         Opportunity_Destinations : Destination_Vectors.Vector;
      end record;

   overriding procedure Allocate_Ships
     (AI : in out Root_Test_AI_Type);

   overriding function Awake (AI : Root_Test_AI_Type) return Boolean;

   overriding function Minimum_Attack_Factor
     (AI : Root_Test_AI_Type)
      return Non_Negative_Real;

   overriding procedure Order_Ship
     (AI : in out Root_Test_AI_Type;
      Ship : not null access Concorde.Ships.Root_Vessel_Type'Class);

   overriding procedure Start
     (AI     : in out Root_Test_AI_Type;
      Faction : Concorde.Factions.Faction_Type);

   overriding procedure System_Acquired
     (AI           : in out Root_Test_AI_Type;
      System       : Concorde.Systems.Star_System_Type;
      Former_Owner : Concorde.Factions.Faction_Type);

   overriding procedure System_Lost
     (AI        : in out Root_Test_AI_Type;
      System    : Concorde.Systems.Star_System_Type;
      New_Owner : Concorde.Factions.Faction_Type);

   overriding procedure Update_Focus
     (AI : in out Root_Test_AI_Type);

   overriding procedure Wake (AI : in out Root_Test_AI_Type);

   --------------------
   -- Allocate_Ships --
   --------------------

   overriding procedure Allocate_Ships
     (AI : in out Root_Test_AI_Type)
   is
      use type Concorde.Systems.Star_System_Type;

      Total_Systems      : constant Natural := AI.Faction.Current_Systems;
      Total_Ships        : constant Natural := AI.Faction.Current_Ships;

      function Destination_Planned_Offensive
        (Ship : Concorde.Ships.Ship_Type)
         return Boolean
      is (Ship.Destination /= null
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
      AI.Enemy_Strength     := 0;

      AI.Defense_Destinations.Clear;
      AI.Explore_Destinations.Clear;
      AI.Attack_Destinations.Clear;
      AI.Opportunity_Destinations.Clear;

      AI.Available_Strength := 0;
      AI.Required_Strength  := 0;

      for I in 1 .. Concorde.Galaxy.System_Count loop
         declare
            System : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Galaxy.Get_System (I);
         begin

            AI.Faction.Set_Required (System, 0);

            if AI.Faction.Is_Neighbour (System) then
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
                     if Enemy_Ships < AI.Faction.Current_Ships then
                        Threat_Ships := Threat_Ships
                          * Enemy_Ships / AI.Faction.Current_Ships;
                     end if;
                     AI.Enemy_Strength := AI.Enemy_Strength + Threat_Ships;
                  end;
                  AI.Attack_Destinations.Append (System);
               else
                  AI.Unexplored_Systems := AI.Unexplored_Systems + 1;
                  AI.Explore_Destinations.Append (System);
                  AI.Faction.Change_Required (System, 1);
               end if;
            elsif AI.Faction.Is_Border (System) then
               AI.Border_Systems := AI.Border_Systems + 1;
               declare
                  Required : Natural := 1;
               begin
                  for N of Galaxy.Neighbours (System.Index) loop
                     if N.Owned and then not AI.Faction.Owned_System (N) then
                        Concorde.Factions.Logging.Log
                          (AI.Faction,
                           N.Name & " owned by " & N.Owner.Name
                           & " threatens " & System.Name
                           & " with" & Natural'Image (N.Ships)
                           & " ships");
                        Required := Required + N.Ships;
                     end if;
                  end loop;

                  if Required > 0 then
                     AI.Defense_Destinations.Append (System);
                     AI.Faction.Change_Required (System, Required);

                     Concorde.Factions.Logging.Log
                       (AI.Faction,
                        System.Name & ": threat level"
                        & Required'Img);
                  end if;
               end;
            elsif AI.Faction.Is_Frontier (System) then
               AI.Frontier_Systems := AI.Frontier_Systems + 1;
               AI.Faction.Set_Required (System, -System.Ships);
            elsif AI.Faction.Is_Internal (System) then
               AI.Internal_Systems := AI.Internal_Systems + 1;
               AI.Faction.Set_Required (System, -System.Ships);
            else
               AI.Faction.Set_Required (System, -System.Ships);
            end if;
         end;
      end loop;

      Concorde.Factions.Logging.Log
        (AI.Faction,
         "border systems:" & AI.Border_Systems'Img
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

      for I in 1 .. AI.Defense_Destinations.Last_Index loop
         declare
            Info : Concorde.Systems.Star_System_Type renames
                     AI.Defense_Destinations (I);
         begin
            AI.Faction.Set_Required
              (Info,
               AI.Faction.Required (Info)
               * AI.Defense_Ships / AI.Nominal_Defense_Ships);
            Concorde.Factions.Logging.Log
              (AI.Faction,
               Info.Name
               & " allocated"
               & Integer'Image (AI.Faction.Required (Info))
               & " ships for defence; currently has"
               & Natural'Image (Info.Ships));
            AI.Faction.Change_Required (Info, -Info.Ships);
         end;
      end loop;

      AI.Exploration_Ships :=
        Natural'Min
          (Total_Ships - AI.Defense_Ships,
           AI.Unexplored_Systems);
      AI.Offense_Ships :=
        Total_Ships - AI.Defense_Ships - AI.Exploration_Ships;

      Concorde.Factions.Logging.Log
        (AI.Faction,
         "total ships:" & Total_Ships'Img
         & "; enemy strength:" & AI.Enemy_Strength'Img
         & "; defense ships:" & AI.Defense_Ships'Img
         & "; exploration:" & AI.Exploration_Ships'Img
         & "; offensive ships:" & AI.Offense_Ships'Img);

      Shuffle (AI.Defense_Destinations);
      Shuffle (AI.Explore_Destinations);
      Shuffle (AI.Attack_Destinations);

      if AI.Planned_Offensive then
         if AI.Faction.Owned_System (AI.Target)
           or else not AI.Faction.Owned_System (AI.Attack_From)
           or else (AI.Attack_From /= AI.Faction.Capital
                    and then AI.Faction.Next_Path_Node_Index
                      (AI.Faction.Capital, AI.Attack_From) = 0)
           or else AI.Target.Ships > AI.Offense_Ships
         then
            Concorde.Factions.Logging.Log
              (AI.Faction,
               "stop " & (if AI.Launch_Offensive then "active" else "planned")
               & " offensive against "
               & AI.Target.Owner.Name
               & " at "
               & AI.Attack_From.Name
               & " because "
               & (if AI.Faction.Owned_System (AI.Target)
                 then "we own the system"
                 elsif not AI.Faction.Owned_System (AI.Attack_From)
                 then "we have lost control of launch site "
                 & AI.Attack_From.Name
                 elsif AI.Attack_From /= AI.Faction.Capital
                 and then AI.Faction.Next_Path_Node_Index
                   (AI.Faction.Capital, AI.Attack_From) = 0
                 then "we are not connected to the launch site "
                 & AI.Attack_From.Name
                 elsif AI.Target.Ships > AI.Offense_Ships
                 then "we need at least" & Natural'Image (AI.Target.Ships)
                 & " but have only" & Natural'Image (AI.Offense_Ships)
                 else raise Constraint_Error with
                   "unknown reason for canceling offensive"));
            AI.Planned_Offensive := False;
            AI.Launch_Offensive := False;
            AI.Faction.Set_Attack_Target (AI.Target, False);
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
                  D : constant Natural :=
                        AI.Faction.Path_Length
                          (AI.Faction.Capital, Info);
                  Opp : constant Natural :=
                          Info.Ships;
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
               if AI.Faction.Path_Length (AI.Faction.Capital, Easiest_System)
                 > Shortest_Distance + 3
               then
                  AI.Target := Closest_System;
               else
                  AI.Target := Easiest_System;
               end if;

               Found := False;
               Closest_System := null;
               Shortest_Distance := Natural'Last;

               if Galaxy.Neighbours (AI.Target, AI.Faction.Capital) then
                  Closest_System := AI.Faction.Capital;
                  Found := True;
               else
                  for N of Galaxy.Neighbours (AI.Target) loop
                     if AI.Faction.Owned_System (N) then
                        declare
                           D    : constant Natural :=
                                    AI.Faction.Path_Length
                                      (AI.Faction.Capital, N);
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
                           if not AI.Faction.Owned_System (M) then
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
            Concorde.Factions.Logging.Log
              (AI.Faction,
               "planning attack on " & AI.Target.Name
               & " owned by " & AI.Target.Owner.Name
               & " from " & AI.Attack_From.Name
               & " (distance"
               & Natural'Image
                 (AI.Faction.Path_Length (AI.Attack_From, AI.Target))
               & ")");

            AI.Faction.Set_Attack_Target (AI.Target, True);
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
                 + AI.Faction.Required (AI.Attack_From) + 1,
                 5);

            if Committed_Ships >= AI.Required_Strength then
               AI.Launch_Offensive := True;
               Concorde.Factions.Logging.Log
                 (AI.Faction,
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
                     Ship.Set_Destination (AI.Target);
                  end if;
               end loop;
            else
               Concorde.Factions.Logging.Log
                 (AI.Faction,
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

   overriding function Awake (AI : Root_Test_AI_Type) return Boolean is
   begin
      return AI.Awake;
   end Awake;

   ---------------------------
   -- Minimum_Attack_Factor --
   ---------------------------

   overriding function Minimum_Attack_Factor
     (AI : Root_Test_AI_Type)
      return Non_Negative_Real
   is
   begin
      return AI.Current_Attack_Factor;
   end Minimum_Attack_Factor;

   ----------------
   -- Order_Ship --
   ----------------

   overriding procedure Order_Ship
     (AI : in out Root_Test_AI_Type;
      Ship : not null access Concorde.Ships.Root_Vessel_Type'Class)
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
                        AI.Faction.Path_Length
                          (Ship.System, Dest);
            begin
               if AI.Faction.Required (Dest) > 0
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
                  AI.Faction.Change_Required (Dest, -1);

                  Factions.Logging.Log
                    (AI.Faction,
                     "ordering " & Ship.Name & " to "
                     & Dest.Name
                     & " at distance"
                     & Minimum_Hops'Img);
               end;
            else
               Factions.Logging.Log
                 (AI.Faction,
                  "ordering " & Ship.Name & " to remain at "
                  & Ship.System.Name);
            end if;
         end if;

      end Choose_Destination;

      Stop : Boolean := False;

      Opportunity_Attack : Boolean := False;

   begin

      Factions.Logging.Log
        (AI.Faction, Ship.Short_Description);

      for Opportunity of AI.Opportunity_Destinations loop

         if Galaxy.Neighbours (Ship.System, Opportunity)
           and then not AI.Faction.Is_Attack_Target (Opportunity)
           and then not Ship.Has_Destination
         then
            AI.Faction.Set_Attack_Target (Opportunity, True);
            Ship.Set_Destination (Opportunity);
            Opportunity_Attack := True;
            Factions.Logging.Log
              (AI.Faction,
               Ship.Short_Description & " sees attack opportunity at "
               & Opportunity.Name);
            exit;
         end if;
      end loop;

      if Ship.Has_Destination and then not Opportunity_Attack
        and then (Ship.System = Ship.Destination
                  or else AI.Faction.Next_Path_Node_Index
                    (Ship.System, Ship.Destination) = 0
                  or else (not AI.Faction.Owned_System (Ship.Destination)
                           and then Ship.Destination /= AI.Target
                           and then Ship.Destination.Owned))
      then
         Ship.Set_Destination (null);
      end if;

      if Ship.Has_Destination then
         null;
      elsif AI.Planned_Offensive
        and then Ship.System = AI.Attack_From
      then
         if AI.Launch_Offensive then
            Factions.Logging.Log
              (AI.Faction,
               Ship.Name & " delays offensive because "
               & " no attack order was given");
         else
            Factions.Logging.Log
              (AI.Faction,
               Ship.Name & " waits at "
               & Ship.System.Name
               & " for attack order");
         end if;
         Ship.Set_Destination (null);
      elsif AI.Faction.Required (Ship.System) > 0 then
         AI.Faction.Change_Required (Ship.System, -1);
         Ship.Set_Destination (null);
         Stop := True;
         Factions.Logging.Log
           (AI.Faction,
            Ship.Name & " required to remain at "
            & Ship.System.Name);
      else

         if AI.Launch_Offensive
           and then Ship.System = AI.Attack_From
           and then Ship.Damage < Max_Battle_Damage
         then
            Factions.Logging.Log
              (AI.Faction,
               Ship.Name & " joins attack on "
               & AI.Target.Name);
            Ship.Set_Destination (AI.Target);
            Stop := True;
         end if;

         if not Stop and then Ship.Damage < Max_Defence_Damage then
            Choose_Destination (AI.Defense_Destinations, Stop);
         end if;

         if not Stop and then Ship.Damage < Max_Explore_Damage then
            Choose_Destination (AI.Explore_Destinations, Stop);
         end if;

         if not Stop and then AI.Planned_Offensive then
            Factions.Logging.Log
              (AI.Faction,
               "checking planned offensive: required ="
               & AI.Required_Strength'Img
               & "; available ="
               & AI.Available_Strength'Img
               & "; connected: "
               & (if Ship.System = AI.Attack_From
                 then "yes"
                 elsif AI.Faction.Next_Path_Node_Index
                   (Ship.System, AI.Attack_From) /= 0
                 then "yes" else "no"));

            if AI.Required_Strength > AI.Available_Strength
              and then (Ship.System = AI.Attack_From
                        or else AI.Faction.Next_Path_Node_Index
                          (Ship.System, AI.Attack_From) /= 0)
            then
               Stop := True;
               if Ship.System = AI.Attack_From then
                  null;
               else
                  Factions.Logging.Log
                    (AI.Faction,
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
           and then Ship.System /= AI.Faction.Capital
         then
            Stop := True;
            Ship.Set_Destination (AI.Faction.Capital);
            Factions.Logging.Log
              (AI.Faction,
               Ship.Name & " returns to capital " & AI.Faction.Capital.Name);
         end if;

         if Stop then
            if AI.Faction.Required (Ship.System) < 0 then
               AI.Faction.Change_Required (Ship.System, 1);
            end if;
         elsif Ship.Damage > 0.0 then
            Factions.Logging.Log
              (AI.Faction,
               Ship.Name & " making repairs at " & Ship.System.Name);
         else
            Factions.Logging.Log
              (AI.Faction,
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
     (AI     : in out Root_Test_AI_Type;
      Faction : Concorde.Factions.Faction_Type)
   is
      pragma Unreferenced (AI);

      function Owner
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is (System.Owner /= null
          and then System.Owner.Name = Faction.Name);

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
            Faction.Add_Focus (N);
         end loop;
      end Start_System;

   begin
      AI.Faction := Faction;
      Concorde.Galaxy.Iterate (Owner'Access, Start_System'Access);
   end Start;

   ---------------------
   -- System_Acquired --
   ---------------------

   overriding procedure System_Acquired
     (AI           : in out Root_Test_AI_Type;
      System       : Concorde.Systems.Star_System_Type;
      Former_Owner : Concorde.Factions.Faction_Type)
   is
      use Concorde.Factions;
   begin
      if Former_Owner /= null then
         AI.Update_Attack_Factor (Former_Owner);
      end if;

      if AI.Planned_Offensive
        and then AI.Target.Index = System.Index
      then
         AI.Planned_Offensive := False;
         AI.Launch_Offensive := False;
      end if;

      AI.Faction.Set_Attack_Target (System, False);
      AI.Awake := True;

   end System_Acquired;

   -----------------
   -- System_Lost --
   -----------------

   overriding procedure System_Lost
     (AI        : in out Root_Test_AI_Type;
      System    : Concorde.Systems.Star_System_Type;
      New_Owner : Concorde.Factions.Faction_Type)
   is
   begin
      AI.Update_Attack_Factor (New_Owner, Can_Decrease => False);

      if AI.Planned_Offensive
        and then AI.Attack_From.Index = System.Index
      then
         AI.Planned_Offensive := False;
         AI.Launch_Offensive := False;
         AI.Faction.Set_Attack_Target (System, False);
      end if;

      AI.Awake := True;

   end System_Lost;

   -------------
   -- Test_AI --
   -------------

   function Test_AI return AI_Type is
   begin
      return new Root_Test_AI_Type;
   end Test_AI;

   ------------------
   -- Update_Focus --
   ------------------

   overriding procedure Update_Focus
     (AI : in out Root_Test_AI_Type)
   is
      function Other_Owner
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is (System.Owner /= null and then System.Owner /= AI.Faction);

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
         AI.Faction.Add_Focus (System);
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
         if System.Owner /= AI.Faction then
            return False;
         end if;

         for N of Ns loop
            if N.Owner /= null and then N.Owner /= AI.Faction then
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
         if System.Owner /= AI.Faction then
            return False;
         end if;

         for N of Ns loop
            if N.Owner /= null and then N.Owner /= AI.Faction then
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
         if System.Owner = null or else System.Owner = AI.Faction then
            return False;
         end if;

         Defenders := System.Ships;
         for N of Ns loop
            if N.Owner = AI.Faction then
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
                         Root_Test_AI_Type'Class (AI).Minimum_Attack_Factor;
            begin
               if Ratio > Min then
--                    Concorde.Factions.Logging.Log
--                      (AI.Faction,
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
            if N.Owner = AI.Faction then
               return True;
            end if;
         end loop;

         return False;

      end Unexplored;

   begin
      AI.Faction.Remove_Focus (Other_Owner'Access);
      AI.Faction.Remove_Focus (Interior'Access);

      Galaxy.Iterate (Border_System'Access, Add_Focus'Access);
      Galaxy.Iterate (Outnumbered_Defenders'Access, Add_Focus'Access);
      Galaxy.Iterate (Unexplored'Access, Add_Focus'Access);
   end Update_Focus;

   ----------
   -- Wake --
   ----------

   overriding procedure Wake (AI : in out Root_Test_AI_Type) is
   begin
      AI.Awake := True;
   end Wake;

end Concorde.AI.Test;
