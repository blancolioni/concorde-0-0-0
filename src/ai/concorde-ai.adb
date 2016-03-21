with WL.Random;

with Concorde.Empires.Logging;
with Concorde.Galaxy;

package body Concorde.AI is

   procedure Shuffle (V : in out Destination_Vectors.Vector);

   --------------------
   -- Allocate_Ships --
   --------------------

   procedure Allocate_Ships
     (AI : in out Root_AI_Type)
   is
      Total_Systems      : constant Natural := AI.Empire.Current_Systems;
      Total_Ships        : constant Natural := AI.Empire.Current_Ships;
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

      for I in 1 .. Concorde.Galaxy.System_Count loop
         declare
            System : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Galaxy.Get_System (I);
         begin
            AI.Empire.Set_Required (System, 0);

            if AI.Empire.Is_Neighbour (System) then
               if System.Owned then
                  AI.Threat_Systems := AI.Threat_Systems + 1;
                  declare
                     Enemy_Ships  : constant Natural :=
                                      System.Owner.Current_Ships;
                     Threat_Ships : Natural := System.Ships;
                  begin
                     if Enemy_Ships < AI.Empire.Current_Ships then
                        Threat_Ships := Threat_Ships
                          * Enemy_Ships / AI.Empire.Current_Ships;
                     end if;
                     AI.Enemy_Strength := AI.Enemy_Strength + Threat_Ships;
                  end;
                  AI.Attack_Destinations.Append
                    ((System => System));
               else
                  AI.Unexplored_Systems := AI.Unexplored_Systems + 1;
                  AI.Explore_Destinations.Append
                    ((System => System));
                  AI.Empire.Change_Required (System, 1);
               end if;
            elsif AI.Empire.Is_Border (System) then
               AI.Border_Systems := AI.Border_Systems + 1;
               declare
                  Required : Natural := 1;
               begin
                  for N of Galaxy.Neighbours (System.Index) loop
                     if N.Owned and then not AI.Empire.Owned_System (N) then
                        Concorde.Empires.Logging.Log
                          (AI.Empire,
                           N.Name & " owned by " & N.Owner.Name
                           & " threatens " & System.Name
                           & " with" & Natural'Image (N.Ships)
                           & " ships");
                        Required := Required + N.Ships;
                     end if;
                  end loop;

                  if Required > 0 then
                     AI.Defense_Destinations.Append
                       ((System => System));
                     AI.Empire.Change_Required (System, Required);

                     Concorde.Empires.Logging.Log
                       (AI.Empire,
                        System.Name & ": threat level"
                        & Required'Img);
                  end if;
               end;
            elsif AI.Empire.Is_Frontier (System) then
               AI.Frontier_Systems := AI.Frontier_Systems + 1;
            elsif AI.Empire.Is_Internal (System) then
               AI.Internal_Systems := AI.Internal_Systems + 1;
            end if;
         end;
      end loop;

      Concorde.Empires.Logging.Log
        (AI.Empire,
         "border systems:" & AI.Border_Systems'Img
         & "; frontier systems:" & AI.Frontier_Systems'Img
         & "; threat systems:" & AI.Threat_Systems'Img
         & "; unexplored systems:" & AI.Unexplored_Systems'Img
         & "; internal systems:" & AI.Internal_Systems'Img);

      AI.Nominal_Defense_Ships := AI.Enemy_Strength;
      AI.Defense_Ships :=
        Natural'Min (AI.Enemy_Strength * 3 / 4, Total_Ships);

      if AI.Nominal_Defense_Ships = 0 then
         AI.Defense_Destinations.Clear;
      end if;

      for I in 1 .. AI.Defense_Destinations.Last_Index loop
         declare
            Info : Destination_Info renames AI.Defense_Destinations (I);
         begin
            AI.Empire.Set_Required
              (Info.System,
               AI.Empire.Required (Info.System)
               * AI.Defense_Ships / AI.Nominal_Defense_Ships);
            Concorde.Empires.Logging.Log
              (AI.Empire,
               Info.System.Name
               & " allocated"
               & Integer'Image (AI.Empire.Required (Info.System))
               & " ships for defence; currently has"
               & Natural'Image (Info.System.Ships));
            AI.Empire.Change_Required (Info.System, -Info.System.Ships);
         end;
      end loop;

      AI.Exploration_Ships :=
        Natural'Min
          (Total_Ships - AI.Defense_Ships,
           AI.Unexplored_Systems);
      AI.Offense_Ships :=
        Total_Ships - AI.Defense_Ships - AI.Exploration_Ships;

      Concorde.Empires.Logging.Log
        (AI.Empire,
         "total ships:" & Total_Ships'Img
         & "; enemy strength:" & AI.Enemy_Strength'Img
         & "; defense ships:" & AI.Defense_Ships'Img
         & "; exploration:" & AI.Exploration_Ships'Img
         & "; offensive ships:" & AI.Offense_Ships'Img);

      Shuffle (AI.Defense_Destinations);
      Shuffle (AI.Explore_Destinations);
      Shuffle (AI.Attack_Destinations);

      if AI.Planned_Offensive then
         if AI.Empire.Owned_System (AI.Target)
           or else not AI.Empire.Owned_System (AI.Attack_From)
           or else AI.Empire.Next_Path_Node_Index
             (AI.Empire.Capital, AI.Attack_From) = 0
         then
            AI.Planned_Offensive := False;
            AI.Launch_Offensive := False;
            AI.Empire.Set_Attack_Target (AI.Target, False);
         end if;
      end if;

      if not AI.Planned_Offensive
        and then not AI.Attack_Destinations.Is_Empty
      then

         declare
            Found             : Boolean := False;
            Closest_System    : Concorde.Systems.Star_System_Type := null;
            Shortest_Distance : Natural := Natural'Last;
         begin
            for Info of AI.Attack_Destinations loop
               declare
                  D : constant Natural :=
                        AI.Empire.Path_Length
                          (AI.Empire.Capital, Info.System);
               begin
                  if D < Shortest_Distance then
                     Shortest_Distance := D;
                     Closest_System := Info.System;
                     Found := True;
                  end if;
               end;
            end loop;

            if Found then
               AI.Target := Closest_System;

               Found := False;
               Closest_System := null;
               Shortest_Distance := Natural'Last;

               if Galaxy.Neighbours (AI.Target, AI.Empire.Capital) then
                  Closest_System := AI.Empire.Capital;
                  Found := True;
               else
                  for N of Galaxy.Neighbours (AI.Target) loop
                     declare
                        D    : constant Natural :=
                                 AI.Empire.Path_Length
                                   (AI.Empire.Capital, N);
                     begin
                        if D < Shortest_Distance then
                           Shortest_Distance := D;
                           Closest_System := N;
                           Found := True;
                        end if;
                     end;
                  end loop;
               end if;

               if Found then
                  AI.Attack_From := Closest_System;
                  AI.Planned_Offensive := True;
                  AI.Launch_Offensive := False;
               end if;
            end if;
         end;

         if AI.Planned_Offensive then
            Concorde.Empires.Logging.Log
              (AI.Empire,
               "planning attack on " & AI.Target.Name
               & " owned by " & AI.Target.Owner.Name
               & " from " & AI.Attack_From.Name);
            AI.Empire.Set_Attack_Target (AI.Target, True);
         end if;
      end if;

      if AI.Planned_Offensive then
         declare
            function Local_Undamaged
              (Ship : Concorde.Ships.Ship_Type)
               return Boolean
            is (Ship.System = AI.Attack_From
                and then Ship.Damage < 0.1);

            Available_Ships : constant Natural :=
                                Concorde.Ships.Count_Ships
                                  (Local_Undamaged'Access);
            Committed_Ships : constant Integer :=
                                Available_Ships
                                  - AI.Empire.Required (AI.Attack_From);
            Opposition      : constant Natural := AI.Target.Ships;
         begin
            if Real (Committed_Ships) / Real (Opposition)
              > AI.Defensiveness
            then
               AI.Launch_Offensive := True;
               Concorde.Empires.Logging.Log
                 (AI.Empire,
                  "launching attack from "
                  & AI.Attack_From.Name
                  & " with"
                  & Natural'Image (Committed_Ships)
                  & " on " & AI.Target.Name
                  & " with"
                  & Natural'Image (Opposition)
                  & " owned by " & AI.Target.Owner.Name);
            end if;
         end;
      end if;

      AI.Awake := False;

   end Allocate_Ships;

   -----------
   -- Awake --
   -----------

   function Awake (AI : Root_AI_Type) return Boolean is
   begin
      return AI.Awake;
   end Awake;

   ---------------------------
   -- Minimum_Attack_Factor --
   ---------------------------

   function Minimum_Attack_Factor
     (AI : Root_AI_Type)
      return Non_Negative_Real
   is
   begin
      return AI.Defensiveness;
   end Minimum_Attack_Factor;

   ----------------
   -- Order_Ship --
   ----------------

   procedure Order_Ship
     (AI : in out Root_AI_Type;
      Ship : not null access Concorde.Ships.Root_Ship_Type'Class)
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
               Dest : Destination_Info renames
                        V.Element (I);
               Hops : constant Natural :=
                        AI.Empire.Path_Length
                          (Ship.System, Dest.System);
            begin
               if AI.Empire.Required (Dest.System) > 0
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
                  Dest : Destination_Info renames V (Closest_Index);
               begin
                  Ship.Set_Destination (Dest.System);
                  AI.Empire.Change_Required (Dest.System, -1);

                  Empires.Logging.Log
                    (AI.Empire,
                     "ordering " & Ship.Name & " to "
                     & Dest.System.Name
                     & " at distance"
                     & Minimum_Hops'Img);
               end;
            else
               Empires.Logging.Log
                 (AI.Empire,
                  "ordering " & Ship.Name & " to remain at "
                  & Ship.System.Name);
            end if;
         end if;

      end Choose_Destination;

      Stop : Boolean := False;

   begin

      Ship.Set_Destination (null);

      if AI.Empire.Required (Ship.System) > 0 then
         AI.Empire.Change_Required (Ship.System, -1);
         Stop := True;
         Empires.Logging.Log
           (AI.Empire,
            Ship.Name & " required to remain at "
            & Ship.System.Name);
      else

         if AI.Launch_Offensive
           and then Ship.System = AI.Attack_From
           and then Ship.Damage < 0.4
         then
            Empires.Logging.Log
              (AI.Empire,
               Ship.Name & " joins attack on "
               & AI.Target.Name);
            Ship.Set_Destination (AI.Target);
            Stop := True;
         end if;

         if not Stop and then Ship.Damage < 0.2 then
            Choose_Destination (AI.Defense_Destinations, Stop);
         end if;

         if not Stop and then Ship.Damage < 0.6 then
            Choose_Destination (AI.Explore_Destinations, Stop);
         end if;

         if not Stop
           and then AI.Planned_Offensive
           and then AI.Empire.Next_Path_Node_Index
             (Ship.System, AI.Attack_From) /= 0
         then
            Stop := True;
            if Ship.System = AI.Attack_From then
               null;
            else
               Empires.Logging.Log
                 (AI.Empire,
                  Ship.Name & " heads to "
                  & AI.Attack_From.Name
                  & " for offensive against "
                  & AI.Target.Name);
               Ship.Set_Destination (AI.Attack_From);
            end if;
         end if;

         if Stop then
            if AI.Empire.Required (Ship.System) < 0 then
               AI.Empire.Change_Required (Ship.System, 1);
            end if;
         elsif Ship.Damage > 0.0 then
            Empires.Logging.Log
              (AI.Empire,
               Ship.Name & " making repairs at " & Ship.System.Name);
         else
            Empires.Logging.Log
              (AI.Empire,
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
                  T : constant Destination_Info := V.Element (From_Index);
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

   procedure Start
     (AI     : in out Root_AI_Type;
      Empire : Concorde.Empires.Empire_Type)
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
      AI.Empire := Empire;
      Concorde.Galaxy.Iterate (Owner'Access, Start_System'Access);
   end Start;

   ---------------------
   -- System_Acquired --
   ---------------------

   procedure System_Acquired
     (AI           : in out Root_AI_Type;
      System       : Concorde.Systems.Star_System_Type;
      Former_Owner : Concorde.Empires.Empire_Type)
   is
      use Concorde.Empires;
   begin
--        if AI.Empire.Has_Focus (System) then
--           declare
--              Stop : Boolean := True;
--              Ns   : constant Concorde.Galaxy.Array_Of_Star_Systems :=
--                       Concorde.Galaxy.Neighbours (System);
--           begin
--              for N of Ns loop
--                 if N.Owner = null then
--                    Stop := False;
--                    AI.Empire.Add_Focus (N);
--                 end if;
--              end loop;
--              if not Stop then
--                 AI.Empire.Remove_Focus (System);
--              end if;
--           end;
--        end if;

      if Former_Owner /= null then
         Update_Defensiveness (AI, Former_Owner);
      end if;

      if AI.Planned_Offensive
        and then AI.Target.Index = System.Index
      then
         AI.Planned_Offensive := False;
         AI.Launch_Offensive := False;
         AI.Empire.Set_Attack_Target (System, False);
      end if;

      AI.Awake := True;

   end System_Acquired;

   -----------------
   -- System_Lost --
   -----------------

   procedure System_Lost
     (AI        : in out Root_AI_Type;
      System    : Concorde.Systems.Star_System_Type;
      New_Owner : Concorde.Empires.Empire_Type)
   is
   begin
      Update_Defensiveness (AI, New_Owner, Can_Decrease => False);

      if AI.Planned_Offensive
        and then AI.Attack_From.Index = System.Index
      then
         AI.Planned_Offensive := False;
         AI.Launch_Offensive := False;
         AI.Empire.Set_Attack_Target (System, False);
      end if;

      AI.Awake := True;

   end System_Lost;

   --------------------------
   -- Update_Defensiveness --
   --------------------------

   procedure Update_Defensiveness
     (AI           : in out Root_AI_Type'Class;
      Enemy        : Concorde.Empires.Empire_Type;
      Can_Increase : Boolean := True;
      Can_Decrease : Boolean := True)
   is
      D : Non_Negative_Real;
   begin
      if AI.Empire.Current_Ships > Enemy.Current_Ships then
         D := 1.2;
      elsif AI.Empire.Current_Ships = 0
        or else AI.Empire.Current_Ships * 4 < Enemy.Current_Ships
      then
         D := 4.0;
      else
         D :=
           Real (Enemy.Current_Ships) * 1.3
           / Real (AI.Empire.Current_Ships);
      end if;

      if (D < AI.Defensiveness and then Can_Decrease)
        or else (D > AI.Defensiveness and then Can_Increase)
      then
         AI.Defensiveness := (AI.Defensiveness + D) / 2.0;
      end if;

   end Update_Defensiveness;

   ------------------
   -- Update_Focus --
   ------------------

   procedure Update_Focus
     (AI : in out Root_AI_Type)
   is
      function Other_Owner
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is (System.Owner /= null and then System.Owner /= AI.Empire);

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
         AI.Empire.Add_Focus (System);
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
         if System.Owner /= AI.Empire then
            return False;
         end if;

         for N of Ns loop
            if N.Owner /= null and then N.Owner /= AI.Empire then
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
         if System.Owner /= AI.Empire then
            return False;
         end if;

         for N of Ns loop
            if N.Owner /= null and then N.Owner /= AI.Empire then
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
         if System.Owner = null or else System.Owner = AI.Empire then
            return False;
         end if;

         Defenders := System.Ships;
         for N of Ns loop
            if N.Owner = AI.Empire then
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
                         Root_AI_Type'Class (AI).Minimum_Attack_Factor;
            begin
               if Ratio > Min then
--                    Concorde.Empires.Logging.Log
--                      (AI.Empire,
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
            if N.Owner = AI.Empire then
               return True;
            end if;
         end loop;

         return False;

      end Unexplored;

   begin
      AI.Empire.Remove_Focus (Other_Owner'Access);
      AI.Empire.Remove_Focus (Interior'Access);

      Galaxy.Iterate (Border_System'Access, Add_Focus'Access);
      Galaxy.Iterate (Outnumbered_Defenders'Access, Add_Focus'Access);
      Galaxy.Iterate (Unexplored'Access, Add_Focus'Access);
   end Update_Focus;

   ----------
   -- Wake --
   ----------

   procedure Wake (AI : in out Root_AI_Type) is
   begin
      AI.Awake := True;
   end Wake;

end Concorde.AI;
