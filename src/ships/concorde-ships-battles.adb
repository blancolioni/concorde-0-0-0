with Ada.Numerics;
with Ada.Strings.Unbounded;

with Concorde.Empires;
with Concorde.Empires.Logging;
with Concorde.Empires.Relations;

with Concorde.Empires.Db;
with Concorde.Modules.Db;
with Concorde.Systems.Db;

package body Concorde.Ships.Battles is

   Arena_Radius    : constant := 1.0E6;
   Planet_Radius   : constant := 1.0E3;
   Ship_Separation : constant := 100.0;

   type Ship_Team is
      record
         Empire : access constant Concorde.Empires.Root_Empire_Type'Class;
         X, Y   : Real;
         Facing : Radians;
         Count  : Natural;
      end record;

   package Team_Vectors is
     new Ada.Containers.Vectors (Positive, Ship_Team);

   function Create_Arena
     (System : in out Concorde.Systems.Root_Star_System_Type'Class;
      Ships  : Concorde.Ships.Lists.List)
     return Concorde.Combat.Ship_Combat.Space_Combat_Arena
   is
      use Concorde.Empires;
      Teams         : Team_Vectors.Vector;
      Min_Team_Size : Natural := Natural'Last;

      Arena         : constant Combat.Ship_Combat.Space_Combat_Arena :=
                        Concorde.Combat.Ship_Combat.New_Arena
                          (Name          => System.Name,
                           Radius        => Arena_Radius,
                           Planet_X      => 0.0,
                           Planet_Y      => 0.0,
                           Planet_Radius => Planet_Radius);
   begin

      for Ship of Ships loop
         declare
            Pi : constant := Ada.Numerics.Pi;
            Owner      : constant Empire_Type := Empire_Type (Ship.Owner);
            New_Team   : Boolean := True;
            Team_Index : Positive;
            Team       : Ship_Team;
         begin
            for I in 1 .. Teams.Last_Index loop
               if Teams (I).Empire = Owner then
                  Team := Teams (I);
                  Team_Index := I;
                  New_Team := False;
               end if;
            end loop;

            if New_Team then
               Team :=
                 (Empire => Ship.Owner,
                  Count  => 0,
                  X      =>
                    (if Teams.Last_Index = 0 then -1000.0 else 1000.0),
                  Y      => 0.0,
                  Facing =>
                    (if Teams.Last_Index = 0 then 0.0 else Pi));
               Teams.Append (Team);
               Team_Index := Teams.Last_Index;
            end if;

            for Mount of Ship.Structure loop

               Concorde.Modules.Db.Update
                 (Mount.Module.Reference,
                  Concorde.Modules.Initial_State'Access);

            end loop;

            Arena.Add_Combatant
              (Combatant => Ship,
               Empire    => Owner,
               X         => Team.X,
               Y         => Team.Y + Ship_Separation * Real (Team.Count),
               Facing    => Team.Facing);

            Teams (Team_Index).Count := Teams (Team_Index).Count + 1;
         end;
      end loop;

      pragma Assert (Teams.Last_Index >= 2);

      for Team of Teams loop
         declare
            Size : constant Positive := Team.Count;

            procedure Set_Battle
              (Empire : in out Concorde.Empires.Root_Empire_Type'Class);

            ----------------
            -- Set_Battle --
            ----------------

            procedure Set_Battle
              (Empire : in out Concorde.Empires.Root_Empire_Type'Class)
            is
            begin
               Empire.Set_Battle (Concorde.Systems.Db.Reference (System));
            end Set_Battle;

         begin
            Concorde.Empires.Db.Update
              (Team.Empire.Reference, Set_Battle'Access);

            if Size < Min_Team_Size then
               Min_Team_Size := Size;
            end if;
         end;
      end loop;

      System.Battle (Min_Team_Size);

      declare
         use Ada.Strings.Unbounded;
         Ts : Unbounded_String;
      begin
         for Team of Teams loop
            Ts := Ts & " " & Team.Empire.Name;
         end loop;

         for Team of Teams loop
            if Min_Team_Size > 5 then
               Concorde.Empires.Logging.Log
                 (Empire  => Team.Empire,
                  Message =>
                    "The Battle of " & System.Name
                  & (if System.Owned
                    then ", owned by " & System.Owner.Name
                    else ", disputed system")
                  & " involving" & To_String (Ts));
            else
               Concorde.Empires.Logging.Log
                 (Empire  => Team.Empire,
                  Message => "Skirmish at " & System.Name & " involving"
                  & To_String (Ts));
            end if;
         end loop;
      end;

      for Team of Teams loop
         for Log_Team of Teams loop
            Concorde.Empires.Logging.Log
              (Log_Team.Empire,
               Team.Empire.Name
               & " fleet size:"
               & Natural'Image (Team.Count));
         end loop;
      end loop;

      return Arena;

   end Create_Arena;

   -----------------------
   -- Empire_Ship_Count --
   -----------------------

   function Empire_Ship_Count
     (Empire : Concorde.Empires.Empire_Type;
      Ships  : Concorde.Ships.Lists.List)
      return Natural
   is
      Count : Natural := 0;
   begin
      for Ship of Ships loop
         if Ship.Owner = Empire then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Empire_Ship_Count;

   ---------------------
   -- Empires_Present --
   ---------------------

   function Empires_Present
     (Ships : Concorde.Ships.Lists.List)
      return Concorde.Empires.Array_Of_Empires
   is
      Result : Concorde.Empires.Array_Of_Empires (1 .. Natural (Ships.Length));
      Count  : Natural := 0;
   begin
      for Ship of Ships loop
         declare
            use Concorde.Empires;
            Found : Boolean := False;
         begin
            for I in 1 .. Count loop
               if Empire_Type (Ship.Owner) = Result (I) then
                  Found := True;
               end if;
            end loop;
            if not Found then
               Count := Count + 1;
               Result (Count) := Empire_Type (Ship.Owner);
            end if;
         end;
      end loop;
      return Result (1 .. Count);
   end Empires_Present;

   ------------------
   -- Has_Conflict --
   ------------------

   function Has_Conflict
     (Ships : Concorde.Ships.Lists.List)
      return Boolean
   is
      use Concorde.Empires;
      Es : constant Array_Of_Empires :=
             Empires_Present (Ships);
   begin
      return Concorde.Empires.Relations.Has_Conflict (Es);
   end Has_Conflict;

   ---------
   -- Hit --
   ---------

--     overriding procedure Hit
--       (Combatant : in out Fighting_Ship;
--        Damage    : Positive)
--     is
--        Ship : Ship_Type renames Combatant.Ship;
--     begin
--        for I in 1 .. Damage loop
--           declare
--              Module_Index : constant Positive :=
--                               WL.Random.Random_Number
--                                 (1, Ship.Structure.Last_Index);
--              Module       : Concorde.Modules.Module_Type renames
--                               Ship.Structure (Module_Index);
--           begin
--              Module.Hit;
--           end;
--        end loop;
--     end Hit;

end Concorde.Ships.Battles;
