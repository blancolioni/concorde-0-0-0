with WL.Random;

with Concorde.Empires;
with Concorde.Empires.Logging;

with Concorde.AI;

package body Concorde.Ships.Battles is

   type Team_Info is
      record
         Leader : Concorde.Empires.Empire_Type;
         Ships  : Concorde.Ships.Lists.List;
      end record;

   package Team_Vectors is
     new Ada.Containers.Vectors (Positive, Team_Info);

   type Ship_Info is
      record
         Team   : Positive;
         Ship   : Ship_Type;
         Target : Ship_Type;
      end record;

   package Ship_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Ship_Info);

   procedure Attack
     (Attacker, Defender : Ship_Type);

   ------------
   -- Attack --
   ------------

   procedure Attack
     (Attacker, Defender : Ship_Type)
   is
      pragma Unreferenced (Attacker);
   begin
      if WL.Random.Random_Number (1, 2) = 2 then
         Defender.HP := Defender.HP - 1;
      end if;
   end Attack;

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

   -----------
   -- Fight --
   -----------

   procedure Fight
     (System : Concorde.Systems.Star_System_Access;
      Ships  : Concorde.Ships.Lists.List)
   is
      use Concorde.Empires;
      Teams         : Team_Vectors.Vector;
      Info_Vector   : Ship_Info_Vectors.Vector;
      Min_Team_Size : Natural := Natural'Last;
   begin

      for Ship of Ships loop
         declare
            Owner      : constant Empire_Type := Empire_Type (Ship.Owner);
            New_Team   : Boolean := True;
            Team_Index : Positive;
         begin
            for I in 1 .. Teams.Last_Index loop
               if Teams (I).Leader = Owner then
                  Teams (I).Ships.Append (Ship);
                  Team_Index := I;
                  New_Team := False;
                  exit;
               end if;
            end loop;

            if New_Team then
               declare
                  List : Lists.List;
               begin
                  List.Append (Ship);
                  Teams.Append ((Owner, List));
                  Team_Index := Teams.Last_Index;
                  Owner.AI.Wake;
               end;
            end if;

            Info_Vector.Append ((Team_Index, Ship, null));
         end;
      end loop;

      pragma Assert (Teams.Last_Index >= 2);

      for Team of Teams loop
         declare
            Size : constant Positive := Positive (Team.Ships.Length);
         begin
            if Size < Min_Team_Size then
               Min_Team_Size := Size;
            end if;
         end;
      end loop;

      System.Battle (Min_Team_Size);

      if Min_Team_Size > 5 then
         Concorde.Empires.Logging.Log
           (Empire  => Teams.First_Element.Leader,
            Message =>
              "The Battle of " & System.Name
            & (if System.Owned
              then ", owned by " & System.Owner.Name
              else ", disputed system"));
      else
         Concorde.Empires.Logging.Log
           (Empire  => Teams.First_Element.Leader,
            Message => "Skirmish at " & System.Name);
      end if;

      for Team of Teams loop
         Concorde.Empires.Logging.Log
           (Team.Leader,
            "fleet size:"
            & Natural'Image (Natural (Team.Ships.Length)));
      end loop;

      for Round_Index in 1 .. 10 loop
         declare
            Continue : Boolean := False;
            Count    : constant Natural := Info_Vector.Last_Index;
            Order    : array (1 .. Count) of Positive;
         begin
            for I in Order'Range loop
               Order (I) := I;
            end loop;
            for I in Order'Range loop
               declare
                  New_Index : constant Positive :=
                                WL.Random.Random_Number (1, Count);
                  T         : constant Positive := Order (New_Index);
               begin
                  Order (New_Index) := Order (I);
                  Order (I) := T;
               end;
            end loop;

            for Ship_Info_Index of Order loop
               declare
                  Info : Ship_Info renames Info_Vector (Ship_Info_Index);
               begin
                  if not Info.Ship.Alive then
                     Info.Target := null;
                  elsif Info.Target = null
                    or else Info.Target.HP = 0
                  then
                     Info.Target := null;
                     for I in 1 .. Teams.Last_Index loop
                        if I /= Info.Team then
                           for Ship of Teams (I).Ships loop
                              if Ship.HP > 0 then
                                 Info.Target := Ship;
                                 exit;
                              end if;
                           end loop;
                        end if;
                     end loop;
                  end if;

                  if Info.Target /= null then
                     Continue := True;
                     Attack (Info.Ship, Info.Target);
                  end if;
               end;
            end loop;

            for Ship of Ships loop
               if Ship.Alive then
                  if Ship.HP = 0 then
                     Ship.Alive := False;
                     Ship.Owner.Remove_Ship;
                     System.Remove_Ship (Ship);
                     Empires.Logging.Log
                       (Ship.Owner, Ship.Name & " destroyed");
                  end if;
               end if;
            end loop;

            exit when not Continue;
         end;
      end loop;

      declare
         Remaining_Teams : Natural := 0;
         Victor          : Concorde.Empires.Empire_Type := null;
      begin
         for Team of Teams loop
            declare
               Team_Remaining : Natural := 0;
            begin
               for Ship of Team.Ships loop
                  if Ship.Alive then
                     Team_Remaining := Team_Remaining + 1;
                  end if;
               end loop;

               Concorde.Empires.Logging.Log
                 (Team.Leader,
                  "ships remaining:" & Team_Remaining'Img);
               if Team_Remaining > 0 then
                  Remaining_Teams := Remaining_Teams + 1;
                  if Remaining_Teams = 1 then
                     Victor := Team.Leader;
                  else
                     Victor := null;
                  end if;
               end if;
            end;
         end loop;

         if Remaining_Teams = 0 then
            Empires.Logging.Log
              (Teams.First_Element.Leader,
               "mutual annihilation");
         elsif Remaining_Teams = 1 then
            Empires.Logging.Log
              (Victor, "victory!");
         else
            Empires.Logging.Log
              (Teams.First_Element.Leader,
               "the battle continues");
         end if;
      end;

   end Fight;

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
      for I in 2 .. Es'Last loop
         for J in 1 .. I - 1 loop
            if Es (I).Relationship (Es (J)) < 0
              or else Es (J).Relationship (Es (I)) < 0
            then
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Has_Conflict;

end Concorde.Ships.Battles;
