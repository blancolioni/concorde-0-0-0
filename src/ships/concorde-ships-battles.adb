with WL.Random;

with Concorde.Empires;
with Concorde.Empires.Logging;

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

   -----------
   -- Fight --
   -----------

   procedure Fight
     (System : Concorde.Systems.Star_System_Access;
      Ships  : Concorde.Ships.Lists.List)
   is
      use Concorde.Empires;
      Teams       : Team_Vectors.Vector;
      Info_Vector : Ship_Info_Vectors.Vector;
   begin

      System.Battle (Positive (Ships.Length));

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
               end;
            end if;

            Info_Vector.Append ((Team_Index, Ship, null));
         end;
      end loop;

      pragma Assert (Teams.Last_Index >= 2);

      Concorde.Empires.Logging.Log
        (Empire  => Teams.First_Element.Leader,
         Message => "The Battle of " & System.Name);

      for Team of Teams loop
         Concorde.Empires.Logging.Log
           (Team.Leader,
            "fleet size:"
            & Natural'Image (Natural (Team.Ships.Length)));
      end loop;

      for Round_Index in 1 .. 10 loop
         declare
            Continue : Boolean := False;
         begin
            for Ship_Info_Index in 1 .. Info_Vector.Last_Index loop
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
                     Empires.Logging.Log
                       (Ship.Owner, Ship.Name & " destroyed");
                  end if;
               end if;
            end loop;

            exit when not Continue;
         end;
      end loop;

   end Fight;

   ------------------
   -- Has_Conflict --
   ------------------

   function Has_Conflict
     (Ships : Concorde.Ships.Lists.List)
      return Boolean
   is
      use Concorde.Empires;
      E : Empire_Type := null;
   begin
      for Ship of Ships loop
         if E = null then
            E := Empire_Type (Ship.Owner);
         elsif Empire_Type (Ship.Owner) /= E then
            return True;
         end if;
      end loop;
      return False;
   end Has_Conflict;

end Concorde.Ships.Battles;
