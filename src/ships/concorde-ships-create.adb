with Concorde.Empires;
with Concorde.Systems;

with Concorde.Roman_Images;

with Concorde.Empires.Logging;

package body Concorde.Ships.Create is

   --------------
   -- New_Ship --
   --------------

   procedure New_Ship
     (Owner  : Concorde.Empires.Empire_Type;
      System : Concorde.Systems.Star_System_Access;
      Max_HP : Natural)
   is
      use Concorde.Roman_Images;
      Ship : constant Ship_Type := new Root_Ship_Type;
   begin
      if Owner.Current_Ships = 0 then
         Ship.Set_Name (Owner.Name);
      else
         Ship.Set_Name (Owner.Name & " "
                        & Roman_Image (Owner.Current_Ships + 1));
      end if;
      Ship.Owner := Owner;
      Ship.System := System;
      Ship.Destination := null;
      Ship.Alive := True;
      Ship.HP := 1;
      Ship.Max_HP := Max_HP;
      Ship_Vector.Append (Ship);

      Owner.New_Ship;
      System.Add_Ship (Ship);

      Concorde.Empires.Logging.Log
        (Owner, System.Name & ": new ship: " & Ship.Name);

   end New_Ship;

end Concorde.Ships.Create;
