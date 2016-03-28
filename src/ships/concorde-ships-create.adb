with Concorde.Empires;
with Concorde.Systems;

with Concorde.Roman_Images;

with Concorde.Empires.Logging;

with Concorde.Ships.Designs;

package body Concorde.Ships.Create is

   --------------
   -- New_Ship --
   --------------

   procedure New_Ship
     (Owner  : Concorde.Empires.Empire_Type;
      System : Concorde.Systems.Star_System_Access;
      Design : String)
   is
      use Concorde.Roman_Images;
      Ship : constant Ship_Type :=
               Concorde.Ships.Designs.Create_Ship_From_Design
                 (Design);
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

      Ship_Vector.Append (Ship);

      declare
         Id : Natural := Ship_Vector.Last_Index;
      begin
         Ship.Identity := "10000";
         for I in reverse Ship.Identity'Range loop
            exit when Id = 0;
            Ship.Identity (I) :=
              Character'Val (Character'Pos ('0') + Id mod 10);
            Id := Id / 10;
         end loop;
      end;

      Owner.New_Ship;
      System.Add_Ship (Ship);

      Concorde.Empires.Logging.Log
        (Owner, System.Name & ": new ship: " & Ship.Name);

   end New_Ship;

end Concorde.Ships.Create;
