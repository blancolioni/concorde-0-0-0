with Concorde.Empires;
with Concorde.Systems;
with Concorde.Worlds;

with Concorde.Roman_Images;

with Concorde.Empires.Logging;

with Concorde.Ships.Designs;

with Concorde.Money;
with Concorde.Quantities;

package body Concorde.Ships.Create is

   --------------
   -- New_Ship --
   --------------

   function New_Ship
     (Owner  : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Name   : String;
      World  : Concorde.Worlds.World_Type;
      Design : String)
      return Ship_Type
   is

      procedure Create
        (Ship : in out Root_Ship_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create
        (Ship : in out Root_Ship_Type'Class)
      is
         use Concorde.Quantities;

      begin
         Concorde.Ships.Designs.Create_Ship_From_Design
           (Design, Ship);
         Ship.New_Agent
           (Concorde.Locations.Geosynchronous_Orbit
              (World),
            World.Market,
            To_Quantity (Ship.Hold_Size));

         if Name = "" then
            if Owner.Current_Ships = 0 then
               Ship.Set_Name (Owner.Name);
            else
               Ship.Set_Name
                 (Owner.Name & " "
                  & Concorde.Roman_Images.Roman_Image
                    (Owner.Current_Ships + 1));
            end if;
         else
            Ship.Set_Name (Name);
         end if;
         Ship.Owner := Owner;
         Ship.Set_Guarantor (Owner);
         Ship.Set_Cash (Concorde.Money.To_Money (10_000.0));
         Ship.Set_Location
           (Concorde.Locations.Orbit
              (World, 300_000.0));
         Ship.Dest_World := null;
         Ship.Dest_System := null;
         Ship.Alive := True;

         declare
            Id : constant String :=
                   "00000" & Memor.To_String (Ship.Reference);
         begin
            Ship.Identity := "1" & Id (Id'Last - 4 .. Id'Last);
         end;

         Owner.Update.New_Ship;

         Concorde.Empires.Logging.Log
           (Owner, World.Name & ": new ship: " & Ship.Name);
      end Create;

      Ship : constant Ship_Type := Db.Create (Create'Access);
   begin
      World.System.Update.Add_Ship (Ship);
      return Ship;
   end New_Ship;

end Concorde.Ships.Create;
