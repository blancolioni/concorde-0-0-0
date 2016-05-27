with Concorde.Empires;
with Concorde.Systems;
with Concorde.Worlds;

with Concorde.Roman_Images;

with Concorde.Empires.Db;
with Concorde.Empires.Logging;

with Concorde.Ships.Db;
with Concorde.Ships.Designs;

with Concorde.Systems.Db;
with Concorde.Worlds.Db;

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
      World  : in out Concorde.Worlds.Root_World_Type'Class;
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

         procedure Update_System
           (System : in out Concorde.Systems.Root_Star_System_Type'Class);

         -------------------
         -- Update_System --
         -------------------

         procedure Update_System
           (System : in out Concorde.Systems.Root_Star_System_Type'Class)
         is
         begin
            System.Add_Ship
              (Concorde.Ships.Db.Reference (Ship));
         end Update_System;

      begin
         Concorde.Ships.Designs.Create_Ship_From_Design
           (Design, Ship);
         Ship.New_Agent
           (Concorde.Locations.Geosynchronous_Orbit
              (Concorde.Worlds.Db.Reference (World)),
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
         Ship.Set_Cash (Concorde.Money.To_Money (100_000.0));
         Ship.Set_Location
           (Concorde.Locations.Geosynchronous_Orbit
              (Concorde.Worlds.Db.Reference (World)));
         Ship.Dest_Reference := Memor.Null_Database_Reference;
         Ship.Alive := True;

         declare
            Id : constant String :=
                   "00000" & Memor.To_String (Ship.Reference);
         begin
            Ship.Identity := "1" & Id (Id'Last - 4 .. Id'Last);
         end;

         declare
            procedure Set_New_Ship
              (Empire : in out Concorde.Empires.Root_Empire_Type'Class);

            ------------------
            -- Set_New_Ship --
            ------------------

            procedure Set_New_Ship
              (Empire : in out Concorde.Empires.Root_Empire_Type'Class)
            is
            begin
               Empire.New_Ship;
            end Set_New_Ship;

         begin
            Concorde.Empires.Db.Update (Owner.Reference, Set_New_Ship'Access);
         end;

         Concorde.Systems.Db.Update
           (World.System.Reference, Update_System'Access);

         Concorde.Empires.Logging.Log
           (Owner, World.Name & ": new ship: " & Ship.Name);
      end Create;

   begin
      return Db.Create (Create'Access);
   end New_Ship;

end Concorde.Ships.Create;
