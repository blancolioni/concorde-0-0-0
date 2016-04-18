with Concorde.Empires;
with Concorde.Systems;

with Concorde.Roman_Images;

with Concorde.Empires.Db;
with Concorde.Empires.Logging;

with Concorde.Ships.Db;
with Concorde.Ships.Designs;

package body Concorde.Ships.Create is

   --------------
   -- New_Ship --
   --------------

   function New_Ship
     (Owner  : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Name   : String;
      System : in out Concorde.Systems.Root_Star_System_Type'Class;
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
      begin
         Concorde.Ships.Designs.Create_Ship_From_Design
           (Design, Ship);
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
         Ship.System_Reference := System.Reference;
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

         Concorde.Empires.Logging.Log
           (Owner, System.Name & ": new ship: " & Ship.Name);
      end Create;

   begin
      return Db.Create (Create'Access);
   end New_Ship;

end Concorde.Ships.Create;
