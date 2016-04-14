with Concorde.Installations.Db;

package body Concorde.Installations is

   --------------
   -- Facility --
   --------------

   function Facility
     (Installation : Root_Installation_Type'Class)
      return Concorde.Facilities.Facility_Type
   is
   begin
      return Installation.Facility;
   end Facility;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Installation_Type)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------
   -- Owner --
   -----------

   function Owner
     (Installation : Root_Installation_Type'Class)
      return access constant Concorde.Agents.Root_Agent_Type'Class
   is
   begin
      return Installation.Owner;
   end Owner;

end Concorde.Installations;
