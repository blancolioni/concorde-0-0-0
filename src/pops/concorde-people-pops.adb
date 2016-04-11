with Concorde.People.Pops.Db;

package body Concorde.People.Pops is

   -----------------
   -- Affiliation --
   -----------------

   function Affiliation
     (Pop   : Root_Pop_Type'Class;
      Group : Concorde.People.Groups.Pop_Group)
      return Affiliation_Range
   is
   begin
      return Pop.Groups.Element (Group.Reference);
   end Affiliation;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Pop_Type)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   ----------
   -- Size --
   ----------

   function Size (Pop : Root_Pop_Type'Class) return Pop_Size is
   begin
      return Pop.Size;
   end Size;

end Concorde.People.Pops;
