with Memor;
with Concorde.Objects;

package Concorde.People.Groups is

   type Root_Pop_Group is
     new Concorde.Objects.Root_Named_Object_Type with private;

   type Pop_Group is access constant Root_Pop_Group'Class;

   function Get (Name : String) return Pop_Group;

private

   type Root_Pop_Group is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Pop_Group_Id : access String;
      end record;

   overriding function Object_Database
     (Item : Root_Pop_Group)
      return Memor.Root_Database_Type'Class;

   overriding function Identifier
     (Item : Root_Pop_Group)
      return String
   is (Item.Pop_Group_Id.all);

end Concorde.People.Groups;
