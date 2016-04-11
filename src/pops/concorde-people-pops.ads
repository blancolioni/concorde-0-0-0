private with Memor;
private with Memor.Element_Vectors;

with Concorde.Agents;

with Concorde.People.Groups;

package Concorde.People.Pops is

   type Pop_Size is range 1 .. 9_999_999;

   type Affiliation_Range is new Unit_Real;

   type Root_Pop_Type is
     new Concorde.Agents.Root_Agent_Type with private;

   function Size (Pop : Root_Pop_Type'Class) return Pop_Size;

   function Affiliation
     (Pop   : Root_Pop_Type'Class;
      Group : Concorde.People.Groups.Pop_Group)
      return Affiliation_Range;

   type Pop_Type is access constant Root_Pop_Type'Class;

private

   package Group_Affiliation_Vectors is
     new Memor.Element_Vectors (Affiliation_Range, 0.0);

   type Root_Pop_Type is
     new Concorde.Agents.Root_Agent_Type with
      record
         Size   : Pop_Size;
         Groups : Group_Affiliation_Vectors.Vector;
      end record;

   overriding function Object_Database
     (Item : Root_Pop_Type)
      return Memor.Root_Database_Type'Class;

end Concorde.People.Pops;
