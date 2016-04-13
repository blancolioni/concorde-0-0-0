private with Memor;

with Concorde.Agents;

with Concorde.Facilities;

package Concorde.Installations is

   type Root_Installation_Type is
     new Concorde.Agents.Root_Agent_Type
   with private;

   type Installation_Type is access constant Root_Installation_Type'Class;

private

   type Root_Installation_Type is
     new Concorde.Agents.Root_Agent_Type with
      record
         Facility : Concorde.Facilities.Facility_Type;
         Owner    : access constant Concorde.Agents.Root_Agent_Type'Class;
      end record;

   overriding function Object_Database
     (Item : Root_Installation_Type)
      return Memor.Root_Database_Type'Class;

end Concorde.Installations;
