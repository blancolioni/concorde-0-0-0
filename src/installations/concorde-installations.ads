private with Memor;

with Concorde.Agents;
with Concorde.Trades;

with Concorde.Facilities;

with Concorde.Locations;

package Concorde.Installations is

   type Root_Installation_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Locations.Located_Interface
   with private;

   function Is_Colony_Hub
     (Installation : Root_Installation_Type'Class)
      return Boolean;

   function Facility
     (Installation : Root_Installation_Type'Class)
      return Concorde.Facilities.Facility_Type;

   function Owner
     (Installation : Root_Installation_Type'Class)
      return access constant Concorde.Agents.Root_Agent_Type'Class;

   type Installation_Type is access constant Root_Installation_Type'Class;

private

   type Root_Installation_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Locations.Located_Interface with
      record
         Facility : Concorde.Facilities.Facility_Type;
         Owner    : access constant Concorde.Agents.Root_Agent_Type'Class;
         Location : Concorde.Locations.Object_Location;
      end record;

   overriding function Short_Name
     (Installation : Root_Installation_Type)
      return String
   is ("[" & Memor.To_String (Installation.Reference) & "]"
       & " " & Installation.Facility.Name);

   overriding function Object_Database
     (Item : Root_Installation_Type)
      return Memor.Root_Database_Type'Class;

   overriding procedure Add_Trade_Offers
     (Item   : not null access constant Root_Installation_Type);

   overriding function Current_Location
     (Installation : Root_Installation_Type)
      return Concorde.Locations.Object_Location
   is (Installation.Location);

   overriding procedure Set_Location
     (Installation : in out Root_Installation_Type;
      Location     : Concorde.Locations.Object_Location);

end Concorde.Installations;
