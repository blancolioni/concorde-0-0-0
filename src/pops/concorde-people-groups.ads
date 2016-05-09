with Memor;

with Concorde.Commodities;
with Concorde.Objects;

package Concorde.People.Groups is

   type Root_Pop_Group is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   function Initial_Cash_Factor
     (Group : Root_Pop_Group'Class)
      return Natural;

   function Preferred_Quality
     (Group : Root_Pop_Group'Class)
      return Concorde.Commodities.Commodity_Quality;

   type Pop_Group is access constant Root_Pop_Group'Class;

   function Get (Name : String) return Pop_Group;

   function Poor return Pop_Group;
   function Middle_Class return Pop_Group;
   function Rich return Pop_Group;

private

   type Root_Pop_Group is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Initial_Cash_Factor : Natural;
         Preferred_Quality   : Concorde.Commodities.Commodity_Quality;
      end record;

   overriding function Object_Database
     (Item : Root_Pop_Group)
      return Memor.Root_Database_Type'Class;

end Concorde.People.Groups;
