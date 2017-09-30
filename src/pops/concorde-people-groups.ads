private with Ada.Containers.Doubly_Linked_Lists;

private with Memor.Database;

with Memor;
with Memor.Element_Vectors;

with Concorde.Commodities;
with Concorde.Objects;

package Concorde.People.Groups is

   type Affiliation_Range is new Unit_Real;

   type Root_Pop_Group is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   function Initial_Cash_Factor
     (Group : Root_Pop_Group'Class)
      return Natural;

   function Preferred_Quality
     (Group : Root_Pop_Group'Class)
      return Concorde.Commodities.Commodity_Quality;

   procedure Scan_Needs
     (Group : Root_Pop_Group'Class;
      Process : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Type;
                   Need      : Non_Negative_Real));

   type Pop_Group is access constant Root_Pop_Group'Class;

   function Get (Name : String) return Pop_Group;

   function Poor return Pop_Group;
   function Middle_Class return Pop_Group;
   function Rich return Pop_Group;

   type Affiliation_Interface is limited interface;

   function Affiliation
     (Affiliator : Affiliation_Interface;
      Group        : Pop_Group)
      return Affiliation_Range
      is abstract;

   function Wealth_Group
     (Affiliator : Affiliation_Interface'Class)
      return Pop_Group;

   function Affiliated
     (Affiliator   : Affiliation_Interface'Class;
      Group        : Pop_Group)
      return Boolean
   is (Affiliator.Affiliation (Group) > 0.0);

   function Poor
     (Affiliator   : Affiliation_Interface'Class)
      return Boolean
   is (Affiliator.Affiliated (Groups.Poor));

   function Middle_Class
     (Affiliator   : Affiliation_Interface'Class)
      return Boolean
   is (Affiliator.Affiliated (Groups.Middle_Class));

   function Rich
     (Affiliator   : Affiliation_Interface'Class)
      return Boolean
   is (Affiliator.Affiliated (Groups.Rich));

   type Affiliation_Vector is tagged private;

   function Get_Affiliation_Range
     (Vector : Affiliation_Vector'Class;
      Group  : Pop_Group)
      return Affiliation_Range;

   procedure Set_Affiliation_Range
     (Vector : in out Affiliation_Vector'Class;
      Group  : Pop_Group;
      Value  : Affiliation_Range);

private

   type Need_Record is
      record
         Commodity : Concorde.Commodities.Commodity_Type;
         Need      : Non_Negative_Real;
      end record;

   package Needs_List is
     new Ada.Containers.Doubly_Linked_Lists (Need_Record);

   type Root_Pop_Group is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Initial_Cash_Factor : Natural;
         Preferred_Quality   : Concorde.Commodities.Commodity_Quality;
         Needs               : Needs_List.List;
      end record;

   overriding function Object_Database
     (Item : Root_Pop_Group)
      return Memor.Memor_Database;

   package Affiliation_Vectors is
     new Memor.Element_Vectors (Root_Pop_Group, Affiliation_Range, 0.0);

   type Affiliation_Vector is new Affiliation_Vectors.Vector with null record;

   function Get_Affiliation_Range
     (Vector : Affiliation_Vector'Class;
      Group  : Pop_Group)
      return Affiliation_Range
   is (Vector.Element (Group));

   package Db is
     new Memor.Database
       ("pop-group", Root_Pop_Group, Pop_Group);

end Concorde.People.Groups;
