private with Memor.Database;
private with Concorde.Localisation;

with Memor;

with Concorde.Objects;

with Concorde.Commodities;
with Concorde.People.Attributes;
with Concorde.People.Skills;

package Concorde.People.Careers is

   type Rank_Count is range 0 .. 6;
   subtype Rank_Index is Rank_Count range 1 .. Rank_Count'Last;

   type Career_Interface is limited interface
     and Concorde.People.Attributes.Has_Attributes;

   type Root_Career_Type is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   function Qualified
     (Career    : Root_Career_Type'Class;
      Candidate : Career_Interface'Class)
      return Boolean;

   function Promotion_Chance
     (Career    : Root_Career_Type'Class;
      Rank      : Rank_Index;
      Candidate : Career_Interface'Class)
      return Unit_Real;

   function Titles
     (Career : Root_Career_Type'Class)
      return Boolean;

   type Career_Type is access constant Root_Career_Type'Class;

   function Exists (Name : String) return Boolean;

   function Get (Name : String) return Career_Type
     with Pre => Exists (Name);

   function Prestige
     (Career : Root_Career_Type'Class)
      return Natural;

   function Number_Of_Ranks
     (Career : Root_Career_Type'Class)
      return Rank_Count;

   function Rank_Name
     (Career : Root_Career_Type'Class;
      Index  : Rank_Index)
      return String
     with Pre => Index <= Career.Number_Of_Ranks;

   procedure Scan_Careers
     (Process : not null access
        procedure (Career : Career_Type));

   procedure Career_Term
     (Career : Root_Career_Type'Class;
      Rank   : Rank_Index;
      Target : in out Career_Interface'Class);

private

   type Rank_Record is
      record
         Name        : access String;
         Progression : Concorde.People.Attributes.Attribute_Container;
      end record;

   type Array_Of_Ranks is array (Rank_Index range <>) of Rank_Record;

   type Root_Career_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Qualifications : Concorde.People.Attributes.Attribute_Container;
         Advanced_Check : Concorde.People.Attributes.Attribute_Container;
         Promotion      : Concorde.People.Attributes.Attribute_Container;
         Development    : Concorde.People.Attributes.Attribute_Container;
         Service        : Concorde.People.Attributes.Attribute_Container;
         Specialist     : Concorde.People.Attributes.Attribute_Container;
         Advanced       : Concorde.People.Attributes.Attribute_Container;
         Titles         : Boolean;
         Prestige       : Natural;
         Ranks          : access Array_Of_Ranks;
      end record;

   overriding function Object_Database
     (Item : Root_Career_Type)
      return Memor.Memor_Database;

   function Titles
     (Career : Root_Career_Type'Class)
      return Boolean
   is (Career.Titles);

   package Db is
     new Memor.Database
       ("Career", Root_Career_Type, Career_Type);

   overriding function Object_Database
     (Item : Root_Career_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Exists (Name : String) return Boolean
   is (Db.Exists (Name));

   function Get (Name : String) return Career_Type
   is (Db.Get (Name));

   function Prestige
     (Career : Root_Career_Type'Class)
      return Natural
   is (Career.Prestige);

   function Number_Of_Ranks
     (Career : Root_Career_Type'Class)
      return Rank_Count
   is (Career.Ranks.all'Last);

   function Rank_Name
     (Career : Root_Career_Type'Class;
      Index  : Rank_Index)
      return String
   is (Concorde.Localisation.Local_Name
         (Career.Ranks (Index).Name.all));

end Concorde.People.Careers;
