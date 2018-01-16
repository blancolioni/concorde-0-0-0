private with Memor.Database;
private with WL.String_Maps;

with Memor;

with Concorde.Objects;

with Concorde.Commodities;
with Concorde.People.Groups;

package Concorde.People.Skills is

   type Root_Skill_Type is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   type Skill_Type is access constant Root_Skill_Type'Class;

   function Exists (Name : String) return Boolean;

   function Get (Name : String) return Skill_Type
     with Pre => Exists (Name);

   type Skill_Level is range 0 .. 10;

   type Has_Skills_Interface is limited interface;

   function Level
     (Set   : Has_Skills_Interface;
      Skill : not null access constant Root_Skill_Type'Class)
      return Skill_Level
      is abstract;

   procedure Scan
     (Set : Has_Skills_Interface;
      Process : not null access
        procedure (Skill : Skill_Type;
                   Level : Skill_Level))
   is abstract;

   type Skill_Set is new Has_Skills_Interface with private;

   overriding function Level
     (Set   : Skill_Set;
      Skill : not null access constant Root_Skill_Type'Class)
      return Skill_Level;

   overriding procedure Scan
     (Set     : Skill_Set;
      Process : not null access
        procedure (Skill : Skill_Type;
                   Level : Skill_Level));

private

   type Root_Skill_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         null;
      end record;

   overriding function Object_Database
     (Item : Root_Skill_Type)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       ("skill", Root_Skill_Type, Skill_Type);

   overriding function Object_Database
     (Item : Root_Skill_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   package Skill_Maps is new WL.String_Maps (Skill_Level);

   type Skill_Set is
     new Skill_Maps.Map
     and Has_Skills_Interface
   with null record;

   overriding function Level
     (Set   : Skill_Set;
      Skill : not null access constant Root_Skill_Type'Class)
      return Skill_Level
   is (if Set.Contains (Skill.Identifier)
       then Set.Element (Skill.Identifier)
       else 0);

   function Exists (Name : String) return Boolean
   is (Db.Exists (Name));

   function Get (Name : String) return Skill_Type
   is (Db.Get (Name));

end Concorde.People.Skills;
