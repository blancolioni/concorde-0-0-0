private with Ada.Strings.Unbounded;
private with Memor.Database;

with Concorde.Agents;
with Concorde.Bureaucracy;
with Concorde.Installations;
with Concorde.Objects;
with Concorde.Powers;

limited with Concorde.People.Individuals;

package Concorde.Ministries is

   type Root_Ministry_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Bureaucracy.Bureaucratic_Interface
   with private;

   type Ministry_Type is access constant Root_Ministry_Type'Class;

   function Minister
     (Ministry : Root_Ministry_Type'Class)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class;

   procedure Set_Minister
     (Ministry : in out Root_Ministry_Type'Class;
      Minister : access constant
        Concorde.People.Individuals.Root_Individual_Type'Class);

   function Headquarters
     (Ministry : Root_Ministry_Type'Class)
      return Concorde.Installations.Installation_Type;

   overriding function Short_Name
     (Ministry : Root_Ministry_Type)
      return String;

   overriding function Has_Power
     (Ministry : Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type)
      return Boolean;

   overriding procedure Add_Power
     (Ministry : in out Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type);

   function Daily_Work
     (Ministry : Root_Ministry_Type'Class)
      return Duration;

   type Updateable_Reference
     (Item : not null access Root_Ministry_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Ministry_Type'Class)
      return Updateable_Reference;

private

   type Root_Ministry_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Bureaucracy.Bureaucratic_Interface with
      record
         Name              : Ada.Strings.Unbounded.Unbounded_String;
         Minister          : access constant
           Concorde.People.Individuals.Root_Individual_Type'Class;
         Area              : Concorde.Objects.Object_Type;
         Headquarters      : Concorde.Installations.Installation_Type;
         Powers            : Concorde.Powers.Power_Set;
      end record;

   overriding function Class_Name
     (Ministry : Root_Ministry_Type)
      return String
   is ("ministry");

   overriding function Object_Database
     (Ministry : Root_Ministry_Type)
      return Memor.Memor_Database;

   overriding function Variable_Reference
     (Ministry : not null access constant Root_Ministry_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is (Ministry.Update.Item);

   overriding function Variable_Reference
     (Ministry : not null access constant Root_Ministry_Type)
      return access Concorde.Bureaucracy.Bureaucratic_Interface'Class
   is (Ministry.Update.Item);

   overriding function Has_Power
     (Ministry : Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type)
      return Boolean
   is (Ministry.Powers.Contains (Power));

   overriding function Short_Name
     (Ministry : Root_Ministry_Type)
      return String
   is ("ministry");

   function Minister
     (Ministry : Root_Ministry_Type'Class)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class
   is (Ministry.Minister);

   function Headquarters
     (Ministry : Root_Ministry_Type'Class)
      return Concorde.Installations.Installation_Type
   is (Ministry.Headquarters);

   package Db is
     new Memor.Database
       ("ministry", Root_Ministry_Type, Ministry_Type);

   type Updateable_Reference
     (Item : not null access Root_Ministry_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

   overriding function Object_Database
     (Ministry : Root_Ministry_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

end Concorde.Ministries;
