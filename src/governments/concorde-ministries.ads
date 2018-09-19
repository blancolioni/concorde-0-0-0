private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with Ada.Strings.Unbounded;
private with Ada.Characters.Handling;
private with Memor.Database;

with WL.Money;

with Concorde.Agents;
with Concorde.Bureaucracy;
with Concorde.Installations;
with Concorde.Objects;
with Concorde.Powers;

limited with Concorde.People.Communities;
limited with Concorde.People.Individuals;

package Concorde.Ministries is

   type Root_Ministry_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Objects.User_Named_Object_Interface
     and Concorde.Bureaucracy.Bureaucratic_Interface
   with private;

   type Ministry_Type is access constant Root_Ministry_Type'Class;

   overriding function Name
     (Ministry : Root_Ministry_Type)
      return String;

   overriding procedure Set_Name
     (Ministry : in out Root_Ministry_Type;
      New_Name : String);

   function Has_Minister
     (Ministry : Root_Ministry_Type'Class)
      return Boolean;

   function Minister
     (Ministry : Root_Ministry_Type'Class)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class;

   procedure Set_Minister
     (Ministry : in out Root_Ministry_Type'Class;
      Minister : access constant
        Concorde.People.Individuals.Root_Individual_Type'Class);

   function Daily_Budget
     (Ministry : Root_Ministry_Type'Class)
      return WL.Money.Money_Type;

   overriding function Short_Name
     (Ministry : Root_Ministry_Type)
      return String;

   overriding function Director
     (Ministry : Root_Ministry_Type)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class;

   overriding function Has_Power
     (Ministry : Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type)
      return Boolean;

   overriding function Has_Delegated_Power
     (Ministry : Root_Ministry_Type;
      Power    : Concorde.Powers.Power_Type)
      return Boolean;

   overriding function Delegated_To
     (Ministry : Root_Ministry_Type;
      Power    : Concorde.Powers.Power_Type)
      return not null access constant
     Concorde.Bureaucracy.Bureaucratic_Interface'Class;

   overriding procedure Add_Power
     (Ministry : in out Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type);

   overriding procedure Remove_Power
     (Ministry   : in out Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type);

   overriding procedure Delegate_Power
     (Ministry   : in out Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type;
      To         : not null access constant
        Concorde.Bureaucracy.Bureaucratic_Interface'Class);

   overriding procedure Scan_Powers
     (Ministry   : Root_Ministry_Type;
      Process    : not null access
        procedure (Power : Concorde.Powers.Power_Type));

   overriding function Check_Powers
     (Ministry   : Root_Ministry_Type;
      Test       : not null access
        function (Power : Concorde.Powers.Power_Type) return Boolean)
      return Boolean;

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

   package Power_Holder is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Powers.Power_Type, Concorde.Powers."=");

   type Delegated_Power_Record is
      record
         Power        : Power_Holder.Holder;
         Delegated_To : access constant
           Concorde.Bureaucracy.Bureaucratic_Interface'Class;
      end record;

   package Delegated_Power_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Delegated_Power_Record);

   type Root_Ministry_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Objects.User_Named_Object_Interface
     and Concorde.Bureaucracy.Bureaucratic_Interface with
      record
         Name              : Ada.Strings.Unbounded.Unbounded_String;
         Minister          : access constant
           Concorde.People.Individuals.Root_Individual_Type'Class;
         Area              : Concorde.Objects.Object_Type;
         Community         : access constant
           Concorde.People.Communities.Root_Community_Type'Class;
         Powers            : Concorde.Powers.Power_Set;
         Delegated_Powers  : Delegated_Power_Lists.List;
         Daily_Budget      : WL.Money.Money_Type;
      end record;

   overriding procedure Update_Agent
     (Ministry     : not null access constant Root_Ministry_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class));

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

   overriding function Director
     (Ministry : Root_Ministry_Type)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class
   is (Ministry.Minister);

   overriding function Has_Power
     (Ministry : Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type)
      return Boolean
   is (Ministry.Powers.Has_Power (Power));

   overriding function Has_Delegated_Power
     (Ministry : Root_Ministry_Type;
      Power    : Concorde.Powers.Power_Type)
      return Boolean
   is (for some Rec of Ministry.Delegated_Powers =>
          Concorde.Powers."=" (Rec.Power.Element, Power));

   overriding function Check_Powers
     (Ministry   : Root_Ministry_Type;
      Test       : not null access
        function (Power : Concorde.Powers.Power_Type) return Boolean)
      return Boolean
   is (Ministry.Powers.Check_Powers (Test));

   overriding function Name
     (Ministry : Root_Ministry_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Ministry.Name));

   overriding function Short_Name
     (Ministry : Root_Ministry_Type)
      return String
   is (Ada.Characters.Handling.To_Lower
       (Name (Ministry))
       & "-ministry");

   function Has_Minister
     (Ministry : Root_Ministry_Type'Class)
      return Boolean
   is (not Concorde.Ministries."=" (Ministry.Minister, null));

   function Minister
     (Ministry : Root_Ministry_Type'Class)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class
   is (Ministry.Director);

   function Daily_Budget
     (Ministry : Root_Ministry_Type'Class)
      return WL.Money.Money_Type
   is (Ministry.Daily_Budget);

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
