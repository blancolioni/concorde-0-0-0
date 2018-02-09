with Concorde.Powers;

limited with Concorde.People.Individuals;

package Concorde.Bureaucracy is

   type Bureaucratic_Interface is limited interface;

   type Bureaucracy_Type is access constant Bureaucratic_Interface'Class;

   function Variable_Reference
     (Item : not null access constant Bureaucratic_Interface)
      return access Bureaucratic_Interface'Class
      is abstract;

   function Director
     (Bureaucracy : Bureaucratic_Interface)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class
   is abstract;

   function Has_Power
     (Item : Bureaucratic_Interface;
      Power : Concorde.Powers.Power_Type)
      return Boolean
      is abstract;

   function Has_Delegated_Power
     (Item  : Bureaucratic_Interface;
      Power : Concorde.Powers.Power_Type)
      return Boolean
      is abstract;

   function Delegated_To
     (Item  : Bureaucratic_Interface;
      Power : Concorde.Powers.Power_Type)
      return not null access constant Bureaucratic_Interface'Class
      is abstract
     with Pre'Class => Item.Has_Delegated_Power (Power);

   procedure Add_Power
     (Item  : in out Bureaucratic_Interface;
      Power : Concorde.Powers.Power_Type)
   is abstract;

   procedure Remove_Power
     (Item  : in out Bureaucratic_Interface;
      Power : Concorde.Powers.Power_Type)
   is abstract;

   procedure Delegate_Power
     (Item  : in out Bureaucratic_Interface;
      Power : Concorde.Powers.Power_Type;
      To    : not null access constant Bureaucratic_Interface'Class)
   is abstract;

   procedure Scan_Powers
     (Item : Bureaucratic_Interface;
      Process : not null access
        procedure (Power : Concorde.Powers.Power_Type))
   is abstract;

   function Find_With_Power
     (Item : not null access constant Bureaucratic_Interface'Class;
      Power : Concorde.Powers.Power_Type)
      return not null access constant Bureaucratic_Interface'Class;

end Concorde.Bureaucracy;
