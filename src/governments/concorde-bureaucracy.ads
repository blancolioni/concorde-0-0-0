with Concorde.Powers;

package Concorde.Bureaucracy is

   type Bureaucratic_Interface is limited interface;

   type Bureaucracy_Type is access constant Bureaucratic_Interface'Class;

   function Variable_Reference
     (Item : not null access constant Bureaucratic_Interface)
      return access Bureaucratic_Interface'Class
      is abstract;

   function Has_Power
     (Item : Bureaucratic_Interface;
      Power : Concorde.Powers.Power_Type)
      return Boolean
      is abstract;

   procedure Add_Power
     (Item  : in out Bureaucratic_Interface;
      Power : Concorde.Powers.Power_Type)
   is abstract;

   procedure Scan_Powers
     (Item : Bureaucratic_Interface;
      Process : not null access
        procedure (Power : Concorde.Powers.Power_Type))
   is abstract;

end Concorde.Bureaucracy;
