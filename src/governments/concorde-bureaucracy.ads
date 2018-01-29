with Concorde.Powers;

package Concorde.Bureaucracy is

   type Bureaucratic_Interface is limited interface;

   type Bureaucracy_Type is access constant Bureaucratic_Interface'Class;

   procedure Add_Power
     (Item  : in out Bureaucratic_Interface;
      Power : Concorde.Powers.Power_Type)
   is abstract;

end Concorde.Bureaucracy;
