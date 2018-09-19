with Concorde.Money;

with Concorde.Powers;

package Concorde.Laws.Bureaucracy is

   function Delegate_Power
     (Context   : Law_Context;
      Power     : Concorde.Powers.Power_Type)
      return Law_Type;

   function Create_Power
     (Context   : Law_Context;
      Power     : Concorde.Powers.Power_Type)
      return Law_Type;

   function Create_Ministry
     (Context  : Law_Context;
      Name     : String;
      Budget   : Concorde.Money.Money_Type;
      Powers   : Concorde.Powers.Power_Set)
      return Law_Type;

end Concorde.Laws.Bureaucracy;
