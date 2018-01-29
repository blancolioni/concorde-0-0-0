with Concorde.Powers;

package Concorde.Laws.Bureaucracy is

   function Delegate_Power
     (Context   : Law_Context;
      Power     : Concorde.Powers.Power_Type)
      return Law_Type;

end Concorde.Laws.Bureaucracy;
