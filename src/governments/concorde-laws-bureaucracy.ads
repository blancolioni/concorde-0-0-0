with Concorde.Bureaucracy;
with Concorde.Powers;

package Concorde.Laws.Bureaucracy is

   function Delegate_Power
     (Power : Concorde.Powers.Power_Type;
      To    : Concorde.Bureaucracy.Bureaucracy_Type)
      return Law_Type;

end Concorde.Laws.Bureaucracy;
