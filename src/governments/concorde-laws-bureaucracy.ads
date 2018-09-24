with Concorde.Ministries;

with Concorde.Powers;

with Concorde.People.Groups;

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
      Budget   : Concorde.Ministries.Ministry_Budget;
      Powers   : Concorde.Powers.Power_Set)
      return Law_Type;

end Concorde.Laws.Bureaucracy;
