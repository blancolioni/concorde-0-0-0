with Concorde.Money;

with Concorde.People.Groups;
with Concorde.People.Skills;

package Concorde.People.Pops.Create is

   function New_Pop
     (Location       : not null access
        Concorde.Agents.Agent_Location_Interface'Class;
      Location_Index : Natural;
      Wealth_Group   : Concorde.People.Groups.Pop_Group;
      Skill          : Concorde.People.Skills.Pop_Skill;
      Size           : Pop_Size;
      Cash           : Concorde.Money.Money_Type)
      return Pop_Type;

end Concorde.People.Pops.Create;
