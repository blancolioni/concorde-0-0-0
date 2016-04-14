with Concorde.Facilities;
with Concorde.People.Skills;

package Concorde.Commodities.Configure is

   procedure Configure_Commodities;

   procedure Create_From_Skill
     (Skill : Concorde.People.Skills.Pop_Skill);

   procedure Create_From_Service
     (Service_Facility : Concorde.Facilities.Facility_Type);

end Concorde.Commodities.Configure;
