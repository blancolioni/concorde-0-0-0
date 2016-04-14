with Concorde.Components.Configure;
with Concorde.Ships.Designs;

with Concorde.Commodities.Configure;
with Concorde.Facilities.Configure;
with Concorde.People.Groups.Configure;
with Concorde.People.Skills.Configure;

package body Concorde.Configure is

   ------------------------
   -- Load_Configuration --
   ------------------------

   procedure Load_Configuration is
   begin
      Concorde.Commodities.Configure.Configure_Commodities;
      Concorde.People.Groups.Configure.Configure_Pop_Groups;
      Concorde.People.Skills.Configure.Configure_Pop_Skills;

      Concorde.Components.Configure.Configure_Components;
      Concorde.Facilities.Configure.Configure_Facilities;
      Concorde.Ships.Designs.Configure_Designs;

      Concorde.Commodities.Configure.Calculate_Base_Prices;

   end Load_Configuration;

end Concorde.Configure;
