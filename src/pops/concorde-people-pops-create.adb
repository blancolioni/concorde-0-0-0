with Concorde.Objects.Queues;

with Concorde.Managers.Pops;

with Concorde.Random;
with Concorde.Dates;

package body Concorde.People.Pops.Create is

   -------------
   -- New_Pop --
   -------------

   function New_Pop
     (Location       : Concorde.Locations.Object_Location;
      Market         : access constant Concorde.Trades.Trade_Interface'Class;
      Wealth_Group   : Concorde.People.Groups.Pop_Group;
      Skill          : Concorde.People.Skills.Pop_Skill;
      Size           : Pop_Size;
      Cash           : Concorde.Money.Money_Type)
      return Pop_Type
   is
      procedure Create (Pop : in out Root_Pop_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Pop : in out Root_Pop_Type'Class) is
         use Concorde.Quantities;
      begin
         Pop.Groups.Set_Affiliation_Range (Wealth_Group, 1.0);
         Pop.Skills.Append (Skill);
         Pop.Size := Size;
         Pop.Set_Cash (Cash);
         Pop.New_Agent (Location, Market,
                        Pop.Size_Quantity * To_Quantity (70.0));
         Pop.Add_Quantity (Skill.Commodity, Pop.Size_Quantity,
                           Concorde.Money.Total
                             (Skill.Commodity.Base_Price,
                              Pop.Size_Quantity));
      end Create;

   begin
      return Pop : constant Pop_Type := Db.Create (Create'Access) do
         Pop.Save_Agent;
         Concorde.Managers.Pops.Create_Manager (Pop).Activate;
         Concorde.Objects.Queues.Next_Event
           (Pop,
            Concorde.Dates.Add_Seconds
              (Concorde.Dates.Current_Date,
               Concorde.Random.Unit_Random * 86_400.0));
         Pop.Log_Trade ("created: skill quantity = "
                        & Concorde.Quantities.Image
                          (Pop.Get_Quantity (Skill.Commodity)));
      end return;
   end New_Pop;

end Concorde.People.Pops.Create;
