with Concorde.People.Pops.Db;

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
         Pop.Groups.Replace_Element (Wealth_Group.Reference, 1.0);
         Pop.Skills.Append (Skill);
         Pop.Size := Size;
         Pop.Set_Cash (Cash);
         Pop.New_Agent (Location, Market,
                        Pop.Size_Quantity * To_Quantity (70.0));
      end Create;

   begin
      return Db.Create (Create'Access);
   end New_Pop;

end Concorde.People.Pops.Create;
