with Concorde.Objects.Queues;

with Concorde.Government;

with Concorde.Managers.Pops;

with Concorde.Random;
with Concorde.Calendar;

package body Concorde.People.Pops.Create is

   -------------
   -- New_Pop --
   -------------

   function New_Pop
     (Location : Concorde.Locations.Object_Location;
      Market   : access constant Concorde.Trades.Trade_Interface'Class;
      Group    : Concorde.People.Groups.Pop_Group;
      Size     : Pop_Size;
      Cash     : WL.Money.Money_Type)
      return Pop_Type
   is
      procedure Create (Pop : in out Root_Pop_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Pop : in out Root_Pop_Type'Class) is
         use WL.Quantities;
      begin
         Pop.Group := Group;
         Pop.Size := Size;
         Pop.New_Agent
           (Location       => Location,
            Government     => Concorde.Government.Get_Government (Location),
            Market         => Market,
            Cash           => Cash,
            Stock_Capacity => Pop.Size_Quantity * To_Quantity (70.0));
         if Pop.Group.Unemployment then
            Pop.Add_Quantity (Group.Work_Commodity, Pop.Size_Quantity,
                              WL.Money.Total
                                (Group.Work_Commodity.Base_Price,
                                 Pop.Size_Quantity));
         end if;
      end Create;

      use type Concorde.Calendar.Time;

   begin
      return Pop : constant Pop_Type := Db.Create (Create'Access) do
         Pop.Save_Agent;
         Concorde.Managers.Pops.Create_Manager (Pop).Activate;
         Concorde.Objects.Queues.Next_Event
           (Pop,
            Concorde.Calendar.Clock
            + Duration (Concorde.Random.Unit_Random * 86_400.0));
      end return;
   end New_Pop;

end Concorde.People.Pops.Create;
