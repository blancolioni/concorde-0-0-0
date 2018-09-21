with Concorde.Money;

with Concorde.Objects.Queues;

with Concorde.Managers.Pops;

with Concorde.Random;
with Concorde.Calendar;

package body Concorde.People.Pops.Create is

   -------------
   -- New_Pop --
   -------------

   function New_Pop
     (Market     : access constant Concorde.Trades.Trade_Interface'Class;
      Government : not null access constant
        Concorde.Government.Root_Government_Type'Class;
      Location   : Concorde.Locations.Object_Location;
      Network    : Concorde.Network.Network_State_Interface'Class;
      Groups     : Concorde.People.Groups.Array_Of_Pop_Groups;
      Size       : Pop_Size;
      Apathy     : Unit_Real)
      return Pop_Type
   is
      procedure Create (Pop : in out Root_Pop_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Pop : in out Root_Pop_Type'Class) is
         use Concorde.Quantities;
      begin
         for Group of Groups loop
            Pop.Groups.Append
              (Group_Membership_Record'
                 (Group    => Group,
                  Income   => Network.Node (Group.Income_Node.Identifier),
                  Strength => 1.0));
            if Group.Is_Wealth_Group then
               Pop.Base_Income := Network.Node (Group.Income_Node.Identifier);
            end if;
         end loop;

         Pop.Apathy := Apathy;
         Pop.Size := Size;
         Pop.New_Agent
           (Location       => Location,
            Government     => Government,
            Market         => Market,
            Cash           =>
              Concorde.Money.To_Money
                (Non_Negative_Real (Size)
                 * Pop.Base_Income.Current_Actual_Value),
            Stock_Capacity => Pop.Size_Quantity * To_Quantity (70.0));

      end Create;

      use type Concorde.Calendar.Time;

   begin
      return Pop : constant Pop_Type := Db.Create (Create'Access) do
         Pop.Save_Agent;
         if False then
            Concorde.Managers.Pops.Create_Manager (Pop).Activate;
            Concorde.Objects.Queues.Next_Event
              (Pop,
               Concorde.Calendar.Clock
               + Duration (Concorde.Random.Unit_Random * 86_400.0));
         end if;
      end return;
   end New_Pop;

end Concorde.People.Pops.Create;
