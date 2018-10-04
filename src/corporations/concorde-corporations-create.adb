with Concorde.Calendar;
with Concorde.Random;

with Concorde.People.Communities;

with Concorde.Objects.Queues;
with Concorde.Managers.Corporations;

package body Concorde.Corporations.Create is

   ---------------------
   -- New_Corporation --
   ---------------------

   function New_Corporation
     (Market          : not null access constant
        Concorde.Trades.Trade_Interface'Class;
      Government      : not null access constant
        Concorde.Government.Root_Government_Type'Class;
      Owner           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Community       : not null access constant
        Concorde.People.Communities.Root_Community_Type'Class;
      Business        : Corporation_Business_Type;
      Requirements    : Concorde.Commodities.Stock_Interface'Class;
      Size            : Concorde.Quantities.Quantity_Type;
      Cash            : Concorde.Money.Money_Type)
      return Corporation_Type
   is
      procedure Create (Corporation : in out Root_Corporation_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Corporation : in out Root_Corporation_Type'Class) is
         use Concorde.Quantities;
      begin
         Corporation.New_Agent
           (Location       =>
              Concorde.Locations.In_Community (Community),
            Government     => Government,
            Market         => Market,
            Cash           => Cash,
            Stock_Capacity =>
              (if Business = Banking then Size else Scale (Size, 10.0)));
         Corporation.Owner := Owner;
         Corporation.Community := Community;
         Corporation.Business := Business;
         Corporation.Size := Size;
         Corporation.Set_Guarantor (Owner);

         declare
            procedure Process
              (Commodity : Concorde.Commodities.Commodity_Type);

            -------------
            -- Process --
            -------------

            procedure Process
              (Commodity : Concorde.Commodities.Commodity_Type)
            is
            begin
               Corporation.Requirements.Add_Quantity
                 (Commodity, Requirements.Get_Quantity (Commodity),
                  Requirements.Get_Value (Commodity));
               Corporation.Commodities.Append (Commodity);
            end Process;

         begin
            Requirements.Scan_Stock (Process'Access);
         end;

      end Create;

      use type Concorde.Calendar.Time;
   begin
      return Corporation : constant Corporation_Type :=
        Db.Create (Create'Access)
      do
         Corporation.Save_Agent;
         Concorde.Managers.Corporations.Create_Manager (Corporation).Activate;
         Concorde.Objects.Queues.Next_Event
           (Corporation,
            Concorde.Calendar.Clock
            + Duration (Concorde.Random.Unit_Random * 86_400.0));
         Corporation.Log
           (Concorde.Quantities.Show (Corporation.Size));
      end return;
   end New_Corporation;

end Concorde.Corporations.Create;
