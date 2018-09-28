with Concorde.Calendar;
with Concorde.Objects.Queues;
with Concorde.Quantities;
with Concorde.Random;
with Concorde.Real_Images;

with Concorde.Managers.Industries;

with Concorde.People.Communities;

package body Concorde.Industries.Create is

   ------------------
   -- New_Industry --
   ------------------

   function New_Industry
     (Market          : not null access constant
        Concorde.Trades.Trade_Interface'Class;
      Government      : not null access constant
        Concorde.Government.Root_Government_Type'Class;
      Owner           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Community       : not null access constant
        Concorde.People.Communities.Root_Community_Type'Class;
      Production      : not null access constant
        Concorde.Production.Root_Production_Type'Class;
      Size            : Non_Negative_Real;
      Cash            : Concorde.Money.Money_Type)
      return Industry_Type
   is

      procedure Create (Industry : in out Root_Industry_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Industry : in out Root_Industry_Type'Class) is
         use Concorde.Quantities;
      begin
         Industry.New_Agent
           (Location       =>
              Concorde.Locations.In_Community (Community),
            Government     => Government,
            Market         => Market,
            Cash           => Cash,
            Stock_Capacity => To_Quantity (Size * 10.0));
         Industry.Owner := Owner;
         Industry.Community := Community;
         Industry.Production :=
           Concorde.Production.Production_Type (Production);
         Industry.Size := Size;
         Industry.Production_Size := Size;
         Industry.Sold.Create_Stock (Concorde.Quantities.Zero, True);
         Industry.Set_Guarantor (Owner);

         for Commodity of Concorde.Commodities.All_Commodities loop
            if not Commodity.Is_Set (Concorde.Commodities.Transient)
              and then not Commodity.Is_Set (Concorde.Commodities.Virtual)
            then
               declare
                  Quantity : constant Concorde.Quantities.Quantity_Type :=
                               Industry.Production.Input_Quantity
                                 (Commodity, Industry.Size);
               begin
                  if Quantity > Zero then
                     Industry.Log ("adding: "
                                   & Show (Quantity)
                                   & " "
                                   & Commodity.Identifier);
                     Industry.Add_Quantity
                       (Commodity, Quantity,
                        Concorde.Money.Total (Commodity.Base_Price, Quantity));
                  end if;
               end;
            end if;
         end loop;

      end Create;

      use type Concorde.Calendar.Time;
   begin
      return Industry : constant Industry_Type := Db.Create (Create'Access) do
         Industry.Save_Agent;
         Concorde.Managers.Industries.Create_Manager (Industry).Activate;
         Concorde.Objects.Queues.Next_Event
           (Industry,
            Concorde.Calendar.Clock
            + Duration (Concorde.Random.Unit_Random * 86_400.0));
         Industry.Log
           (Industry.Production.Name & " "
            & Concorde.Real_Images.Approximate_Image (Industry.Size));
      end return;
   end New_Industry;

end Concorde.Industries.Create;
