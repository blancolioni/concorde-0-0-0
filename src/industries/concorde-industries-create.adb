with Concorde.Money;
with Concorde.Quantities;

package body Concorde.Industries.Create is

   ------------------
   -- New_Industry --
   ------------------

   function New_Industry
     (Market          : not null access constant
        Concorde.Trades.Trade_Interface'Class;
      Government      : not null access constant
        Concorde.Government.Root_Government_Type'Class;
      Production_Node : Concorde.Network.Node_State_Access;
      Inputs          : Concorde.Commodities.Array_Of_Commodities;
      Outputs         : Concorde.Commodities.Array_Of_Commodities)
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
           (Location       => Concorde.Locations.Nowhere,
            Government     => Government,
            Market         => Market,
            Cash           => Concorde.Money.Zero,
            Stock_Capacity => To_Quantity (1.0E6));
         Industry.Node := Production_Node;
         for Item of Inputs loop
            Industry.Inputs.Append (Item);
         end loop;
         for Item of Outputs loop
            Industry.Outputs.Append (Item);
         end loop;

      end Create;

   begin
      return Industry : constant Industry_Type := Db.Create (Create'Access) do
         Industry.Save_Agent;
      end return;
   end New_Industry;

end Concorde.Industries.Create;
