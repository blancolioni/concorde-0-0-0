with Ada.Text_IO;

with Concorde.Money;
with Concorde.Quantities;
with Concorde.Logs;

package body Concorde.Markets.Reports is

   ----------------------
   -- Log_Market_State --
   ----------------------

   procedure Log_Market_State
     (Market : Market_Interface'Class)
   is
   begin
      for Commodity of Concorde.Commodities.All_Commodities loop
         declare
            use Concorde.Money, Concorde.Quantities;
            Log_Path : constant String :=
                         "markets/"
                         & Market.Identifier
                         & "/" & Commodity.Identifier;
         begin
            Concorde.Logs.Log_Fields
              (Log_Path,
               Image (Market.Current_Supply (Commodity)),
               Image (Market.Current_Demand (Commodity)),
               Image (Market.Current_Price (Commodity)));
         end;
      end loop;
   end Log_Market_State;

   -------------------
   -- Report_Market --
   -------------------

   procedure Report_Market
     (Market : Market_Interface'Class)
   is
      use Ada.Text_IO;

      procedure Show_Line (Commodity : Concorde.Commodities.Commodity_Type);

      procedure Show_Line (Commodity : Concorde.Commodities.Commodity_Type) is

         procedure Put_Right
           (Column : Count;
            Width  : Natural;
            Text   : String);

         ---------------
         -- Put_Right --
         ---------------

         procedure Put_Right
           (Column : Count;
            Width  : Natural;
            Text   : String)
         is
         begin
            if Text'Length > Width then
               Set_Col (Column - Count (Text'Length - Width));
            else
               Set_Col (Column + Count (Width - Text'Length));
            end if;
            Put (Text);
         end Put_Right;

         use Concorde.Money, Concorde.Quantities;

         Supply : constant Quantity_Type :=
                    Market.Current_Supply (Commodity);
         Demand : constant Quantity_Type :=
                    Market.Current_Demand (Commodity);
         Price  : constant Price_Type :=
                    Market.Current_Price (Commodity);
      begin
         if Supply > Zero or else Demand > Zero then
            Put (Commodity.Name);
            Put_Right (24, 6, Show (Supply));
            Put_Right (40, 6, Show (Demand));
            Put_Right (56, 5, Show (Price));
            New_Line;
         end if;
      end Show_Line;

   begin
      Put ("COMMODITY");
      Set_Col (24);
      Put ("SUPPLY");
      Set_Col (40);
      Put ("DEMAND");
      Set_Col (56);
      Put ("PRICE");
      New_Line;
      Concorde.Commodities.Scan
        (Show_Line'Access);
   end Report_Market;

end Concorde.Markets.Reports;
