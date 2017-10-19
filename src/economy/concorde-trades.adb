with Ada.Characters.Handling;

package body Concorde.Trades is

   ------------------------
   -- Get_Daily_Quantity --
   ------------------------

   function Get_Daily_Quantity
     (Trade    : Trade_Interface'Class;
      Item     : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Metric   : Trade_Metric;
      Days     : Positive := 1)
      return WL.Quantities.Quantity_Type
   is
      use Concorde.Calendar;
      use WL.Quantities;
      Finish_Date : constant Time := Clock;
      Start_Date  : constant Time :=
                      Finish_Date - Day_Duration'Last * Duration (Days);
      Result : constant WL.Quantities.Quantity_Type :=
                      Trade.Get_Quantity (Item, Metric,
                                          Start_Date, Finish_Date);
   begin
      return Result;
   end Get_Daily_Quantity;

   ---------------
   -- Metric_Id --
   ---------------

   function Metric_Id (Metric : Trade_Metric) return String is
      Result : String :=
                 Ada.Characters.Handling.To_Lower
                   (Trade_Metric'Image (Metric));
   begin
      for Ch of Result loop
         if Ch = '_' then
            Ch := '-';
         end if;
      end loop;
      return Result;
   end Metric_Id;

end Concorde.Trades;
