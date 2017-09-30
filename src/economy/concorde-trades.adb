package body Concorde.Trades is

   ------------------------
   -- Get_Daily_Quantity --
   ------------------------

   function Get_Daily_Quantity
     (Trade    : Trade_Interface'Class;
      Item     : Concorde.Commodities.Commodity_Type;
      Metric   : Trade_Metric;
      Days     : Positive := 1)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
      Finish_Date : constant Concorde.Dates.Date_Type :=
                      Concorde.Dates.Current_Date;
      Start_Date  : constant Concorde.Dates.Date_Type :=
                     Concorde.Dates.Add_Days
                        (Finish_Date, -Days);
      Result : constant Concorde.Quantities.Quantity_Type :=
                      Trade.Get_Quantity (Item, Metric,
                                          Start_Date, Finish_Date);
   begin
      return Result;
   end Get_Daily_Quantity;

end Concorde.Trades;
