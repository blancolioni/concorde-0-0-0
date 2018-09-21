with Concorde.Logging;
with Concorde.Real_Images;

package body Concorde.Production is

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Production : Root_Production_Type'Class;
      Stock      : in out Concorde.Commodities.Stock_Interface'Class;
      Size       : Non_Negative_Real;
      Cost       : out Concorde.Money.Money_Type)
   is
      use Concorde.Money;
      use Concorde.Quantities;
      Capacity : Non_Negative_Real := Size;

      procedure Log_Capacity
        (Quantity : Quantity_Type;
         Message  : String);

      ------------------
      -- Log_Capacity --
      ------------------

      procedure Log_Capacity
        (Quantity : Quantity_Type;
         Message  : String)
      is
      begin
         Concorde.Logging.Log
           (Production.Identifier, "", "production",
            Message
            & " " & Show (Quantity)
            & ": capacity "
            & Concorde.Real_Images.Approximate_Image (Capacity)
            & " of "
            & Concorde.Real_Images.Approximate_Image (Size)
            & " ("
            & Concorde.Real_Images.Approximate_Image (Capacity / Size * 100.0)
            & "%)");
      end Log_Capacity;

   begin

      for Input of Production.Inputs loop
         declare
            Required  : constant Non_Negative_Real :=
                          Size * Input.Relative_Quantity;
            Available : constant Non_Negative_Real :=
                          To_Real (Stock.Get_Quantity (Input.Commodity));
            Max       : Non_Negative_Real := Size;
         begin
            if Available < Required then
               Max := Max * Available / Required;
            end if;
            Capacity := Real'Min (Capacity, Max);
            Log_Capacity (Stock.Get_Quantity (Input.Commodity),
                          Input.Commodity.Identifier);
         end;
      end loop;

      if Capacity > 0.0 then
         for Input of Production.Inputs loop
            declare
               Used : constant Quantity_Type :=
                        (if Input.Commodity.Is_Pop_Group
                         then Stock.Get_Quantity (Input.Commodity)
                         else To_Quantity
                           (Capacity * Input.Relative_Quantity
                            * Input.Consumption));
               This_Cost : constant Concorde.Money.Money_Type :=
                             Stock.Get_Value (Input.Commodity);
            begin
               Stock.Remove_Quantity (Input.Commodity, Used, This_Cost);
               Cost := Cost + This_Cost;
            end;
         end loop;

         Concorde.Logging.Log
           (Production.Identifier, "", "production",
            "total production cost: "
            & Concorde.Money.Show (Cost));

         declare
            Total_Output : Non_Negative_Real := 0.0;
         begin
            for Output of Production.Outputs loop
               Total_Output := Total_Output + Output.Relative_Quantity;
            end loop;

            for Output of Production.Outputs loop
               declare
                  Quantity  : constant Quantity_Type :=
                                To_Quantity
                                  (Capacity * Output.Relative_Quantity);
                  This_Cost : constant Money_Type :=
                                Adjust
                                  (Cost,
                                   Output.Relative_Quantity / Total_Output);
               begin
                  Concorde.Logging.Log
                    (Production.Identifier, "", "production",
                     "produce " & Show (Quantity)
                     & " " & Output.Commodity.Identifier
                     & " for " & Show (This_Cost)
                     & " (minimum sell price "
                     & Show (Price (This_Cost, Quantity))
                     & ")");

                  Stock.Add_Quantity
                    (Item     => Output.Commodity,
                     Quantity =>
                       Min (Quantity, Stock.Available_Quantity),
                     Value    => This_Cost);
               end;

            end loop;
         end;
      end if;
   end Execute;

   ------------
   -- Exists --
   ------------

   function Exists (Name : String) return Boolean is
   begin
      return Db.Exists (Name);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Production_Type is
   begin
      return Db.Get (Name);
   end Get;

   --------------------
   -- Input_Quantity --
   --------------------

   function Input_Quantity
     (Production : Root_Production_Type'Class;
      Commodity  : Concorde.Commodities.Commodity_Type;
      Size       : Non_Negative_Real)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Commodities.Commodity_Type;
   begin
      for Input of Production.Inputs loop
         if Input.Commodity = Commodity then
            declare
               Q : constant Concorde.Quantities.Quantity_Type :=
                     Concorde.Quantities.To_Quantity
                       (Input.Relative_Quantity * Size);
            begin
               return Q;
            end;
         end if;
      end loop;
      return Concorde.Quantities.Zero;
   end Input_Quantity;

   ---------------
   -- Is_Output --
   ---------------

   function Is_Output
     (Production : Root_Production_Type'Class;
      Commodity  : Concorde.Commodities.Commodity_Type)
      return Boolean
   is
      use type Concorde.Commodities.Commodity_Type;
   begin
      for Output of Production.Outputs loop
         if Output.Commodity = Commodity then
            return True;
         end if;
      end loop;
      return False;
   end Is_Output;

   ------------------
   -- Minimum_Size --
   ------------------

   function Minimum_Size
     (Production : Root_Production_Type'Class;
      To_Produce : Concorde.Commodities.Stock_Interface'Class)
      return Non_Negative_Real
   is
   begin
      return Size : Non_Negative_Real := 0.0 do
         for Output of Production.Outputs loop
            declare
               This_Quantity : constant Non_Negative_Real :=
                                 Concorde.Quantities.To_Real
                                   (To_Produce.Get_Quantity
                                      (Output.Commodity));
               This_Size     : constant Non_Negative_Real :=
                                 This_Quantity / Output.Relative_Quantity;
            begin
               Size := Real'Max (Size, This_Size);
            end;
         end loop;
      end return;
   end Minimum_Size;

   -------------
   -- Outputs --
   -------------

   function Outputs
     (Production : Root_Production_Type'Class)
      return Concorde.Commodities.Array_Of_Commodities
   is
      Count : Natural := 0;
   begin
      return Arr : Concorde.Commodities.Array_Of_Commodities
        (1 .. Natural (Production.Outputs.Length))
      do
         for Output of Production.Outputs loop
            Count := Count + 1;
            Arr (Count) := Output.Commodity;
         end loop;
      end return;
   end Outputs;

   -------------------------
   -- Relative_Input_Cost --
   -------------------------

   function Relative_Input_Cost
     (Production : Root_Production_Type'Class;
      Commodity  : Concorde.Commodities.Commodity_Type)
      return Unit_Real
   is
      use type Concorde.Commodities.Commodity_Type;
      Total_Cost     : Non_Negative_Real := 0.0;
      Commodity_Cost : Non_Negative_Real := 0.0;
   begin
      for Input of Production.Inputs loop
         declare
            Item_Cost : constant Non_Negative_Real :=
                          Concorde.Money.To_Real
                            (Input.Commodity.Base_Price);
            This_Cost : constant Non_Negative_Real :=
                          Input.Relative_Quantity
                            * Item_Cost
                            * Input.Consumption;
         begin
            Total_Cost := Total_Cost + This_Cost;
            if Input.Commodity = Commodity then
               Commodity_Cost := This_Cost;
            end if;
         end;
      end loop;
      if Commodity_Cost = 0.0 then
         return 0.0;
      else
         return Commodity_Cost / Total_Cost;
      end if;
   end Relative_Input_Cost;

end Concorde.Production;
