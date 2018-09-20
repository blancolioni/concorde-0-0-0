with Ada.Text_IO;

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

         declare
            Total_Output : Non_Negative_Real := 0.0;
         begin
            for Output of Production.Outputs loop
               Total_Output := Total_Output + Output.Relative_Quantity;
            end loop;

            for Output of Production.Outputs loop
               Stock.Add_Quantity
                 (Item     => Output.Commodity,
                  Quantity =>
                    To_Quantity (Capacity * Output.Relative_Quantity),
                  Value    =>
                    Adjust (Cost, Output.Relative_Quantity / Total_Output));
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
               Ada.Text_IO.Put_Line
                 (Production.Name & " requires "
                  & Concorde.Quantities.Show (Q)
                  & " "
                  & Commodity.Name);
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
