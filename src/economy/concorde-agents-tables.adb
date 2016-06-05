package body Concorde.Agents.Tables is

   Account_Table_Column_Count : constant := 6;

   subtype Account_Table_Column is
     Integer range 1 .. Account_Table_Column_Count;

   type Root_Account_Table is
     new Lui.Tables.Root_Model_Table with
      record
         Agent : access constant Root_Agent_Type'Class;
      end record;

   overriding function Cell_Text
     (Table  : Root_Account_Table;
      Row    : Positive;
      Column : Positive)
      return String;

   overriding function Heading_Column_Text
     (Table  : Root_Account_Table;
      Column : Positive)
      return String
   is ((case Account_Table_Column (Column) is
           when 1 => "Date",
           when 2 => "Type",
           when 3 => "Item",
           when 4 => "Quantity",
           when 5 => "Cost",
           when 6 => "Balance"));

   overriding function Row_Count
     (Table : Root_Account_Table)
      return Natural
   is (Table.Agent.Account.Last_Index);

   -------------------
   -- Account_Table --
   -------------------

   function Account_Table
     (Agent : not null access constant Root_Agent_Type'Class)
      return Lui.Tables.Model_Table
   is
      Table : constant Lui.Tables.Model_Table :=
                new Root_Account_Table'
                  (Lui.Tables.Root_Model_Table with Agent => Agent);
   begin
      Table.Initialise ("Accounts", Num_Rows => 0,
                        Num_Cols    => Account_Table_Column_Count);
      return Table;
   end Account_Table;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table  : Root_Account_Table;
      Row    : Positive;
      Column : Positive)
      return String
   is
      Data : Account_Entry renames Table.Agent.Account (Row);
   begin
      case Account_Table_Column (Column) is
         when 1 =>
            return Concorde.Dates.To_String (Data.Date);
         when 2 =>
            case Data.Entry_Type is
               when Concorde.Trades.Buy =>
                  return "buy";
               when Concorde.Trades.Sell =>
                  return "sell";
            end case;
         when 3 =>
            return Data.Item.Name;
         when 4 =>
            return Concorde.Quantities.Image (Data.Quantity);
         when 5 =>
            return Concorde.Money.Image (Data.Cost);
         when 6 =>
            return Concorde.Money.Image (Data.Balance);
      end case;
   end Cell_Text;

end Concorde.Agents.Tables;
