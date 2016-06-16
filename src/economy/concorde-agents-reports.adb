with Ada.Text_IO;

with Concorde.Dates;

package body Concorde.Agents.Reports is

   --------------------
   -- Write_Accounts --
   --------------------

   procedure Write_Accounts
     (Agent : Root_Agent_Type'Class)
   is
      use Ada.Text_IO;
   begin
      Put ("Date");
      Set_Col (12);
      Put ("Trade");
      Set_Col (20);
      Put ("Commodity");
      Set_Col (36);
      Put ("Price");
      Set_Col (52);
      Put ("Quantity");
      Set_Col (68);
      Put ("Total");
      Set_Col (84);
      Put ("Cash");
      New_Line;

      for Transaction of Agent.Account loop
         Put (Concorde.Dates.To_String (Transaction.Date));
         Set_Col (12);
         case Transaction.Entry_Type is
            when Concorde.Trades.Buy =>
               Put ("Bid");
            when Concorde.Trades.Sell =>
               Put ("Ask");
         end case;
         Set_Col (20);
         Put (Transaction.Item.Name);
         Set_Col (36);
         Put (Concorde.Money.Image
              (Concorde.Money.Price
                 (Transaction.Cost, Transaction.Quantity)));
         Set_Col (52);
         Put (Concorde.Quantities.Image (Transaction.Quantity));
         Set_Col (68);
         Put (Concorde.Money.Image (Transaction.Cost));
         Set_Col (84);
         Put (Concorde.Money.Image (Transaction.Balance));
         New_Line;
      end loop;
   end Write_Accounts;

end Concorde.Agents.Reports;
