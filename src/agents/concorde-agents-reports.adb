with Ada.Text_IO;

with Concorde.Calendar;

package body Concorde.Agents.Reports is

   --------------------
   -- Write_Accounts --
   --------------------

   procedure Write_Accounts
     (Agent : Agent_Type)
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
         Put (Concorde.Calendar.Image (Transaction.Date));
         Set_Col (12);
         case Transaction.Entry_Type is
            when Concorde.Trades.Bid =>
               Put ("Bid");
            when Concorde.Trades.Ask =>
               Put ("Ask");
         end case;
         Set_Col (20);
         Put (Transaction.Item.Name);
         Set_Col (36);
         Put (WL.Money.Image
              (WL.Money.Price
                 (Transaction.Cost, Transaction.Quantity)));
         Set_Col (52);
         Put (WL.Quantities.Image (Transaction.Quantity));
         Set_Col (68);
         Put (WL.Money.Image (Transaction.Cost));
         Set_Col (84);
         Put (WL.Money.Image (Transaction.Balance));
         New_Line;
      end loop;
   end Write_Accounts;

end Concorde.Agents.Reports;
