with Ada.Text_IO;

with Concorde.Calendar;

package body Concorde.Agents.Reports is

   ----------------
   -- Log_Status --
   ----------------

   procedure Log_Status
     (Market : Concorde.Markets.Market_Interface'Class)
   is
      use Concorde.Money;

      function Higher_Wealth (Left, Right : Agent_Type) return Boolean
      is (Left.Cash > Right.Cash);

      package Agent_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Agent_Type);

      package Agent_Sorting is
        new Agent_Lists.Generic_Sorting (Higher_Wealth);

      List : Agent_Lists.List;

      procedure Add_Agent (Agent : not null access constant
                             Concorde.Agents.Root_Agent_Type'Class);

      ---------------
      -- Add_Agent --
      ---------------

      procedure Add_Agent (Agent : not null access constant
                             Concorde.Agents.Root_Agent_Type'Class)
      is
      begin
         List.Append (Agent_Type (Agent));
      end Add_Agent;

   begin
      Market.Scan_Agents (Add_Agent'Access);
      Agent_Sorting.Sort (List);
      for Agent of List loop
         Agent.Log ("cash: " & Show (Agent.Cash));
      end loop;
   end Log_Status;

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
