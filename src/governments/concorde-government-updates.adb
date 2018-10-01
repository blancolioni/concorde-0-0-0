with Concorde.Money;

package body Concorde.Government.Updates is

   --------------------------
   -- Process_Tax_Receipts --
   --------------------------

   procedure Process_Tax_Receipts
     (Government : in out Root_Government_Type'Class)
   is
      use type Concorde.Money.Money_Type;

      function Show (X : Concorde.Money.Money_Type) return String
      is (Concorde.Money.Show (X));

      function Receipt
        (Source : Revenue_Source)
         return Concorde.Money.Money_Type
      is (Government.Tax_Receipts (Source));

      Total_Revenue : Concorde.Money.Money_Type :=
                        Concorde.Money.Zero;
   begin
      for Revenue of Government.Tax_Receipts loop
         Total_Revenue := Total_Revenue + Revenue;
      end loop;

      Government.Log
        ("tax receipts: "
         & "total: " & Show (Total_Revenue)
         & "; income tax: " & Show (Receipt (Income_Tax))
         & "; sales tax: " & Show (Receipt (Sales_Tax))
         & "; import tariff: " & Show (Receipt (Import_Tariff))
         & "; export tariff: " & Show (Receipt (Export_Tariff))
         & "; corporate tax: " & Show (Receipt (Corporate_Tax))
         & "; cash: " & Show (Government.Cash));

      declare
         Tithe : constant Concorde.Money.Money_Type :=
                   Concorde.Money.Adjust
                     (Total_Revenue, Government.Owner_Tithe);
      begin
         Government.Log ("tithe: " & Show (Tithe));
         Government.Require_Cash (Tithe);
         Government.Remove_Cash (Tithe);
         Government.Owner.Variable_Reference.Add_Cash (Tithe);
      end;

      Government.Tax_Receipts := (others => Concorde.Money.Zero);

   end Process_Tax_Receipts;

end Concorde.Government.Updates;
