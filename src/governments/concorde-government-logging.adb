with Concorde.Logs;

package body Concorde.Government.Logging is

   Income_Tax_Heading     : aliased constant String := "Income Tax";
   Sales_Tax_Heading      : aliased constant String := "Sales Tax";
   Corporate_Tax_Heading  : aliased constant String := "Corporate Tax";

   Government_Heading_Count : constant := 3;

   Government_Headings : constant array (1 .. Government_Heading_Count)
     of access constant String :=
       (1 => Income_Tax_Heading'Access,
        2 => Sales_Tax_Heading'Access,
        3 => Corporate_Tax_Heading'Access);

   type Government_Logger is
     new Concorde.Logs.Log_Interface with
      record
         Government : Government_Type;
      end record;

   overriding function Path (Log : Government_Logger) return String;

   overriding function Field_Count (Log : Government_Logger) return Natural;

   overriding function Heading
     (Log   : Government_Logger;
      Index : Positive)
      return String;

   overriding function Value
     (Log   : Government_Logger;
      Index : Positive)
      return String;

   -----------------
   -- Field_Count --
   -----------------

   overriding function Field_Count (Log : Government_Logger) return Natural is
      pragma Unreferenced (Log);
   begin
      return Government_Heading_Count;
   end Field_Count;

   -------------
   -- Heading --
   -------------

   overriding function Heading
     (Log   : Government_Logger;
      Index : Positive)
      return String
   is
      pragma Unreferenced (Log);
   begin
      return Government_Headings (Index).all;
   end Heading;

   ---------
   -- Log --
   ---------

   procedure Log (Government : Government_Type) is
      Log : constant Government_Logger :=
              Government_Logger'
                (Government => Government);
   begin
      Concorde.Logs.Log (Log);
   end Log;

   ----------
   -- Path --
   ----------

   overriding function Path (Log : Government_Logger) return String is
   begin
      return Log.Government.Owner.Identifier
        & "/"
        & Log.Government.Identifier;
   end Path;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Log   : Government_Logger;
      Index : Positive)
      return String
   is

      function Receipt (Source : Revenue_Source) return String
      is (Concorde.Money.Image (Log.Government.Tax_Receipts (Source)));

   begin
      case Index is
         when 1 =>
            return Receipt (Income_Tax);
         when 2 =>
            return Receipt (Sales_Tax);
         when 3 =>
            return Receipt (Corporate_Tax);
         when others =>
            raise Program_Error;
      end case;
   end Value;

end Concorde.Government.Logging;
