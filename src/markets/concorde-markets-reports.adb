with Ada.Text_IO;

with Concorde.Money;
with Concorde.Quantities;
with Concorde.Logs;

package body Concorde.Markets.Reports is

   Supply_Heading : aliased constant String := "Supply";
   Demand_Heading : aliased constant String := "Demand";
   Price_Heading  : aliased constant String := "Price";

   Commodity_Heading_Count : constant := 3;

   Commodity_Headings : constant array (1 .. Commodity_Heading_Count)
     of access constant String :=
       (1 => Supply_Heading'Access,
        2 => Demand_Heading'Access,
        3 => Price_Heading'Access);

   package Market_Commodity_Vectors is
     new Ada.Containers.Vectors
       (Positive, Concorde.Commodities.Commodity_Type,
        Concorde.Commodities."=");

   Market_Commodities : Market_Commodity_Vectors.Vector;

   type Market_Logger is
     new Concorde.Logs.Log_Interface with
      record
         Market : access constant Market_Interface'Class;
      end record;

   overriding function Path (Log : Market_Logger) return String;

   overriding function Field_Count (Log : Market_Logger) return Natural;

   overriding function Heading
     (Log   : Market_Logger;
      Index : Positive)
      return String;

   overriding function Value
     (Log   : Market_Logger;
      Index : Positive)
      return String;

   -----------------
   -- Field_Count --
   -----------------

   overriding function Field_Count (Log : Market_Logger) return Natural is
      pragma Unreferenced (Log);
   begin
      return Market_Commodities.Last_Index * 3;
   end Field_Count;

   -------------
   -- Heading --
   -------------

   overriding function Heading
     (Log   : Market_Logger;
      Index : Positive)
      return String
   is
      pragma Unreferenced (Log);
      Commodity_Index : constant Positive :=
                          (Index - 1) / Commodity_Heading_Count + 1;
      Heading_Index   : constant Positive :=
                          (Index - 1) mod Commodity_Heading_Count + 1;
   begin
      return Market_Commodities.Element (Commodity_Index).Identifier
        & " " & Commodity_Headings (Heading_Index).all;
   end Heading;

   ----------------------
   -- Log_Market_State --
   ----------------------

   procedure Log_Market_State
     (Market : not null access constant Market_Interface'Class)
   is
      Logger : constant Market_Logger := (Market => Market);
   begin

      if Market_Commodities.Is_Empty then
         for Item of Concorde.Commodities.All_Commodities loop
            Market_Commodities.Append (Item);
         end loop;
      end if;

      Concorde.Logs.Log (Logger);
      for Commodity of Concorde.Commodities.All_Commodities loop
         declare
            use Concorde.Money, Concorde.Quantities;
            Log_Path : constant String :=
                         "markets/"
                         & Market.Identifier
                         & "/" & Commodity.Identifier;
         begin
            Concorde.Logs.Log_Fields
              (Log_Path,
               Show (Market.Current_Supply (Commodity)),
               Show (Market.Current_Demand (Commodity)),
               Image (Market.Base_Price (Commodity)),
               Image (Market.Current_Price (Commodity)));
         end;
      end loop;
   end Log_Market_State;

   ----------
   -- Path --
   ----------

   overriding function Path (Log : Market_Logger) return String is
   begin
      return "markets/" & Log.Market.Identifier;
   end Path;

   -------------------
   -- Report_Market --
   -------------------

   procedure Report_Market
     (Market : Market_Interface'Class)
   is
      use Ada.Text_IO;

      procedure Show_Line (Commodity : Concorde.Commodities.Commodity_Type);

      procedure Show_Line (Commodity : Concorde.Commodities.Commodity_Type) is

         procedure Put_Right
           (Column : Count;
            Width  : Natural;
            Text   : String);

         ---------------
         -- Put_Right --
         ---------------

         procedure Put_Right
           (Column : Count;
            Width  : Natural;
            Text   : String)
         is
         begin
            if Text'Length > Width then
               Set_Col (Column - Count (Text'Length - Width));
            else
               Set_Col (Column + Count (Width - Text'Length));
            end if;
            Put (Text);
         end Put_Right;

         use Concorde.Money, Concorde.Quantities;

         Supply : constant Quantity_Type :=
                    Market.Current_Supply (Commodity);
         Demand : constant Quantity_Type :=
                    Market.Current_Demand (Commodity);
         Price  : constant Price_Type :=
                    Market.Current_Price (Commodity);
      begin
         if Supply > Zero or else Demand > Zero then
            Put (Commodity.Name);
            Put_Right (24, 6, Show (Supply));
            Put_Right (40, 6, Show (Demand));
            Put_Right (56, 5, Show (Price));
            New_Line;
         end if;
      end Show_Line;

   begin
      Put ("COMMODITY");
      Set_Col (24);
      Put ("SUPPLY");
      Set_Col (40);
      Put ("DEMAND");
      Set_Col (56);
      Put ("PRICE");
      New_Line;
      Concorde.Commodities.Scan
        (Show_Line'Access);
   end Report_Market;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Log   : Market_Logger;
      Index : Positive)
      return String
   is
      use Concorde.Money, Concorde.Quantities;
      Commodity_Index : constant Positive :=
                          (Index - 1) / Commodity_Heading_Count + 1;
      Heading_Index   : constant Positive :=
                          (Index - 1) mod Commodity_Heading_Count + 1;
      Commodity       : constant Concorde.Commodities.Commodity_Type :=
                          Market_Commodities.Element (Commodity_Index);
   begin
      case Heading_Index is
         when 1 =>
            return Image (Log.Market.Current_Supply (Commodity));
         when 2 =>
            return Image (Log.Market.Current_Demand (Commodity));
         when 3 =>
            return Image (Log.Market.Current_Price (Commodity));
         when others =>
            raise Constraint_Error with
              "bad index:" & Index'Image;
      end case;
   end Value;

end Concorde.Markets.Reports;
