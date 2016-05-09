with Ada.Text_IO;

with Concorde.Commodities.Db;

package body Concorde.Commodities is

   ------------------
   -- Add_Quantity --
   ------------------

   procedure Add_Quantity
     (Stock    : in out Stock_Interface'Class;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity;
      Value    : Concorde.Money.Money_Type)
   is
      use type Concorde.Money.Money_Type;

   begin
      Stock.Set_Quantity
        (Item,
         Stock.Get_Quantity (Item) + Quantity,
         Stock.Get_Value (Item) + Value);
   end Add_Quantity;

   ----------------
   -- Base_Price --
   ----------------

   function Base_Price
     (Commodity : Root_Commodity_Type'Class)
      return Concorde.Money.Price_Type
   is
   begin
      return Commodity.Base_Price;
   end Base_Price;

   -----------
   -- Class --
   -----------

   function Class
     (Commodity : Root_Commodity_Type'Class)
      return Commodity_Class
   is
   begin
      return Commodity.Class;
   end Class;

   ------------------
   -- Create_Stock --
   ------------------

   procedure Create_Stock
     (Stock   : in out Root_Stock_Type'Class;
      Maximum : Concorde.Quantities.Quantity)
   is
   begin
      Stock.Maximum := Maximum;
      Stock.Vector.Clear;
   end Create_Stock;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Commodity_Type is
   begin
      return Db.Get (Name);
   exception
      when others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "unknown commodity: " & Name);
         raise;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Class : Commodity_Class) return Array_Of_Commodities is
      Result : Array_Of_Commodities (1 .. Db.Upper_Bound);
      Count  : Natural := 0;

      function Match (Commodity : Commodity_Type) return Boolean
      is (Commodity.Class = Class);

      procedure Add (Commodity : Commodity_Type);

      ---------
      -- Add --
      ---------

      procedure Add (Commodity : Commodity_Type) is
      begin
         Count := Count + 1;
         Result (Count) := Commodity;
      end Add;

   begin
      Db.Scan (Match'Access, Add'Access);
      return Result (1 .. Count);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Class   : Commodity_Class;
                 Quality : Commodity_Quality)
                 return Array_Of_Commodities
   is
      Result : Array_Of_Commodities := Get (Class);
      Count  : Natural := 0;
   begin
      for I in Result'Range loop
         if Result (I).Quality = Quality then
            Count := Count + 1;
            Result (Count) := Result (I);
         end if;
      end loop;
      return Result (1 .. Count);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Flag : Commodity_Flag) return Array_Of_Commodities is
      Result : Array_Of_Commodities (1 .. Db.Upper_Bound);
      Count  : Natural := 0;

      function Match (Commodity : Commodity_Type) return Boolean
      is (Commodity.Is_Set (Flag));

      procedure Add (Commodity : Commodity_Type);

      ---------
      -- Add --
      ---------

      procedure Add (Commodity : Commodity_Type) is
      begin
         Count := Count + 1;
         Result (Count) := Commodity;
      end Add;

   begin
      Db.Scan (Match'Access, Add'Access);
      return Result (1 .. Count);
   end Get;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Commodity : Root_Commodity_Type'Class;
      Flag      : Commodity_Flag)
      return Boolean
   is
   begin
      return Commodity.Flags (Flag);
   end Is_Set;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Commodity_Type)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   -------------
   -- Quality --
   -------------

   function Quality
     (Commodity : Root_Commodity_Type'Class)
      return Commodity_Quality
   is
   begin
      return Commodity.Quality;
   end Quality;

   ---------------------
   -- Remove_Quantity --
   ---------------------

   procedure Remove_Quantity
     (Stock    : in out Stock_Interface'Class;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity;
      Earn     : Concorde.Money.Money_Type)
   is
      use type Concorde.Money.Money_Type;
   begin
      Stock.Set_Quantity
        (Item,
         Stock.Get_Quantity (Item) - Quantity,
         Money.Max (Stock.Get_Value (Item) - Earn, Money.Zero));
   end Remove_Quantity;

   ---------------------
   -- Remove_Quantity --
   ---------------------

   procedure Remove_Quantity
     (Stock    : in out Stock_Interface'Class;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity)
   is
      use type Concorde.Money.Money_Type;
      New_Quantity : constant Concorde.Quantities.Quantity :=
                       Stock.Get_Quantity (Item) - Quantity;
      New_Value : constant Money.Money_Type :=
                    Money.Total (Stock.Get_Average_Price (Item), New_Quantity);
   begin
      Stock.Set_Quantity
        (Item, New_Quantity, New_Value);
   end Remove_Quantity;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (Stock    : in out Root_Stock_Type;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity;
      Value    : Concorde.Money.Money_Type)
   is
   begin
      Stock.Vector.Replace_Element (Item.Reference, (Quantity, Value));
   end Set_Quantity;

   ----------------
   -- Total_Mass --
   ----------------

   function Total_Mass
     (Stock    : Stock_Interface'Class)
      return Non_Negative_Real
   is
      Result : Non_Negative_Real := 0.0;

      procedure Update (Commodity : Commodity_Type);

      ------------
      -- Update --
      ------------

      procedure Update (Commodity : Commodity_Type) is
      begin
         Result := Result
           + Quantities.To_Real (Stock.Get_Quantity (Commodity))
           * Commodity.Mass;
      end Update;

   begin
      Db.Scan (Update'Access);
      return Result;
   end Total_Mass;

   --------------------
   -- Total_Quantity --
   --------------------

   function Total_Quantity
     (Stock    : Stock_Interface'Class)
      return Concorde.Quantities.Quantity
   is
      Result : Quantities.Quantity := Quantities.Zero;

      procedure Update (Commodity : Commodity_Type);

      ------------
      -- Update --
      ------------

      procedure Update (Commodity : Commodity_Type) is
      begin
         Result := Result + Stock.Get_Quantity (Commodity);
      end Update;

   begin
      Db.Scan (Update'Access);
      return Result;
   end Total_Quantity;

   -----------------
   -- Total_Value --
   -----------------

   function Total_Value
     (Stock    : in out Stock_Interface'Class)
      return Concorde.Money.Money_Type
   is
      Result : Money.Money_Type := Money.Zero;

      procedure Update (Commodity : Commodity_Type);

      ------------
      -- Update --
      ------------

      procedure Update (Commodity : Commodity_Type) is
         use type Concorde.Money.Money_Type;
      begin
         Result := Result + Stock.Get_Value (Commodity);
      end Update;

   begin
      Db.Scan (Update'Access);
      return Result;
   end Total_Value;

   ---------------
   -- Unit_Mass --
   ---------------

   function Unit_Mass
     (Commodity : Root_Commodity_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Commodity.Mass;
   end Unit_Mass;

end Concorde.Commodities;
