with Concorde.Commodities.Db;

package body Concorde.Commodities is

   ------------------
   -- Add_Quantity --
   ------------------

   procedure Add_Quantity
     (Stock    : in out Stock_Interface'Class;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity)
   is
      use type Concorde.Quantities.Quantity;
   begin
      Stock.Set_Quantity (Item, Stock.Get_Quantity (Item) + Quantity);
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

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Commodity_Type is
   begin
      return Db.Get (Name);
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
      Quantity : Concorde.Quantities.Quantity)
   is
      use type Concorde.Quantities.Quantity;
   begin
      Stock.Set_Quantity (Item, Stock.Get_Quantity (Item) - Quantity);
   end Remove_Quantity;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (Stock    : in out Root_Stock_Type;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity)
   is
   begin
      Stock.Vector.Replace_Element (Item.Reference, Quantity);
   end Set_Quantity;

   --------------------
   -- Total_Quantity --
   --------------------

   function Total_Quantity
     (Stock    : in out Stock_Interface'Class)
      return Concorde.Quantities.Quantity
   is
      Result : Quantities.Quantity := Quantities.Zero;

      procedure Update (Commodity : Commodity_Type);

      ------------
      -- Update --
      ------------

      procedure Update (Commodity : Commodity_Type) is
         use type Concorde.Quantities.Quantity;
      begin
         Result := Result + Stock.Get_Quantity (Commodity);
      end Update;

   begin
      Db.Scan (Update'Access);
      return Result;
   end Total_Quantity;

end Concorde.Commodities;
