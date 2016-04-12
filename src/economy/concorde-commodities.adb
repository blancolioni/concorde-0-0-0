with Concorde.Commodities.Db;

package body Concorde.Commodities is

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
      Count  : Natural;

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

end Concorde.Commodities;
