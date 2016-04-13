with Concorde.Facilities.Db;

package body Concorde.Facilities is

   function Get_By_Match
     (Match : not null access
        function (Facility : Facility_Type) return Boolean)
      return Array_Of_Facilities;

   -------------------------
   -- Base_Service_Charge --
   -------------------------

   function Base_Service_Charge
     (Facility : Root_Facility_Type'Class)
      return Concorde.Money.Price_Type
   is
   begin
      return Facility.Base_Service_Charge;
   end Base_Service_Charge;

   -----------------
   -- Can_Produce --
   -----------------

   function Can_Produce
     (Facility  : Root_Facility_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Boolean
   is
   begin
      for Flag in Facility.Commodity_Flags'Range loop
         if Facility.Commodity_Flags (Flag)
           and then Commodity.Is_Set (Flag)
         then
            return True;
         end if;
      end loop;
      return False;
   end Can_Produce;

   --------------
   -- Capacity --
   --------------

   function Capacity
     (Facility : Root_Facility_Type'Class)
      return Facility_Capacity
   is
   begin
      return Facility.Capacity;
   end Capacity;

   -----------
   -- Class --
   -----------

   function Class
     (Facility : Root_Facility_Type'Class)
      return Facility_Class
   is
   begin
      return Facility.Class;
   end Class;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Facility_Type is
   begin
      return Db.Get (Name);
   end Get;

   ------------------
   -- Get_By_Class --
   ------------------

   function Get_By_Class
     (Class : Facility_Class)
      return Array_Of_Facilities
   is
      function Match (Facility : Facility_Type) return Boolean
      is (Facility.Class = Class);

   begin
      return Get_By_Match (Match'Access);
   end Get_By_Class;

   ------------------
   -- Get_By_Match --
   ------------------

   function Get_By_Match
     (Match : not null access
        function (Facility : Facility_Type) return Boolean)
      return Array_Of_Facilities
   is
      use type Concorde.Commodities.Commodity_Type;

      Result : Array_Of_Facilities (1 .. Db.Upper_Bound);
      Count  : Natural := 0;

      procedure Update (Facility : Facility_Type);

      ------------
      -- Update --
      ------------

      procedure Update (Facility : Facility_Type) is
      begin
         Count := Count + 1;
         Result (Count) := Facility;
      end Update;

   begin
      Db.Scan (Match, Update'Access);
      return Result (1 .. Count);
   end Get_By_Match;

   -----------------------
   -- Get_By_Production --
   -----------------------

   function Get_By_Production
     (Output : Concorde.Commodities.Commodity_Type)
      return Array_Of_Facilities
   is
      use type Concorde.Commodities.Commodity_Type;

      function Match (Facility : Facility_Type) return Boolean
      is (Facility.Has_Output and then Facility.Output = Output);

   begin
      return Get_By_Match (Match'Access);
   end Get_By_Production;

   ----------------
   -- Has_Output --
   ----------------

   function Has_Output
     (Facility : Root_Facility_Type'Class)
      return Boolean
   is
      use type Concorde.Commodities.Commodity_Type;
   begin
      return Facility.Output /= null;
   end Has_Output;

   ---------------------
   -- Input_Commodity --
   ---------------------

   function Input_Commodity
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Concorde.Commodities.Commodity_Type
   is
   begin
      return Facility.Inputs (Index).Commodity;
   end Input_Commodity;

   -----------------
   -- Input_Count --
   -----------------

   function Input_Count
     (Facility : Root_Facility_Type'Class)
      return Natural
   is
   begin
      if Facility.Inputs = null then
         return 0;
      else
         return Facility.Inputs'Length;
      end if;
   end Input_Count;

   --------------------
   -- Input_Quantity --
   --------------------

   function Input_Quantity
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Concorde.Quantities.Quantity
   is
   begin
      return Facility.Inputs (Index).Quantity;
   end Input_Quantity;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Facility : Root_Facility_Type'Class;
      Flag     : Facility_Flag)
      return Boolean
   is
   begin
      return Facility.Flags (Flag);
   end Is_Set;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Facility_Type)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   ------------
   -- Output --
   ------------

   function Output
     (Facility : Root_Facility_Type'Class)
      return Concorde.Commodities.Commodity_Type
   is
   begin
      return Facility.Output;
   end Output;

   -------------
   -- Quality --
   -------------

   function Quality
     (Facility : Root_Facility_Type'Class)
      return Concorde.Commodities.Commodity_Quality
   is
   begin
      return Facility.Quality;
   end Quality;

   ------------------------
   -- Resource_Generator --
   ------------------------

   function Resource_Generator
     (Resource : Concorde.Commodities.Commodity_Type)
      return Facility_Type
   is
      use type Concorde.Commodities.Commodity_Type;

      function Match (Facility : Root_Facility_Type'Class) return Boolean
      is (Facility.Class = Resource_Generator
          and then Facility.Output = Resource);

   begin
      return Db.Reference (Db.Search (Match'Access));
   end Resource_Generator;

end Concorde.Facilities;
