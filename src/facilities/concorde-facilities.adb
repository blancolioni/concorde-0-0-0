with Concorde.Facilities.Db;

package body Concorde.Facilities is

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

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Facility : Root_Facility_Type'Class;
      Flag     : Concorde.Commodities.Commodity_Flag)
      return Boolean
   is
   begin
      return Facility.Commodity_Flags (Flag);
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

end Concorde.Facilities;
