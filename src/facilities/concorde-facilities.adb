package body Concorde.Facilities is

   Local_Colony_Hub : Facility_Type := null;

   Local_Facility_Array : access Array_Of_Facilities;

   function Get_By_Match
     (Match : not null access
        function (Facility : Facility_Type) return Boolean)
      return Array_Of_Facilities;

   --------------------
   -- All_Facilities --
   --------------------

   function All_Facilities return Array_Of_Facilities is
   begin
      if Local_Facility_Array = null then
         Local_Facility_Array :=
           new Array_Of_Facilities (1 .. Db.Active_Count);
         declare
            Count : Natural := 0;
            procedure Add (Facility : Facility_Type);

            ---------
            -- Add --
            ---------

            procedure Add (Facility : Facility_Type) is
            begin
               Count := Count + 1;
               Local_Facility_Array (Count) := Facility;
            end Add;
         begin
            Db.Scan (Add'Access);
         end;
      end if;
      return Local_Facility_Array.all;
   end All_Facilities;

   -------------------------
   -- Base_Service_Charge --
   -------------------------

   function Base_Service_Charge
     (Facility : Root_Facility_Type'Class)
      return WL.Money.Price_Type
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

   ----------------
   -- Colony_Hub --
   ----------------

   function Colony_Hub return Facility_Type is
   begin
      if Local_Colony_Hub = null then
         declare
            Arr : constant Array_Of_Facilities :=
                    Get_By_Class (Colony_Hub);
         begin
            Local_Colony_Hub := Arr (Arr'First);
         end;
      end if;
      return Local_Colony_Hub;
   end Colony_Hub;

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

   -----------------------
   -- Get_By_Production --
   -----------------------

   function Get_By_Production
     (Output : Concorde.Commodities.Root_Commodity_Type'Class)
      return Array_Of_Facilities
   is
      function Match (Facility : Facility_Type) return Boolean
      is (Facility.Has_Output
          and then Facility.Output.Identifier = Output.Identifier);

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
      return Memor.Memor_Database
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

   ------------------------
   -- Resource_Generator --
   ------------------------

   function Resource_Generator
     (Resource : Concorde.Commodities.Commodity_Type)
      return Facility_Type
   is
      use type Concorde.Commodities.Commodity_Type;

      function Match (Facility : Facility_Type) return Boolean
      is (Facility.Class = Resource_Generator
          and then Facility.Can_Produce (Resource));

      Matches : constant Array_Of_Facilities :=
                  Get_By_Match (Match'Access);
   begin
      return Matches (Matches'First);
   end Resource_Generator;

end Concorde.Facilities;
