package body Concorde.Atmosphere is

   -----------------
   -- Abundance_E --
   -----------------

   function Abundance_E
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Gas.Abundance_E;
   end Abundance_E;

   -----------------
   -- Abundance_S --
   -----------------

   function Abundance_S
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Gas.Abundance_S;
   end Abundance_S;

   -------------------
   -- Boiling_Point --
   -------------------

   function Boiling_Point
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Gas.Boiling_Point;
   end Boiling_Point;

   -------------------------
   -- By_Molecular_Weight --
   -------------------------

   function By_Molecular_Weight return Array_Of_Gases is
      Result : Array_Of_Gases (1 .. Db.Upper_Bound);
      Count  : Natural := 0;

      procedure Add_Gas (Gas : Gas_Type);

      -------------
      -- Add_Gas --
      -------------

      procedure Add_Gas (Gas : Gas_Type) is
      begin
         Count := Count + 1;
         Result (Count) := Gas;
      end Add_Gas;

   begin
      Db.Scan (Add_Gas'Access);
      return Result;
   end By_Molecular_Weight;

   -------------
   -- Density --
   -------------

   function Density
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Gas.Density;
   end Density;

   -------------
   -- Formula --
   -------------

   function Formula
     (Gas : Root_Gas_Type'Class)
      return String
   is
   begin
      return Gas.Formula.all;
   end Formula;

   -------------
   -- Max_IPP --
   -------------

   function Max_IPP
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Gas.Max_IPP;
   end Max_IPP;

   -------------------
   -- Melting_Point --
   -------------------

   function Melting_Point
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Gas.Melting_Point;
   end Melting_Point;

   ----------------------
   -- Molecular_Weight --
   ----------------------

   function Molecular_Weight
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Gas.Molecular_Weight;
   end Molecular_Weight;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Gas : Root_Gas_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Gas);
   begin
      return Db.Get_Database;
   end Object_Database;

   ----------------
   -- Reactivity --
   ----------------

   function Reactivity
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Gas.Reactivity;
   end Reactivity;

end Concorde.Atmosphere;
