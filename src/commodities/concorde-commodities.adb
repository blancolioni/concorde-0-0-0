with Ada.Text_IO;

package body Concorde.Commodities is

   Local_Commodity_Array         : access Array_Of_Commodities;
   Local_Trade_Commodity_Array   : access Array_Of_Commodities;
   Local_Virtual_Commodity_Array : access Array_Of_Commodities;

   function Commodity_Array
     (Test : not null access
        function (Commodity : Commodity_Type) return Boolean)
      return Array_Of_Commodities;

   ---------
   -- Add --
   ---------

   procedure Add
     (To   : in out Stock_Interface'Class;
      From : Stock_Interface'Class)
   is
      use Concorde.Quantities;
   begin
      for Item of Commodity_Vector loop
         if From.Get_Quantity (Item) > Zero then
            To.Add_Quantity
              (Item     => Item,
               Quantity => From.Get_Quantity (Item),
               Value    => From.Get_Value (Item));
         end if;
      end loop;
   end Add;

   ------------------
   -- Add_Quantity --
   ------------------

   procedure Add_Quantity
     (Stock    : in out Stock_Interface'Class;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type)
   is
      use type Concorde.Money.Money_Type;

   begin
      Stock.Set_Quantity
        (Item,
         Stock.Get_Quantity (Item) + Quantity,
         Stock.Get_Value (Item) + Value);
   end Add_Quantity;

   ---------------------
   -- All_Commodities --
   ---------------------

   function All_Commodities return Array_Of_Commodities is
   begin
      if Local_Commodity_Array = null then
         Local_Commodity_Array :=
           new Array_Of_Commodities (1 .. Commodity_Vector.Last_Index);
         for I in Local_Commodity_Array'Range loop
            Local_Commodity_Array (I) := Commodity_Vector (I);
         end loop;
      end if;

      return Local_Commodity_Array.all;
   end All_Commodities;

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

   -------------------------
   -- Clear_Flagged_Stock --
   -------------------------

   procedure Clear_Flagged_Stock
     (Stock : in out Stock_Interface'Class;
      Flag  : Commodity_Flag)
   is
   begin
      for Commodity of Get (Flag) loop
         Stock.Set_Quantity (Commodity, Concorde.Quantities.Zero,
                             Concorde.Money.Zero);
      end loop;
   end Clear_Flagged_Stock;

   -----------------
   -- Clear_Stock --
   -----------------

   overriding procedure Clear_Stock
     (Stock : in out Root_Stock_Type)
   is
   begin
      Stock.Vector.Clear;
   end Clear_Stock;

   -----------------
   -- Clear_Stock --
   -----------------

   procedure Clear_Stock
     (Stock : in out Stock_Interface'Class;
      Class : Commodity_Class)
   is
   begin
      for Commodity of Get (Class) loop
         Stock.Set_Quantity (Commodity, Concorde.Quantities.Zero,
                             Concorde.Money.Zero);
      end loop;
   end Clear_Stock;

   ---------------------
   -- Commodity_Array --
   ---------------------

   function Commodity_Array
     (Test : not null access
        function (Commodity : Commodity_Type) return Boolean)
      return Array_Of_Commodities
   is
      Cs   : Array_Of_Commodities := All_Commodities;
      Back : Natural := 0;
   begin
      for I in Cs'Range loop
         if Test (Cs (I)) then
            if Back > 0 then
               Cs (I - Back) := Cs (I);
            end if;
         else
            Back := Back + 1;
         end if;
      end loop;

      return Cs (1 .. Cs'Last - Back);
   end Commodity_Array;

   ------------------
   -- Create_Stock --
   ------------------

   procedure Create_Stock
     (Stock   : in out Root_Stock_Type'Class;
      Maximum : Concorde.Quantities.Quantity_Type;
      Virtual : Boolean)
   is
   begin
      Stock.Maximum := Maximum;
      Stock.Vector.Clear;
      Stock.Virtual := Virtual;
   end Create_Stock;

   --------------------------
   -- Create_Virtual_Stock --
   --------------------------

   procedure Create_Virtual_Stock
     (Stock   : in out Root_Stock_Type'Class)
   is
   begin
      Stock.Create_Stock (Concorde.Quantities.Zero, True);
   end Create_Virtual_Stock;

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

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Stock : in out Root_Stock_Type) is
   begin
      Stock.Virtual := False;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Stock : in out Virtual_Stock_Type) is
   begin
      Stock.Virtual := True;
   end Initialize;

   ------------------
   -- Is_Pop_Group --
   ------------------

   function Is_Pop_Group
     (Commodity : Root_Commodity_Type'Class)
      return Boolean
   is
   begin
      return Commodity.Class = Pop_Group;
   end Is_Pop_Group;

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

   --------------------
   -- No_Commodities --
   --------------------

   function No_Commodities return Array_Of_Commodities is
   begin
      return Result : Array_Of_Commodities (1 .. 0);
   end No_Commodities;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Commodity_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   function Pop_Daily_Desires
     (Commodity : Commodity_Type)
      return Non_Negative_Real
   is
   begin
      return Daily_Pop.Element (Commodity).Desires;
   end Pop_Daily_Desires;

   ---------------------
   -- Pop_Daily_Needs --
   ---------------------

   function Pop_Daily_Needs
     (Commodity : Commodity_Type)
      return Non_Negative_Real
   is
   begin
      return Daily_Pop.Element (Commodity).Needs;
   end Pop_Daily_Needs;

   --------------------
   -- Pop_Max_Budget --
   --------------------

   function Pop_Max_Budget
     (Commodity : Commodity_Type)
      return Unit_Real
   is
   begin
      return Daily_Pop.Element (Commodity).Budget;
   end Pop_Max_Budget;

   ---------------------
   -- Remove_Quantity --
   ---------------------

   procedure Remove_Quantity
     (Stock    : in out Stock_Interface'Class;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity_Type;
      Earn     : Concorde.Money.Money_Type)
   is
      use type Concorde.Money.Money_Type;
   begin
      Stock.Set_Quantity
        (Item,
         Stock.Get_Quantity (Item) - Quantity,
         Concorde.Money.Max
           (Stock.Get_Value (Item) - Earn,
           Concorde.Money.Zero));
   end Remove_Quantity;

   ---------------------
   -- Remove_Quantity --
   ---------------------

   procedure Remove_Quantity
     (Stock    : in out Stock_Interface'Class;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity_Type)
   is
      New_Quantity : constant Concorde.Quantities.Quantity_Type :=
                       Stock.Get_Quantity (Item) - Quantity;
      New_Value : constant Concorde.Money.Money_Type :=
                    Concorde.Money.Total
                      (Stock.Get_Average_Price (Item), New_Quantity);
   begin
      Stock.Set_Quantity
        (Item, New_Quantity, New_Value);
   end Remove_Quantity;

   ----------
   -- Scan --
   ----------

   procedure Scan (Process : not null access
                     procedure (Commodity : Commodity_Type))
   is
   begin
      for Commodity of Commodity_Vector loop
         Process (Commodity);
      end loop;
   end Scan;

   ----------------
   -- Scan_Stock --
   ----------------

   overriding procedure Scan_Stock
     (Stock   : Root_Stock_Type;
      Process : not null access
        procedure (Commodity : Commodity_Type))
   is
      procedure Process_Stock
        (Commodity : not null access constant Root_Commodity_Type'Class;
         Info      : Stock_Entry);

      -------------------
      -- Process_Stock --
      -------------------

      procedure Process_Stock
        (Commodity : not null access constant Root_Commodity_Type'Class;
         Info      : Stock_Entry)
      is
         use Concorde.Quantities;
      begin
         if (Stock.Virtual
             or else not Commodity.Flags (Virtual))
           and then Info.Quantity > Zero
         then
            Process (Commodity_Type (Commodity));
         end if;
      end Process_Stock;

   begin
      Stock.Vector.Scan (Process_Stock'Access);
   end Scan_Stock;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (Stock    : in out Root_Stock_Type;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type)
   is
   begin
      Stock.Vector.Replace_Element (Item, (Quantity, Value));
   end Set_Quantity;

   --------------------------
   -- Total_Concrete_Value --
   --------------------------

   function Total_Concrete_Value
     (Stock    : in out Stock_Interface'Class)
      return Concorde.Money.Money_Type
   is
      Result : Concorde.Money.Money_Type := Concorde.Money.Zero;

      procedure Update (Commodity : Commodity_Type);

      ------------
      -- Update --
      ------------

      procedure Update (Commodity : Commodity_Type) is
         use type Concorde.Money.Money_Type;
      begin
         if not Commodity.Is_Set (Virtual) then
            Result := Result + Stock.Get_Value (Commodity);
         end if;
      end Update;

   begin
      Db.Scan (Update'Access);
      return Result;
   end Total_Concrete_Value;

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
           + Concorde.Quantities.To_Real (Stock.Get_Quantity (Commodity))
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
      return Concorde.Quantities.Quantity_Type
   is
      Result : Concorde.Quantities.Quantity_Type := Concorde.Quantities.Zero;

      procedure Update (Commodity : Commodity_Type);

      ------------
      -- Update --
      ------------

      procedure Update (Commodity : Commodity_Type) is
      begin
         if not Commodity.Is_Set (Virtual)
           and then not Commodity.Is_Pop_Group
         then
            Result := Result + Stock.Get_Quantity (Commodity);
         end if;
      end Update;

   begin
      Db.Scan (Update'Access);
      return Result;
   end Total_Quantity;

   -------------------------
   -- Total_Virtual_Value --
   -------------------------

   function Total_Virtual_Value
     (Stock    : in out Stock_Interface'Class)
      return Concorde.Money.Money_Type
   is
      Result : Concorde.Money.Money_Type := Concorde.Money.Zero;

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
   end Total_Virtual_Value;

   -----------------------
   -- Trade_Commodities --
   -----------------------

   function Trade_Commodities return Array_Of_Commodities is
   begin
      if Local_Trade_Commodity_Array = null then
         declare
            function Test (Commodity : Commodity_Type) return Boolean
            is (not Commodity.Flags (Virtual));
         begin
            Local_Trade_Commodity_Array :=
              new Array_Of_Commodities'(Commodity_Array (Test'Access));
         end;
      end if;

      return Local_Trade_Commodity_Array.all;
   end Trade_Commodities;

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

   -------------------------
   -- Virtual_Commodities --
   -------------------------

   function Virtual_Commodities return Array_Of_Commodities is
   begin
      if Local_Virtual_Commodity_Array = null then
         declare
            function Test (Commodity : Commodity_Type) return Boolean
            is (Commodity.Flags (Virtual));
         begin
            Local_Virtual_Commodity_Array :=
              new Array_Of_Commodities'(Commodity_Array (Test'Access));
         end;
      end if;

      return Local_Virtual_Commodity_Array.all;
   end Virtual_Commodities;

end Concorde.Commodities;
