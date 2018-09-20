with Ada.Characters.Handling;

with Tropos.Reader;

with Concorde.Configure;

package body Concorde.Commodities.Configure is

   procedure Configure_Commodity
     (Config : Tropos.Configuration);

   function Create
     (Tag        : String;
      Class      : Commodity_Class;
      Mass       : Non_Negative_Real;
      Base_Price : Concorde.Money.Price_Type;
      Flags      : Array_Of_Flags)
      return Commodity_Type;

   procedure Configure_Daily
     (Vector : in out Commodity_Daily_Vectors.Vector;
      Config : Tropos.Configuration);

   ---------------------------
   -- Calculate_Base_Prices --
   ---------------------------

   procedure Calculate_Base_Prices is
      use Concorde.Money;
      Finished : Boolean := False;

      function Not_Priced
        (Commodity : Root_Commodity_Type'Class)
         return Boolean
      is (Commodity.Base_Price = Zero);

      procedure Check_Price
        (Commodity : in out Root_Commodity_Type'Class);

      -----------------
      -- Check_Price --
      -----------------

      procedure Check_Price
        (Commodity : in out Root_Commodity_Type'Class)
      is
         use Concorde.Facilities;
         Fs        : constant Array_Of_Facilities :=
                       Get_By_Production (Commodity);
         pragma Assert (Fs'Length >= 1,
                        Commodity.Name
                        & " has no base price and cannot be produced");
         Facility  : constant Facility_Type := Fs (Fs'First);
         Size      : constant Concorde.Quantities.Quantity_Type :=
                       Concorde.Quantities.To_Quantity (1000.0);
         Has_Price : Boolean := True;
         Cost      : Real := 0.0;
      begin
         for I in 1 .. Facility.Input_Count loop
            if Facility.Simple_Input (I) then
               declare
                  Input : constant Commodity_Type :=
                            Facility.Input_Commodity (I);
                  Quant : constant Concorde.Quantities.Quantity_Type :=
                            Facility.Input_Quantity (Size, I);
               begin
                  if Not_Priced (Input.all) then
                     Has_Price := False;
                     exit;
                  else
                     Cost := Cost
                       + Real
                       (To_Real (Input.Base_Price)
                        * Concorde.Quantities.To_Real (Quant));
                  end if;
               end;
            else
               declare
                  Lowest : Money_Type := Zero;
                  Found  : Boolean := False;
               begin
                  for J in 1 .. Facility.Input_Choice_Count (I) loop
                     declare
                        Input : constant Commodity_Type :=
                                  Facility.Input_Choice_Commodity (I, J);
                        Quant : constant Concorde.Quantities.Quantity_Type :=
                                  Facility.Input_Choice_Quantity (Size, I, J);
                        This  : Money_Type;
                     begin
                        if Not_Priced (Input.all) then
                           null;
                        elsif not Found then
                           Lowest := Total (Input.Base_Price, Quant);
                           Found := True;
                        else
                           This := Total (Input.Base_Price, Quant);
                           if This < Lowest then
                              Lowest := This;
                           end if;
                        end if;
                     end;
                  end loop;

                  if Found then
                     Cost := Cost
                       + Real (To_Real (Lowest));
                  else
                     Has_Price := False;
                     exit;
                  end if;
               end;
            end if;
         end loop;

         if Has_Price then
            Commodity.Base_Price :=
              To_Price (Cost * 2.0 / Concorde.Quantities.To_Real (Size));
         else
            Finished := False;
         end if;

      end Check_Price;

   begin
      while not Finished loop
         Finished := True;

         Concorde.Commodities.Db.Iterate
           (Not_Priced'Access, Check_Price'Access);
      end loop;
   end Calculate_Base_Prices;

   ---------------------------
   -- Configure_Commodities --
   ---------------------------

   procedure Configure_Commodities is
   begin
      Tropos.Reader.Read_Config
        (Path      => Concorde.Configure.Directory_Path ("commodities"),
         Extension => "commodity",
         Configure => Configure_Commodity'Access);
      Configure_Daily
        (Vector => Daily_Pop,
         Config =>
           Tropos.Reader.Read_Config
             (Concorde.Configure.File_Path
                  (Directory => "init",
                   File_Name => "pop-needs",
                   Extension => "txt")));
   end Configure_Commodities;

   -------------------------
   -- Configure_Commodity --
   -------------------------

   procedure Configure_Commodity
     (Config : Tropos.Configuration)
   is
      Flags : Array_Of_Flags;

   begin

      for Flag in Flags'Range loop
         Flags (Flag) :=
           Config.Get
             (Ada.Characters.Handling.To_Lower
                (Commodity_Flag'Image (Flag)));
      end loop;

      Flags (Available) := not Config.Contains ("unavailable");

      declare
         New_Commodity : constant Commodity_Type :=
                           Create
                             (Tag        => Config.Config_Name,
                              Class      =>
                                Commodity_Class'Value
                                  (Config.Get ("class", "consumer")),
                              Mass       =>
                                1000.0 *
                                  Non_Negative_Real
                                (Float'(Config.Get ("unit_mass", 1.0))),
                              Base_Price =>
                                Concorde.Money.Value
                                  (Config.Get ("base_price", "0")),
                              Flags      => Flags);
         pragma Unreferenced (New_Commodity);
      begin
         null;
      end;

   end Configure_Commodity;

   ---------------------
   -- Configure_Daily --
   ---------------------

   procedure Configure_Daily
     (Vector : in out Commodity_Daily_Vectors.Vector;
      Config : Tropos.Configuration)
   is
   begin
      for Item_Config of Config loop
         declare
            Commodity : constant Commodity_Type :=
                          Get (Item_Config.Config_Name);
         begin
            Vector.Replace_Element
              (Commodity,
               (Real (Float'(Item_Config.Get ("need"))),
                Real (Float'(Item_Config.Get ("budget")))));
         end;
      end loop;
   end Configure_Daily;

   ---------------------
   -- Configure_Stock --
   ---------------------

   procedure Configure_Stock
     (From_Config : Tropos.Configuration;
      Stock       : in out Stock_Interface'Class;
      Factor      : Non_Negative_Real := 1.0)
   is
   begin
      for Config of From_Config loop
         declare
            use Concorde.Quantities, Concorde.Money;
            Item : constant Commodity_Type := Get (Config.Config_Name);
            Quantity : constant Float :=
                         (if Config.Contains ("quantity")
                          then Config.Get ("quantity")
                          else Config.Value);
            Value    : constant Non_Negative_Real :=
                         (if Config.Contains ("value")
                          then Real (Float'(Config.Get ("value")))
                          else To_Real
                            (Adjust_Price
                               (Item.Base_Price,
                                Real (Quantity))));
         begin
            Stock.Add_Quantity
              (Item     => Item,
               Quantity => To_Quantity (Real (Quantity) * Factor),
               Value    => To_Money (Value * Factor));
         end;
      end loop;
   end Configure_Stock;

   ------------
   -- Create --
   ------------

   function Create
     (Tag        : String;
      Class      : Commodity_Class;
      Mass       : Non_Negative_Real;
      Base_Price : Concorde.Money.Price_Type;
      Flags      : Array_Of_Flags)
     return Commodity_Type
   is

      procedure Create (Commodity : in out Root_Commodity_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Commodity : in out Root_Commodity_Type'Class) is
      begin
         Commodity.Set_Local_Tag (Tag);
         Commodity.Class := Class;
         Commodity.Flags := Flags;
         Commodity.Mass := Mass;
         Commodity.Base_Price := Base_Price;
      end Create;

   begin
      return Commodity : constant Commodity_Type :=
        Concorde.Commodities.Db.Create (Create'Access)
      do
         Commodity_Vector.Append (Commodity);
      end return;
   end Create;

   -------------------
   -- New_Pop_Group --
   -------------------

   function New_Pop_Group
     (Identifier : String;
      Base_Price : Concorde.Money.Price_Type)
      return Commodity_Type
   is

      procedure Create (Commodity : in out Root_Commodity_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Commodity : in out Root_Commodity_Type'Class) is
      begin
         Commodity.Set_Local_Tag (Identifier);
         Commodity.Class := Pop_Group;
         Commodity.Flags := (others => False);
         Commodity.Mass := 60.0;
         Commodity.Base_Price := Base_Price;
      end Create;

   begin
      return Commodity : constant Commodity_Type :=
        Concorde.Commodities.Db.Create (Create'Access)
      do
         Commodity_Vector.Append (Commodity);
      end return;
   end New_Pop_Group;

end Concorde.Commodities.Configure;
