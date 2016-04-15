with Ada.Characters.Handling;

with Tropos.Reader;

with Concorde.Paths;

with Concorde.Commodities.Db;

package body Concorde.Commodities.Configure is

   procedure Configure_Commodity
     (Config : Tropos.Configuration);

   function Create
     (Tag        : String;
      Name       : String;
      Class      : Commodity_Class;
      Mass       : Non_Negative_Real;
      Base_Price : Concorde.Money.Price_Type;
      Quality    : Commodity_Quality;
      Flags      : Array_Of_Flags)
      return Commodity_Type;

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
         Has_Price : Boolean := True;
         Cost      : Real := 0.0;
      begin
         for I in 1 .. Facility.Input_Count loop
            declare
               Input : constant Commodity_Type :=
                         Facility.Input_Commodity (I);
               Quant : constant Quantities.Quantity :=
                         Facility.Input_Quantity (I);
            begin
               if Not_Priced (Input.all) then
                  Has_Price := False;
                  exit;
               else
                  Cost := Cost
                    + To_Real (Input.Base_Price) * Quantities.To_Real (Quant);
               end if;
            end;
         end loop;

         if Has_Price then
            Cost := Cost * Real (Facility.Capacity);

            for I in 1 .. Facility.Worker_Count loop
               Cost := Cost
                 + To_Real (Facility.Worker_Skill (I).Base_Price)
                 * Quantities.To_Real (Facility.Worker_Quantity (I));
            end loop;

            Cost := Cost / Real (Facility.Capacity) * 1.1;

            Commodity.Base_Price := To_Price (Cost);

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
      Commodities_Config : constant Tropos.Configuration :=
                             Tropos.Reader.Read_Config
                               (Concorde.Paths.Config_File
                                  ("commodities/commodities.txt"));
   begin
      for Config of Commodities_Config loop
         Configure_Commodity (Config);
      end loop;
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

      declare
         New_Commodity : constant Commodity_Type :=
                           Create
                             (Tag        => Config.Config_Name,
                              Name       =>
                                Config.Get ("name", Config.Config_Name),
                              Class      =>
                                Commodity_Class'Value (Config.Get ("class")),
                              Mass       =>
                                Non_Negative_Real
                                  (Float'(Config.Get ("mass"))),
                              Base_Price =>
                                Concorde.Money.Value
                                  (Config.Get ("base_price", "0")),
                              Quality    =>
                                Commodity_Quality'Val
                                  (Config.Get ("quality", 2) - 1),
                              Flags      => Flags);
         pragma Unreferenced (New_Commodity);
      begin
         null;
      end;

   end Configure_Commodity;

   ------------
   -- Create --
   ------------

   function Create
     (Tag        : String;
      Name       : String;
      Class      : Commodity_Class;
      Mass       : Non_Negative_Real;
      Base_Price : Concorde.Money.Price_Type;
      Quality    : Commodity_Quality;
      Flags      : Array_Of_Flags)
     return Commodity_Type
   is

      procedure Create (Commodity : in out Root_Commodity_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Commodity : in out Root_Commodity_Type'Class) is
      begin
         Commodity.Tag := new String'(Tag);
         Commodity.Class := Class;
         Commodity.Set_Name (Name);
         Commodity.Flags := Flags;
         Commodity.Mass := Mass;
         Commodity.Base_Price := Base_Price;
         Commodity.Quality := Quality;
      end Create;

   begin
      return Concorde.Commodities.Db.Create (Create'Access);
   end Create;

   -------------------------
   -- Create_From_Service --
   -------------------------

   procedure Create_From_Service
     (Service_Facility : Concorde.Facilities.Facility_Type)
   is
      Service : constant Commodity_Type :=
                  Create
                    (Tag        => Service_Facility.Identifier,
                     Name       => Service_Facility.Name,
                     Class      => Concorde.Commodities.Service,
                     Mass       => 0.0,
                     Base_Price => Service_Facility.Base_Service_Charge,
                     Quality    => Service_Facility.Quality,
                     Flags      => (Virtual => True, others => False));
      pragma Unreferenced (Service);
   begin
      null;
   end Create_From_Service;

   -----------------------
   -- Create_From_Skill --
   -----------------------

   function Create_From_Skill
     (Tag      : String;
      Name     : String;
      Base_Pay : Concorde.Money.Price_Type)
      return Commodity_Type
   is
   begin
      return Create
        (Tag        => Tag,
         Name       => Name,
         Class      => Concorde.Commodities.Skill,
         Mass       => 0.0,
         Base_Price => Base_Pay,
         Quality    => Middle,
         Flags      => (Virtual => True, others => False));
   end Create_From_Skill;

end Concorde.Commodities.Configure;
