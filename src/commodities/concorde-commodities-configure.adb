with Ada.Characters.Handling;

with Tropos.Reader;

with Concorde.Paths;

package body Concorde.Commodities.Configure is

   procedure Configure_Commodity
     (Config : Tropos.Configuration);

   function Create
     (Tag        : String;
      Class      : Commodity_Class;
      Mass       : Non_Negative_Real;
      Base_Price : WL.Money.Price_Type;
      Quality    : Commodity_Quality;
      Energy     : Non_Negative_Real;
      Flags      : Array_Of_Flags)
      return Commodity_Type;

   ---------------------------
   -- Calculate_Base_Prices --
   ---------------------------

   procedure Calculate_Base_Prices is
      use WL.Money;
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
            if Facility.Simple_Input (I) then
               declare
                  Input : constant Commodity_Type :=
                            Facility.Input_Commodity (I);
                  Quant : constant WL.Quantities.Quantity_Type :=
                            Facility.Input_Quantity (I);
               begin
                  if Not_Priced (Input.all) then
                     Has_Price := False;
                     exit;
                  else
                     Cost := Cost
                       + Real
                       (To_Float (Input.Base_Price)
                        * WL.Quantities.To_Float (Quant));
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
                        Quant : constant WL.Quantities.Quantity_Type :=
                                  Facility.Input_Choice_Quantity (I, J);
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
                       + Real (To_Float (Lowest));
                  else
                     Has_Price := False;
                     exit;
                  end if;
               end;
            end if;
         end loop;

         if Has_Price then
            Cost := Cost * Real (Facility.Capacity);

            for I in 1 .. Facility.Worker_Count loop
               Cost := Cost
                 + Real (To_Float (Facility.Worker_Skill (I).Base_Pay)
                         * WL.Quantities.To_Float
                           (Facility.Worker_Quantity (I)));
            end loop;

            Cost := Cost / Real (Facility.Capacity) * 1.1;

            Commodity.Base_Price := To_Price (Float (Cost));

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
                              Class      =>
                                Commodity_Class'Value (Config.Get ("class")),
                              Mass       =>
                                Non_Negative_Real
                                  (Float'(Config.Get ("mass"))) * 1000.0,
                              Base_Price =>
                                WL.Money.Value
                                  (Config.Get ("base_price", "0")),
                              Quality    =>
                                Commodity_Quality'Val
                                  (Config.Get ("quality", 2) - 1),
                              Energy     =>
                                Non_Negative_Real
                                  (Float'(Config.Get ("energy", 0.0))),
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
      Class      : Commodity_Class;
      Mass       : Non_Negative_Real;
      Base_Price : WL.Money.Price_Type;
      Quality    : Commodity_Quality;
      Energy     : Non_Negative_Real;
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
         Commodity.Energy := Energy;
         Commodity.Quality := Quality;
      end Create;

   begin
      return Commodity : constant Commodity_Type :=
        Concorde.Commodities.Db.Create (Create'Access)
      do
         Commodity_Vector.Append (Commodity);
      end return;
   end Create;

   -------------------------
   -- Create_From_Service --
   -------------------------

   procedure Create_From_Service
     (Service_Facility : Concorde.Facilities.Facility_Type)
   is
      Service : constant Commodity_Type :=
                  Create
                    (Tag        => Service_Facility.Local_Tag,
                     Class      => Concorde.Commodities.Service,
                     Mass       => 0.0,
                     Base_Price => Service_Facility.Base_Service_Charge,
                     Quality    => Service_Facility.Quality,
                     Energy     => 0.0,
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
      Base_Pay : WL.Money.Price_Type)
      return Commodity_Type
   is
   begin
      return Create
        (Tag        => Tag,
         Class      => Concorde.Commodities.Skill,
         Mass       => 0.0,
         Base_Price => Base_Pay,
         Quality    => Middle,
         Energy     => 0.0,
         Flags      => (Virtual => True, others => False));
   end Create_From_Skill;

end Concorde.Commodities.Configure;
