with Ada.Characters.Handling;

with Tropos.Reader;

with Concorde.Paths;

with Concorde.Commodities.Db;

package body Concorde.Commodities.Configure is

   procedure Configure_Commodity
     (Config : Tropos.Configuration);

   procedure Create
     (Tag        : String;
      Name       : String;
      Class      : Commodity_Class;
      Mass       : Non_Negative_Real;
      Base_Price : Concorde.Money.Price_Type;
      Quality    : Commodity_Quality;
      Flags      : Array_Of_Flags);

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

      Create
        (Tag        => Config.Config_Name,
         Name       => Config.Get ("name", Config.Config_Name),
         Class      => Commodity_Class'Value (Config.Get ("class")),
         Mass       => Non_Negative_Real (Float'(Config.Get ("mass"))),
         Base_Price =>
           Concorde.Money.Value (Config.Get ("base_price", "0")),
         Quality    =>
           Commodity_Quality'Val (Config.Get ("quality", 2) - 1),
         Flags      => Flags);
   end Configure_Commodity;

   ------------
   -- Create --
   ------------

   procedure Create
     (Tag        : String;
      Name       : String;
      Class      : Commodity_Class;
      Mass       : Non_Negative_Real;
      Base_Price : Concorde.Money.Price_Type;
      Quality    : Commodity_Quality;
      Flags      : Array_Of_Flags)
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
      Concorde.Commodities.Db.Create (Create'Access);
   end Create;

   -------------------------
   -- Create_From_Service --
   -------------------------

   procedure Create_From_Service
     (Service_Facility : Concorde.Facilities.Facility_Type)
   is
   begin
      Create
        (Tag        => Service_Facility.Identifier,
         Name       => Service_Facility.Name,
         Class      => Concorde.Commodities.Service,
         Mass       => 0.0,
         Base_Price => Service_Facility.Base_Service_Charge,
         Quality    => Service_Facility.Quality,
         Flags      => (Virtual => True, others => False));
   end Create_From_Service;

   -----------------------
   -- Create_From_Skill --
   -----------------------

   procedure Create_From_Skill
     (Skill : Concorde.People.Skills.Pop_Skill)
   is
   begin
      Create
        (Tag        => Skill.Identifier,
         Name       => Skill.Name,
         Class      => Concorde.Commodities.Skill,
         Mass       => 0.0,
         Base_Price => Skill.Base_Pay,
         Quality    => Middle,
         Flags      => (Virtual => True, others => False));
   end Create_From_Skill;

end Concorde.Commodities.Configure;
