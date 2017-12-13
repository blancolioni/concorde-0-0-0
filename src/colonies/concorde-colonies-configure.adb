with Ada.Text_IO;

with WL.Random;

with Tropos.Reader;

with Memor.Element_Vectors;

with Concorde.Options;
with Concorde.Paths;

with Concorde.Locations;
with Concorde.Maps;

with WL.Money;
with WL.Quantities;

with Concorde.Commodities;
with Concorde.Factions;
with Concorde.Facilities;
with Concorde.Installations;

with Concorde.People.Groups;
with Concorde.People.Skills;

with Concorde.Installations.Create;
with Concorde.People.Individuals.Create;
with Concorde.People.Pops.Create;
with Concorde.Government.Create;

with Concorde.Features;
with Concorde.Terrain;
with Concorde.Surfaces;

package body Concorde.Colonies.Configure is

   Template_Config : Tropos.Configuration;
   Got_Config      : Boolean := False;

   procedure Read_Config;

   procedure Create_Colony_From_Template
     (World    : Concorde.Worlds.World_Type;
      Template : Tropos.Configuration);

   ---------------------------------
   -- Create_Colony_From_Template --
   ---------------------------------

   procedure Create_Colony_From_Template
     (World         : Concorde.Worlds.World_Type;
      Template_Name : String)
   is
   begin
      if not Got_Config then
         Read_Config;
      end if;

      Create_Colony_From_Template
        (World, Template_Config.Child (Template_Name));

   end Create_Colony_From_Template;

   ---------------------------------
   -- Create_Colony_From_Template --
   ---------------------------------

   procedure Create_Colony_From_Template
     (World    : Concorde.Worlds.World_Type;
      Template : Tropos.Configuration)
   is

      use WL.Quantities;

      function Get (Name : String;
                    Default : Float)
                    return Float
      is (Template.Get (Name, Default));

      package Skilled_Pop_Vectors is
        new Memor.Element_Vectors
          (Concorde.People.Skills.Root_Pop_Skill, Quantity_Type, Zero);

      Skilled_Pop : Skilled_Pop_Vectors.Vector;

      Tiles            : Concorde.Maps.List_Of_Tiles.List;
      Start_Tiles      : Concorde.Maps.List_Of_Tiles.List;
      Used_Tiles       : Concorde.Maps.List_Of_Tiles.List;
      Current_Position : Concorde.Maps.List_Of_Tiles.Cursor;
      Capital_Tile     : Concorde.Surfaces.Surface_Tile_Index;

      Organics : constant Concorde.Commodities.Array_Of_Commodities :=
                   Concorde.Commodities.Get
                     (Concorde.Commodities.Organic);
      Farms    : constant Concorde.Facilities.Array_Of_Facilities :=
                   Concorde.Facilities.Get_By_Class
                     (Concorde.Facilities.Farm);

      function Current_Tile return Concorde.Surfaces.Surface_Tile_Index;
      procedure Next_Tile;

      Hub        : Concorde.Installations.Installation_Type;
      Port       : Concorde.Installations.Installation_Type;
      Government : Concorde.Government.Government_Type;

      function Start_Sector_Good
        (Tile_Index : Positive)
         return Boolean;

      function Start_Sector_OK
        (Tile_Index : Positive)
         return Boolean;

      procedure Create_Pop_From_Config
        (Config : Tropos.Configuration;
         Sector : Concorde.Surfaces.Surface_Tile_Index);

      procedure Create_Pop_From_Skill
        (Skill     : not null access constant
           Concorde.People.Skills.Root_Pop_Skill'Class;
         Element   : Quantity_Type);

      procedure Create_Pop
        (Sector : Concorde.Surfaces.Surface_Tile_Index;
         Group  : Concorde.People.Groups.Pop_Group;
         Skill  : Concorde.People.Skills.Pop_Skill;
         Size   : Concorde.People.Pops.Pop_Size);

      procedure Create_Installation
        (Facility : Concorde.Facilities.Facility_Type;
         Sector   : Concorde.Surfaces.Surface_Tile_Index);

      procedure Add_Inputs
        (Installation : Concorde.Installations.Installation_Type);

      procedure Add_Population
        (Installation : Concorde.Installations.Installation_Type);

      procedure Create_Service_Facilities is null;

      ----------------
      -- Add_Inputs --
      ----------------

      procedure Add_Inputs
        (Installation : Concorde.Installations.Installation_Type)
      is
         Facility : constant Concorde.Facilities.Facility_Type :=
                      Installation.Facility;
      begin
         for I in 1 .. Facility.Input_Count loop
            declare
               Need : constant Concorde.Commodities.Commodity_Type :=
                        (if Facility.Simple_Input (I)
                         then Facility.Input_Commodity (I)
                         else Facility.Input_Choice_Commodity (I, 1));
               Quant : constant Quantity_Type :=
                         (if Facility.Simple_Input (I)
                          then Facility.Input_Quantity (I)
                          else Facility.Input_Choice_Quantity (I, 1))
                         * Installation.Facility.Capacity_Quantity
                         * To_Quantity (15.0);
               Value    : constant WL.Money.Money_Type :=
                            WL.Money.Total
                              (Need.Base_Price, Quant);

            begin
               Hub.Update.Add_Quantity (Need, Quant, Value);
            end;
         end loop;
      end Add_Inputs;

      --------------------
      -- Add_Population --
      --------------------

      procedure Add_Population
        (Installation : Concorde.Installations.Installation_Type)
      is
      begin
         for I in 1 .. Installation.Facility.Worker_Count loop
            declare
               Skill : constant Concorde.People.Skills.Pop_Skill :=
                         Installation.Facility.Worker_Skill (I);
               Quant : constant Quantity_Type :=
                         Installation.Facility.Worker_Quantity (I);
               Current : constant Quantity_Type  :=
                           Skilled_Pop.Element (Skill);
            begin
               Skilled_Pop.Replace_Element
                 (Skill, Current + Quant);
            end;
         end loop;
      end Add_Population;

      -------------------------
      -- Create_Installation --
      -------------------------

      procedure Create_Installation
        (Facility : Concorde.Facilities.Facility_Type;
         Sector   : Concorde.Surfaces.Surface_Tile_Index)
      is
         Location     : constant Concorde.Locations.Object_Location :=
                          Concorde.Locations.World_Surface
                            (World, Positive (Sector));
         Installation : constant Concorde.Installations.Installation_Type :=
                          Concorde.Installations.Create.Create
                            (Location => Location,
                             Market   => World.Market,
                             Facility => Facility,
                             Cash     => WL.Money.To_Money (1.0E5),
                             Owner    => World.Owner);
      begin
         Installation.Update.Set_Manager
           (Concorde.People.Individuals.Create.Create_Family_Member
              (World.Owner,
               Concorde.Locations.At_Installation (Installation)));

         World.Update.Add_Installation (Sector, Installation);
         Add_Population (Installation);
         Add_Inputs (Installation);
      end Create_Installation;

      ----------------
      -- Create_Pop --
      ----------------

      procedure Create_Pop
        (Sector : Concorde.Surfaces.Surface_Tile_Index;
         Group  : Concorde.People.Groups.Pop_Group;
         Skill  : Concorde.People.Skills.Pop_Skill;
         Size   : Concorde.People.Pops.Pop_Size)
      is
         use Concorde.Commodities;
         Cash  : constant Non_Negative_Real :=
                   Real (Size) * Real (Group.Initial_Cash_Factor);
         Location : constant Concorde.Locations.Object_Location :=
                      Concorde.Locations.World_Surface
                        (World, Positive (Sector));
         Pop   : constant Concorde.People.Pops.Pop_Type :=
                      Concorde.People.Pops.Create.New_Pop
                        (Location     => Location,
                         Market       => World.Market,
                         Wealth_Group => Group,
                         Skill        => Skill,
                         Size         => Size,
                         Cash         => WL.Money.To_Money (Float (Cash)));

         procedure Add_Needs
           (Commodity : Concorde.Commodities.Commodity_Type;
            Need      : Non_Negative_Real);

         ---------------
         -- Add_Needs --
         ---------------

         procedure Add_Needs
           (Commodity : Concorde.Commodities.Commodity_Type;
            Need      : Non_Negative_Real)
         is
            Quantity : constant Quantity_Type :=
                         To_Quantity
                           (Float
                              (Real'Ceiling
                                 (5.0 * Need * Non_Negative_Real (Size))));
         begin
            Hub.Update.Add_Quantity
              (Commodity, Quantity,
               WL.Money.Total (Commodity.Base_Price, Quantity));
         end Add_Needs;

      begin
         World.Update.Add_Pop (Sector, Pop);

         Pop.Wealth_Group.Scan_Needs (Add_Needs'Access);

      end Create_Pop;

      ----------------------------
      -- Create_Pop_From_Config --
      ----------------------------

      procedure Create_Pop_From_Config
        (Config : Tropos.Configuration;
         Sector : Concorde.Surfaces.Surface_Tile_Index)
      is
--           use Concorde.Commodities;
         use Concorde.People.Groups;
         use Concorde.People.Skills;
         Group : constant Pop_Group := Get (Config.Config_Name);
         Skill : constant Pop_Skill := Get (Config.Get ("skill", "unskilled"));
         Size  : constant Natural := Config.Get ("size");
      begin
         Create_Pop
           (Sector, Group, Skill,
            Concorde.People.Pops.Pop_Size (Size));
      end Create_Pop_From_Config;

      ---------------------------
      -- Create_Pop_From_Skill --
      ---------------------------

      procedure Create_Pop_From_Skill
        (Skill     : not null access constant
           Concorde.People.Skills.Root_Pop_Skill'Class;
         Element   : Quantity_Type)
      is
      begin
         Create_Pop (Capital_Tile, Skill.Wealth_Group,
                     Concorde.People.Skills.Pop_Skill (Skill),
                     Concorde.People.Pops.Pop_Size
                       (WL.Quantities.To_Float (Element)));
      end Create_Pop_From_Skill;

      ------------------
      -- Current_Tile --
      ------------------

      function Current_Tile return Concorde.Surfaces.Surface_Tile_Index is
      begin
         return Concorde.Surfaces.Surface_Tile_Index
           (Concorde.Maps.List_Of_Tiles.Element (Current_Position));
      end Current_Tile;

      ---------------
      -- Next_Tile --
      ---------------

      procedure Next_Tile is
         use Concorde.Maps.List_Of_Tiles;
      begin
         Next (Current_Position);
         if not Has_Element (Current_Position) then
            declare
               New_Tiles : List;
            begin
               World.External_Border (Used_Tiles, New_Tiles);
               Tiles := New_Tiles;
               for Tile of New_Tiles loop
                  Used_Tiles.Append (Tile);
               end loop;
            end;
            Current_Position := Tiles.First;
         end if;

         while Has_Element (Current_Position) loop
            declare
               use type Concorde.Features.Feature_Type;
               Tile_Index : constant Concorde.Surfaces.Surface_Tile_Index :=
                              Current_Tile;
            begin
               exit when
                 (not World.Sector_Has_Terrain (Tile_Index)
                  or else not World.Sector_Terrain (Tile_Index).Is_Water)
                 and then
                   (not World.Sector_Has_Feature (Tile_Index)
                    or else World.Sector_Feature (Tile_Index) /= Features.Ice);
               Next (Current_Position);
            end;
         end loop;

         if not Has_Element (Current_Position) then
            Current_Position := Tiles.Last;
            Next_Tile;
         end if;

      end Next_Tile;

      -----------------------
      -- Start_Sector_Good --
      -----------------------

      function Start_Sector_Good
        (Tile_Index : Positive)
            return Boolean
      is
         use Concorde.Features, Concorde.Terrain;
         Sector_Index : constant Concorde.Surfaces.Surface_Tile_Index :=
                          Concorde.Surfaces.Surface_Tile_Index
                            (Tile_Index);
         Terrain      : constant Terrain_Type :=
                          (if World.Sector_Has_Terrain (Sector_Index)
                           then World.Sector_Terrain (Sector_Index)
                           else null);
         Feature      : constant Feature_Type :=
                          (if World.Sector_Has_Feature (Sector_Index)
                           then World.Sector_Feature (Sector_Index)
                           else null);
         Water_Count  : Natural := 0;
         Low_Temp     : constant Non_Negative_Real :=
                          World.Sector_Temperature_Low (Sector_Index);
         High_Temp    : constant Non_Negative_Real :=
                          World.Sector_Temperature_High (Sector_Index);
      begin
         if Terrain /= null and then Terrain.Is_Water then
            return False;
         end if;

         if Feature = Ice then
            return False;
         end if;

         if Low_Temp < 260.0 or else High_Temp > 323.0 then
            return False;
         end if;

         for I in 1 .. World.Neighbour_Count (Tile_Index) loop
            declare
               Neighbour : constant Concorde.Surfaces.Surface_Tile_Index :=
                             Concorde.Surfaces.Surface_Tile_Index
                               (World.Neighbour (Tile_Index, I));
               Neighbour_Low : constant Non_Negative_Real :=
                                 World.Sector_Temperature_Low (Neighbour);
               Neighbour_High : constant Non_Negative_Real :=
                                  World.Sector_Temperature_High (Neighbour);
            begin
               if World.Sector_Has_Feature (Neighbour)
                 and then World.Sector_Feature (Neighbour) = Ice
               then
                  return False;
               end if;
               if Neighbour_Low < 260.0 or else Neighbour_High > 323.0 then
                  return False;
               end if;

               if World.Sector_Has_Terrain (Neighbour)
                 and then World.Sector_Terrain (Neighbour).Is_Water
               then
                  Water_Count := Water_Count + 1;
               end if;
            end;
         end loop;
         if Water_Count > 3 then
            return False;
         end if;

         return True;
      end Start_Sector_Good;

      ---------------------
      -- Start_Sector_OK --
      ---------------------

      function Start_Sector_OK
        (Tile_Index : Positive)
         return Boolean
      is
         use Concorde.Features, Concorde.Terrain;
         Sector_Index : constant Concorde.Surfaces.Surface_Tile_Index :=
                          Concorde.Surfaces.Surface_Tile_Index
                            (Tile_Index);
         Terrain      : constant Terrain_Type :=
                          (if World.Sector_Has_Terrain (Sector_Index)
                           then World.Sector_Terrain (Sector_Index)
                           else null);
         Feature      : constant Feature_Type :=
                          (if World.Sector_Has_Feature (Sector_Index)
                           then World.Sector_Feature (Sector_Index)
                           else null);
         Water_Count  : Natural := 0;
         Low_Temp     : constant Non_Negative_Real :=
                          World.Sector_Temperature_Low (Sector_Index);
         High_Temp    : constant Non_Negative_Real :=
                          World.Sector_Temperature_High (Sector_Index);
      begin
         if Terrain /= null and then Terrain.Is_Water then
            return False;
         end if;

         if Feature = Ice then
            return False;
         end if;

         if Low_Temp < 240.0 or else High_Temp > 343.0 then
            return False;
         end if;

         for I in 1 .. World.Neighbour_Count (Tile_Index) loop
            declare
               Neighbour      : constant Surfaces.Surface_Tile_Index :=
                                  Concorde.Surfaces.Surface_Tile_Index
                                    (World.Neighbour (Tile_Index, I));
               Neighbour_Low  : constant Non_Negative_Real :=
                                  World.Sector_Temperature_Low (Neighbour);
               Neighbour_High : constant Non_Negative_Real :=
                                  World.Sector_Temperature_High (Neighbour);
            begin
               if World.Sector_Has_Feature (Neighbour)
                 and then World.Sector_Feature (Neighbour) = Ice
               then
                  return False;
               end if;
               if Neighbour_Low < 240.0 or else Neighbour_High > 343.0 then
                  return False;
               end if;

               if World.Sector_Has_Terrain (Neighbour)
                 and then World.Sector_Terrain (Neighbour).Is_Water
               then
                  Water_Count := Water_Count + 1;
               end if;
            end;
         end loop;
         if Water_Count > 5 then
            return False;
         end if;

         return True;
      end Start_Sector_OK;

      Basic_Living_Wage : WL.Money.Price_Type := WL.Money.Zero;

   begin

      Ada.Text_IO.Put_Line
        ("New colony on " & World.Name);

      World.Scan_Tiles (Start_Sector_Good'Access, Start_Tiles);
      if Start_Tiles.Is_Empty then
         Ada.Text_IO.Put_Line
           ("  -- retry with looser constraints " & World.Name);
         World.Scan_Tiles (Start_Sector_OK'Access, Start_Tiles);
      end if;

      if Start_Tiles.Is_Empty then
         Ada.Text_IO.Put_Line
           ("Unable to find good spot for hub");
         return;
      end if;

      Tiles.Append (Start_Tiles.First_Element);
      Used_Tiles.Append (Start_Tiles.First_Element);
      Current_Position := Tiles.First;
      Capital_Tile :=
        Concorde.Surfaces.Surface_Tile_Index
          (Tiles.First_Element);

      Hub :=
        Concorde.Installations.Create.Create
          (Location =>
             Concorde.Locations.World_Surface
               (World,
                Tiles.First_Element),
           Market => null,
           Facility => Concorde.Facilities.Colony_Hub,
           Cash     =>
             WL.Money.To_Money
               (Get ("cash", 10_000.0)),
           Owner    => World.Owner);

      Hub.Update.Set_Manager
        (Concorde.People.Individuals.Create.Create_Family_Member
           (World.Owner,
            Concorde.Locations.At_Installation (Hub)));

      Government :=
        Concorde.Government.Create.Create_Government
          (Governed          => World,
           Cash              =>
             WL.Money.To_Money
               (Get ("cash", 10_000.0)),
           Owner             => World.Owner,
           Headquarters      => Hub,
           Basic_Living_Wage => Basic_Living_Wage);

      Government.Update.Set_Governor
        (Concorde.People.Individuals.Create.Create_Family_Member
           (World.Owner,
            Concorde.Locations.At_Installation (Hub)));

      World.Update.Add_Installation (Current_Tile, Hub);
      World.Update.Set_Government (Government);

      Hub.Update.Set_Market (World.Market);

      Add_Population (Hub);

      for Pop_Config of Template.Child ("pops") loop
         Create_Pop_From_Config
           (Pop_Config, Current_Tile);
      end loop;

      if Template.Contains ("basic_living_wage") then
         declare
            use WL.Money;
         begin
            for Commodity_Config of Template.Child ("basic_living_wage") loop
               Basic_Living_Wage := Basic_Living_Wage
                 + World.Market.Current_Price
                 (Concorde.Commodities.Get
                    (Commodity_Config.Config_Name));
            end loop;
         end;
         Government.Update.Set_Basic_Living_Wage (Basic_Living_Wage);
      end if;

      Next_Tile;

      declare
         Install_Config : constant Tropos.Configuration :=
                            Template.Child ("installations");
      begin
         for I in 1 .. Install_Config.Get ("resource_generator", 0) loop
            declare
               Tile          : constant Concorde.Surfaces.Surface_Tile_Index :=
                                 Current_Tile;
               Resource      : Concorde.Commodities.Commodity_Type;
               Accessibility : Unit_Real;
               Concentration : Unit_Real;
               Facility      : Concorde.Facilities.Facility_Type;
            begin
               World.Get_Sector_Resource
                 (Tile, Resource, Concentration, Accessibility);
               if Concentration + Accessibility > 0.8 then
                  Facility :=
                    Concorde.Facilities.Resource_Generator
                      (World.Sector_Resource (Tile));
               else
                  if WL.Random.Random_Number (1, 6) < 10 then
                     Facility := Concorde.Facilities.Get ("grain_farm");
                  elsif True then
                     Facility :=
                       Farms (WL.Random.Random_Number (1, Farms'Last));
                  else
                     Facility :=
                       Concorde.Facilities.Resource_Generator
                         (Organics
                            (WL.Random.Random_Number (1, Organics'Last)));
                  end if;
               end if;

               Create_Installation
                 (Facility, Tile);
               Next_Tile;
            end;
         end loop;

         for Facility of Concorde.Facilities.All_Facilities loop
            declare
               Id : constant String := Facility.Identifier;
            begin
               if Install_Config.Contains (Id) then
                  Ada.Text_IO.Put_Line
                    (Install_Config.Get (Id) & " x " & Id);
               end if;

               for I in 1 .. Install_Config.Get (Facility.Identifier, 0) loop
                  Create_Installation (Facility, Current_Tile);
                  Next_Tile;
               end loop;
            end;
         end loop;

      end;

      Create_Service_Facilities;

      Port :=
        Concorde.Installations.Create.Create
          (Location =>
             Concorde.Locations.World_Surface
               (World, Positive (Current_Tile)),
           Market   => World.Market,
           Facility => Concorde.Facilities.Get ("port"),
           Cash     =>
             WL.Money.To_Money (500_000.0),
           Owner    => World.Owner);

      World.Update.Add_Installation (Current_Tile, Port);

      Skilled_Pop.Scan (Create_Pop_From_Skill'Access);

   end Create_Colony_From_Template;

   -----------------
   -- Read_Config --
   -----------------

   procedure Read_Config is
   begin
      Template_Config :=
        Tropos.Reader.Read_Config
          (Concorde.Paths.Config_File
             ("scenarios/"
              & Concorde.Options.Scenario
              & "/colonies.txt"));
      Got_Config := True;
   end Read_Config;

end Concorde.Colonies.Configure;
