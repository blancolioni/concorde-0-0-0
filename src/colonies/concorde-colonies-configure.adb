with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.Random;

with Tropos.Reader;

with Concorde.Configure;

with Concorde.Locations;
with Concorde.Maps;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Commodities;
with Concorde.Factions;
with Concorde.Facilities;
with Concorde.Installations;

with Concorde.People.Groups;

with Concorde.Installations.Create;
with Concorde.People.Pops.Create;
with Concorde.Government.Create;

with Concorde.Armies.Create;
with Concorde.Regiments.Create;
with Concorde.Units;

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

   function Get_Initial_Artisans
     (Config : Tropos.Configuration)
     return Concorde.Facilities.Array_Of_Facilities;

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

      use Concorde.Quantities;

      function Get (Name : String;
                    Default : Float)
                    return Float
      is (Template.Get (Name, Default));

      Tiles            : Concorde.Maps.List_Of_Tiles.List;
      Start_Tiles      : Concorde.Maps.List_Of_Tiles.List;
      Used_Tiles       : Concorde.Maps.List_Of_Tiles.List;
      Current_Position : Concorde.Maps.List_Of_Tiles.Cursor;
--        Capital_Tile     : Concorde.Surfaces.Surface_Tile_Index;

      Artisan_Facilities : constant Concorde.Facilities.Array_Of_Facilities :=
                             Get_Initial_Artisans (Template);
      Next_Artisan       : Natural := 0;

      function Current_Tile return Concorde.Surfaces.Surface_Tile_Index;
      procedure Next_Tile;

      Hub         : Concorde.Installations.Installation_Type;
      Port        : Concorde.Installations.Installation_Type;
      Government  : Concorde.Government.Government_Type;

      Soldier_Pop : Concorde.People.Pops.Pop_Type;

      function Start_Sector_Good
        (Tile_Index : Positive)
         return Boolean;

      function Start_Sector_OK
        (Tile_Index : Positive)
         return Boolean;

      procedure Create_Pop_From_Config
        (Config : Tropos.Configuration;
         Sector : Concorde.Surfaces.Surface_Tile_Index);

      procedure Create_Pop
        (Sector : Concorde.Surfaces.Surface_Tile_Index;
         Group  : Concorde.People.Groups.Pop_Group;
         Size   : Concorde.People.Pops.Pop_Size);

      procedure Create_Installation
        (Facility : Concorde.Facilities.Facility_Type;
         Sector   : Concorde.Surfaces.Surface_Tile_Index;
         Size     : Concorde.Quantities.Quantity_Type);

      procedure Add_Inputs
        (Installation : Concorde.Installations.Installation_Type);

      procedure Create_Buildings
        (List_Config : Tropos.Configuration);

      procedure Create_Service_Facilities is null;

      ----------------
      -- Add_Inputs --
      ----------------

      procedure Add_Inputs
        (Installation : Concorde.Installations.Installation_Type)
      is
         Facility : constant Concorde.Facilities.Facility_Type :=
                      Installation.Facility;
         Size     : constant Concorde.Quantities.Quantity_Type :=
                      Installation.Size;
      begin
         for I in 1 .. Facility.Input_Count loop
            declare
               Need : constant Concorde.Commodities.Commodity_Type :=
                        Facility.Input_Choice_Commodity (I, 1);
               Quant : constant Quantity_Type :=
                         Scale
                           (Facility.Input_Choice_Quantity (Size, I, 1),
                            2.0);
               Value    : constant Concorde.Money.Money_Type :=
                            Concorde.Money.Total
                              (Need.Base_Price, Quant);

            begin
               Installation.Log_Production
                 ("adding input "
                  & Show (Quant) & " " & Need.Name);
               Installation.Update.Add_Quantity (Need, Quant, Value);
            end;
         end loop;
      end Add_Inputs;

      ----------------------
      -- Create_Buildings --
      ----------------------

      procedure Create_Buildings
        (List_Config : Tropos.Configuration)
      is
         package Vectors is
           new Ada.Containers.Vectors (Positive, Facilities.Facility_Type,
                                       Facilities."=");

         Vs : Vectors.Vector;

      begin
         for Config of List_Config loop
            declare
               Count : Natural := 1;
            begin
               if Config.Child_Count > 0 then
                  Count := Config.Value;
               end if;
               for I in 1 .. Count loop
                  Vs.Append (Concorde.Facilities.Get (Config.Config_Name));
               end loop;
            end;
         end loop;

         while not Vs.Is_Empty loop
            declare
               Tile     : constant Concorde.Surfaces.Surface_Tile_Index :=
                            Current_Tile;
               Index    : constant Positive :=
                            WL.Random.Random_Number (1, Vs.Last_Index);
               Facility : constant Concorde.Facilities.Facility_Type :=
                            Vs.Element (Index);
            begin
               Create_Installation (Facility, Tile,
                                    (if Facility.Is_Farm
                                     or else Facility.Is_Resource_Generator
                                     then Facility.Workforce
                                     else To_Quantity (1000.0)));
               Vs.Replace_Element (Index, Vs.Last_Element);
               Vs.Delete_Last;
               Next_Tile;
            end;

         end loop;

      end Create_Buildings;

      -------------------------
      -- Create_Installation --
      -------------------------

      procedure Create_Installation
        (Facility : Concorde.Facilities.Facility_Type;
         Sector   : Concorde.Surfaces.Surface_Tile_Index;
         Size     : Concorde.Quantities.Quantity_Type)
      is
         Location     : constant Concorde.Locations.Object_Location :=
                          Concorde.Locations.World_Surface
                            (World, Positive (Sector));
         Installation : constant Concorde.Installations.Installation_Type :=
                          Concorde.Installations.Create.Create
                            (Location => Location,
                             Market   => World.Market,
                             Facility => Facility,
                             Cash     => Concorde.Money.To_Money (1.0E7),
                             Owner    => World.Owner,
                             Size     => Size);
      begin
         World.Update.Add_Installation (Sector, Installation);
         Add_Inputs (Installation);
      end Create_Installation;

      ----------------
      -- Create_Pop --
      ----------------

      procedure Create_Pop
        (Sector : Concorde.Surfaces.Surface_Tile_Index;
         Group  : Concorde.People.Groups.Pop_Group;
         Size   : Concorde.People.Pops.Pop_Size)
      is
         Cash  : constant Non_Negative_Real :=
                   Real (Size) * Real (Group.Initial_Cash_Factor);
         Location : constant Concorde.Locations.Object_Location :=
                      Concorde.Locations.World_Surface
                        (World, Positive (Sector));
         Pop   : constant Concorde.People.Pops.Pop_Type :=
                      Concorde.People.Pops.Create.New_Pop
                        (Location  => Location,
                         Market    => World.Market,
                         Group     => Group,
                         Size      => Size,
                         Cash      => Concorde.Money.To_Money (Float (Cash)));
      begin
         if Group.Is_Artisan then
            Next_Artisan := Next_Artisan + 1;
            if Next_Artisan > Artisan_Facilities'Last then
               Next_Artisan := 1;
            end if;
            Pop.Update.Set_Production (Artisan_Facilities (Next_Artisan));
            Pop.Log_Production (Artisan_Facilities (Next_Artisan).Name);
         end if;
         if Group.Is_Soldier then
            Soldier_Pop := Pop;
         end if;

         World.Update.Add_Pop (Sector, Pop);
      end Create_Pop;

      ----------------------------
      -- Create_Pop_From_Config --
      ----------------------------

      procedure Create_Pop_From_Config
        (Config : Tropos.Configuration;
         Sector : Concorde.Surfaces.Surface_Tile_Index)
      is
         use Concorde.People.Groups;
         Group : constant Pop_Group := Get (Config.Config_Name);
         Size  : constant Natural := Config.Value;
      begin
         Create_Pop
           (Sector, Group,
            Concorde.People.Pops.Pop_Size (Size));
      end Create_Pop_From_Config;

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

      Basic_Living_Wage : Concorde.Money.Price_Type := Concorde.Money.Zero;

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
--        Capital_Tile :=
--          Concorde.Surfaces.Surface_Tile_Index
--            (Tiles.First_Element);

      Hub :=
        Concorde.Installations.Create.Create
          (Location =>
             Concorde.Locations.World_Surface
               (World,
                Tiles.First_Element),
           Market => null,
           Facility => Concorde.Facilities.Colony_Hub,
           Cash     =>
             Concorde.Money.To_Money
               (Get ("cash", 10_000.0)),
           Owner    => World.Owner,
           Size     => Concorde.Quantities.To_Quantity (10_000.0));

      Government :=
        Concorde.Government.Create.Create_Government
          (Governed          => World,
           Cash              =>
             Concorde.Money.To_Money
               (Get ("cash", 10_000.0)),
           Owner             => World.Owner,
           Headquarters      => Hub,
           Basic_Living_Wage => Basic_Living_Wage);

      World.Update.Add_Installation (Current_Tile, Hub);
      World.Update.Set_Government (Government);

      World.Owner.Update.Set_Capital_Building (Hub);

      Hub.Update.Set_Market (World.Market);

      for Pop_Config of Template.Child ("pops") loop
         Create_Pop_From_Config
           (Pop_Config, Current_Tile);
      end loop;

      declare
         Army : constant Concorde.Armies.Army_Type :=
                  Concorde.Armies.Create.New_Army
                    (Faction  => World.Owner,
                     Location =>
                       Concorde.Locations.At_Installation (Hub));
      begin
         for Regiment_Config of Template.Child ("regiments") loop
            declare
               Unit  : constant String := Regiment_Config.Config_Name;
               Count : constant Positive :=
                         (if Regiment_Config.Child_Count = 1
                          then Regiment_Config.Value else 1);
            begin
               for I in 1 .. Count loop
                  declare
                     Regiment : constant Concorde.Regiments.Regiment_Type :=
                                  Concorde.Regiments.Create.New_Regiment
                                    (Pop  => Soldier_Pop,
                                     Unit =>
                                       Concorde.Units.Get (Unit));
                  begin
                     Army.Update.Add_Regiment (Regiment);
                  end;
               end loop;
            end;
         end loop;
      end;

      if Template.Contains ("basic_living_wage") then
         declare
            use Concorde.Money;
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

      if Template.Contains ("building") then
         Create_Buildings (Template.Child ("building"));
      end if;

      if Template.Contains ("rgo") then
         Create_Buildings (Template.Child ("rgo"));
      end if;

      Create_Service_Facilities;

      Port :=
        Concorde.Installations.Create.Create
          (Location =>
             Concorde.Locations.World_Surface
               (World, Positive (Current_Tile)),
           Market   => World.Market,
           Facility => Concorde.Facilities.Get ("port"),
           Cash     =>
             Concorde.Money.To_Money (5_000_000.0),
           Owner    => World.Owner,
           Size     => Concorde.Quantities.To_Quantity (1000.0));

      World.Update.Add_Installation (Current_Tile, Port);

   end Create_Colony_From_Template;

   --------------------------
   -- Get_Initial_Artisans --
   --------------------------

   function Get_Initial_Artisans
     (Config : Tropos.Configuration)
         return Concorde.Facilities.Array_Of_Facilities
   is
      use Concorde.Facilities;
   begin
      if Config.Contains ("artisan") then
         declare
            Artisan_Config : constant Tropos.Configuration :=
                               Config.Child ("artisan");
            Result         : Array_Of_Facilities
              (1 .. Artisan_Config.Child_Count);
            Index          : Natural := 0;
         begin
            for Item of Artisan_Config loop
               Index := Index + 1;
               Result (Index) := Get (Item.Config_Name);
            end loop;
            return Result;
         end;
      else
         return Concorde.Facilities.Artisan_Facilities;
      end if;
   end Get_Initial_Artisans;

   -----------------
   -- Read_Config --
   -----------------

   procedure Read_Config is
   begin
      Template_Config :=
        Tropos.Reader.Read_Config
          (Concorde.Configure.File_Path
             ("init", "colonies"));
      Got_Config := True;
   end Read_Config;

end Concorde.Colonies.Configure;
