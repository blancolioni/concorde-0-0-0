with Ada.Characters.Handling;
with Ada.Containers.Vectors;

with Lui.Rendering;
with Lui.Tables;

with Concorde.Solar_System;

with Concorde.Hash_Table;
with Concorde.Watchers;

with Concorde.Money;

with Concorde.Empires;

with Concorde.Commodities.Db;
with Concorde.Systems.Db;

with Concorde.Stars;
with Concorde.Worlds;

with Concorde.People.Pops.Lists;

with Concorde.Ships.Models;
with Concorde.Worlds.Models;

with Concorde.Worlds.Db;

package body Concorde.Systems.Models is

   subtype Pop_Column is Integer range 1 .. 3;

   type Pop_Table is
     new Lui.Tables.Root_Model_Table with
      record
         System : Star_System_Type;
      end record;

   overriding function Heading_Column_Text
     (Table : Pop_Table;
      Col   : Positive)
      return String
   is ((case Pop_Column (Col) is
           when 1 => "Group",
           when 2 => "Size",
           when 3 => "Cash"));

   overriding function Cell_Text
     (Table : Pop_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   overriding function Row_Count
     (Table : Pop_Table)
      return Natural
   is (Natural (Table.System.Pops.Length));

   subtype Installation_Column is Integer range 1 .. 2;

   type Installation_Table is
     new Lui.Tables.Root_Model_Table with
      record
         System : Star_System_Type;
      end record;

   overriding function Heading_Column_Text
     (Table : Installation_Table;
      Col   : Positive)
      return String
   is ((case Installation_Column (Col) is
           when 1 => "Facility",
           when 2 => "Cash"));

   overriding function Cell_Text
     (Table : Installation_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   overriding function Row_Count
     (Table : Installation_Table)
      return Natural
   is (Natural (Table.System.Installations.Length));

   subtype Market_Column is Integer range 1 .. 4;

   package Row_Map_Vectors is
     new Ada.Containers.Vectors
       (Positive,
        Concorde.Commodities.Commodity_Type,
        Concorde.Commodities."=");

   type Market_Table is
     new Lui.Tables.Root_Model_Table with
      record
         System  : Star_System_Type;
         Row_Map : Row_Map_Vectors.Vector;
      end record;

   overriding function Heading_Column_Text
     (Table : Market_Table;
      Col   : Positive)
      return String
   is ((case Market_Column (Col) is
           when 1 => "Commodity",
           when 2 => "Price",
           when 3 => "Supply",
           when 4 => "Demand"));

   overriding function Cell_Text
     (Table : Market_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   subtype Ship_Column is Integer range 1 .. 4;

   type Ship_Table is
     new Lui.Tables.Root_Model_Table with
      record
         System : Star_System_Type;
      end record;

   overriding function Heading_Column_Text
     (Table : Ship_Table;
      Col   : Positive)
      return String
   is ((case Ship_Column (Col) is
           when 1 => "Id",
           when 2 => "Name",
           when 3 => "Owner",
           when 4 => "Destination"));

   overriding function Cell_Text
     (Table : Ship_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   overriding function Row_Count
     (Table : Ship_Table)
      return Natural
   is (Natural (Table.System.Ships.Length));

   overriding function Row_Model
     (Table : Ship_Table;
      Row   : Positive)
      return access Lui.Models.Root_Object_Model'Class;

   type Rendered_World is
      record
         World : Concorde.Worlds.World_Type;
         X, Y  : Integer;
         W, H  : Integer;
      end record;

   package List_Of_Rendered_Worlds is
     new Ada.Containers.Doubly_Linked_Lists (Rendered_World);

   type Root_Star_System_Model is
     new Lui.Models.Root_Object_Model
     and Concorde.Watchers.Watcher_Interface with
      record
         System         : Star_System_Type;
         Rendered_Words : List_Of_Rendered_Worlds.List;
         Needs_Render   : Boolean := True;
      end record;

   overriding procedure On_Object_Changed
     (Model  : in out Root_Star_System_Model;
      Object : Concorde.Watchers.Watched_Object_Interface'Class);

   overriding function Handle_Update
     (Model    : in out Root_Star_System_Model)
      return Boolean
   is (Model.Needs_Render);

   overriding function Select_XY
     (Model : Root_Star_System_Model;
      X, Y  : Natural)
      return Lui.Models.Object_Model;

   overriding procedure Render
     (Model    : in out Root_Star_System_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   overriding function Get_Drag_Behaviour
     (Model : Root_Star_System_Model)
      return Lui.Models.Drag_Behaviour
   is (Lui.Models.Translation);

   type Star_System_Model_Access is
     access all Root_Star_System_Model'Class;

   package Model_Table is
     new Concorde.Hash_Table (Star_System_Model_Access);

   System_Models : Model_Table.Map;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Market_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      use Concorde.Commodities;
      Commodity : constant Commodity_Type := Table.Row_Map (Row);
   begin
      case Market_Column (Col) is
         when 1 =>
            return Commodity.Name;
         when 2 =>
            return Concorde.Money.Image
              (Table.System.Market.Current_Price (Commodity));
         when 3 =>
            return Concorde.Quantities.Image
              (Table.System.Market.Last_Supply (Commodity));
         when 4 =>
            return Concorde.Quantities.Image
              (Table.System.Market.Last_Demand (Commodity));
      end case;
   end Cell_Text;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Pop_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      use Concorde.People.Pops.Lists;
      Position : Cursor := Table.System.Pops.First;
   begin
      for I in 2 .. Row loop
         Next (Position);
      end loop;

      declare
         Pop : constant Concorde.People.Pops.Pop_Type :=
                  Element (Position);
      begin
         case Pop_Column (Col) is
            when 1 =>
               return (if Pop.Rich then "Rich"
                       elsif Pop.Middle_Class then "Middle Class"
                       else "Poor");
            when 2 =>
               return Lui.Approximate_Image (Natural (Pop.Size));
            when 3 =>
               return Concorde.Money.Image (Pop.Cash);
         end case;
      end;
   end Cell_Text;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Installation_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      use Concorde.Installations.Lists;
      Position : Cursor := Table.System.Installations.First;
   begin
      for I in 2 .. Row loop
         Next (Position);
      end loop;

      declare
         Installation : constant Concorde.Installations.Installation_Type :=
                          Element (Position);
      begin
         case Installation_Column (Col) is
            when 1 =>
               return Installation.Facility.Name;
            when 2 =>
               return Concorde.Money.Image (Installation.Cash);
         end case;
      end;
   end Cell_Text;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Ship_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      use Concorde.Ships.Lists;
      Position : Cursor := Table.System.Ships.First;
   begin
      for I in 2 .. Row loop
         Next (Position);
      end loop;

      declare
         Ship : constant Concorde.Ships.Ship_Type :=
                  Element (Position);
      begin
         case Ship_Column (Col) is
            when 1 =>
               return Ship.Identifier;
            when 2 =>
               return Ship.Name;
            when 3 =>
               return Ship.Owner.Name;
            when 4 =>
               if Ship.Has_Destination then
                  return Ship.Destination.Name;
               else
                  return "-";
               end if;
         end case;
      end;
   end Cell_Text;

   -----------------------
   -- On_Object_Changed --
   -----------------------

   overriding procedure On_Object_Changed
     (Model  : in out Root_Star_System_Model;
      Object : Concorde.Watchers.Watched_Object_Interface'Class)
   is
      pragma Unreferenced (Object);
   begin
      Model.Needs_Render := True;
   end On_Object_Changed;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Root_Star_System_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is

      procedure Render_System_Object
        (Object : Star_System_Object_Interface'Class;
         Position : Concorde.Geometry.Radians);

      procedure Render_Star
        (Star : Concorde.Stars.Root_Star_Type'Class);

      procedure Render_World
        (World    : Concorde.Worlds.Root_World_Type'Class;
         Position : Concorde.Geometry.Radians);

      -----------------
      -- Render_Star --
      -----------------

      procedure Render_Star
        (Star : Concorde.Stars.Root_Star_Type'Class)
      is
         pragma Unreferenced (Star);
         Star_Width  : constant Natural :=
                         Natural (100.0 / Model.Eye_Z);
         Star_Height : constant Natural :=
                         Natural (100.0 / Model.Eye_Z);
      begin
         Renderer.Draw_Image
           (Model.Width / 2 - Star_Width / 2,
            Model.Height / 2 - Star_Width / 2,
            Star_Width, Star_Height,
            "stars/sun");
      end Render_Star;

      --------------------------
      -- Render_System_Object --
      --------------------------

      procedure Render_System_Object
        (Object   : Star_System_Object_Interface'Class;
         Position : Concorde.Geometry.Radians)
      is
      begin
         if Object in Concorde.Stars.Root_Star_Type'Class then
            Render_Star
              (Concorde.Stars.Root_Star_Type'Class (Object));
         else
            Render_World
              (Concorde.Worlds.Root_World_Type'Class (Object), Position);
         end if;
      end Render_System_Object;

      ------------------
      -- Render_World --
      ------------------

      procedure Render_World
        (World    : Concorde.Worlds.Root_World_Type'Class;
         Position : Concorde.Geometry.Radians)
      is
         use Concorde.Geometry;
         X_Offset : constant Real := Cos (Position) * World.Semimajor_Axis;
         Y_Offset : constant Real := Sin (Position) * World.Semimajor_Axis;
         Scale_Factor : constant Non_Negative_Real :=
                          1000.0 / Concorde.Solar_System.Earth_Orbit
                            / Model.Eye_Z;
         Scaled_X : constant Real :=
                      X_Offset * Scale_Factor;
         Scaled_Y : constant Real :=
                          Y_Offset * Scale_Factor;
         Image_Name   : constant String :=
                          Ada.Characters.Handling.To_Lower
                            (Concorde.Worlds.World_Category'Image
                               (World.Category));
         Image_Size   : constant Natural :=
                          Natural (50.0 / Model.Eye_Z);
         Render       : constant Rendered_World :=
                          (World => Concorde.Worlds.Db.Reference (World),
                           X     => Model.Width / 2
                           + Integer (Scaled_X) - Image_Size / 2,
                           Y     => Model.Height / 2
                           + Integer (Scaled_Y) - Image_Size / 2,
                           W     => Image_Size,
                           H     => Image_Size);

      begin
         Renderer.Draw_Circle
           (X          => Model.Width / 2,
            Y          => Model.Height / 2,
            Radius     => Natural (World.Semimajor_Axis * Scale_Factor),
            Colour     => (0.0, 0.6, 0.0, 1.0),
            Filled     => False,
            Line_Width => 1);

         Renderer.Draw_Image
           (X        => Render.X,
            Y        => Render.Y,
            W        => Render.W,
            H        => Render.H,
            Resource => "planets/" & Image_Name & "-planet");
         Model.Rendered_Words.Append (Render);

      end Render_World;

   begin
      Model.Rendered_Words.Clear;
      for Object of Model.System.Objects loop
         Render_System_Object (Object.Object.all, Object.Start);
      end loop;

      Model.Needs_Render := False;
   end Render;

   ---------------
   -- Row_Model --
   ---------------

   overriding function Row_Model
     (Table : Ship_Table;
      Row   : Positive)
      return access Lui.Models.Root_Object_Model'Class
   is
      use Concorde.Ships.Lists;
      Position : Cursor := Table.System.Ships.First;
   begin
      for I in 2 .. Row loop
         Next (Position);
      end loop;

      declare
         Ship : constant Concorde.Ships.Ship_Type :=
                  Element (Position);
      begin
         return Concorde.Ships.Models.Create_Ship_Model
           (Ship);
      end;

   end Row_Model;

   ---------------
   -- Select_XY --
   ---------------

   overriding function Select_XY
     (Model : Root_Star_System_Model;
      X, Y  : Natural)
      return Lui.Models.Object_Model
   is
   begin
      for Render of Model.Rendered_Words loop
         if X in Render.X .. Render.X + Render.W
           and then Y in Render.Y .. Render.Y + Render.H
         then
            return Concorde.Worlds.Models.World_Model (Render.World);
         end if;
      end loop;
      return null;
   end Select_XY;

   ------------------
   -- System_Model --
   ------------------

   function System_Model
     (System : Star_System_Type)
      return Lui.Models.Object_Model
   is
      Result : Star_System_Model_Access;

      procedure Watch (System : in out Root_Star_System_Type'Class);

      -----------
      -- Watch --
      -----------

      procedure Watch (System : in out Root_Star_System_Type'Class) is
      begin
         System.Add_Watcher (Result);
      end Watch;

   begin
      if not System_Models.Contains (System.Name) then
         declare
            M_Table : Market_Table;
            M       : Lui.Tables.Model_Table;
            P_Table : Pop_Table;
            P       : Lui.Tables.Model_Table;
            I_Table : Installation_Table;
            I       : Lui.Tables.Model_Table;
            S_Table : Ship_Table;
            S       : Lui.Tables.Model_Table;

            procedure Add_Market_Row
              (Commodity : Concorde.Commodities.Commodity_Type);

            --------------------
            -- Add_Market_Row --
            --------------------

            procedure Add_Market_Row
              (Commodity : Concorde.Commodities.Commodity_Type)
            is
            begin
               M_Table.Row_Map.Append (Commodity);
            end Add_Market_Row;

         begin
            M_Table.System := System;

            Concorde.Commodities.Db.Scan (Add_Market_Row'Access);

            M_Table.Initialise
              ("Market", M_Table.Row_Map.Last_Index,
               Market_Column'Last);

            M := new Market_Table'(M_Table);

            P_Table.System := System;
            P_Table.Initialise ("Population", 0, Pop_Column'Last);
            P := new Pop_Table'(P_Table);

            I_Table.System := System;
            I_Table.Initialise ("Installations", 0, Installation_Column'Last);
            I := new Installation_Table'(I_Table);

            S_Table.System := System;
            S_Table.Initialise ("Ships", 0, Ship_Column'Last);
            S := new Ship_Table'(S_Table);

            Result := new Root_Star_System_Model;
            Result.Initialise (System.Name, Tables => (M, P, I, S));
            Result.System := System;

            Db.Update (System.Reference, Watch'Access);
         end;

         declare
            Total_Pop : Real := 0.0;
         begin
            for Pop of System.Pops loop
               Total_Pop := Total_Pop + Real (Pop.Size);
            end loop;
            Result.Add_Property
              ("Population",
               Lui.Approximate_Image (Total_Pop));
         end;

         declare
            Star : constant Concorde.Stars.Star_Type :=
                     Concorde.Stars.Star_Type (System.Main_Object);
         begin
            Result.Add_Property ("Primary", Star.Stellar_Class);
            Result.Add_Property ("Solar masses",
                                 Lui.Approximate_Image (Star.Solar_Masses));
         end;

         Result.Add_Property ("Resource", System.Deposit.Resource.Name);
         Result.Add_Property ("Size",
                              Concorde.Quantities.Image (System.Deposit.Size));
         Result.Add_Property ("Accessibility",
                              Lui.Approximate_Image
                                (System.Deposit.Accessibility));
         Result.Add_Property ("Concentration",
                              Lui.Approximate_Image
                                (System.Deposit.Concentration));

         if System.Has_Government then
            Result.Add_Property ("Government cash",
                                 Concorde.Money.Image
                                   (System.Government.Cash));
         end if;

         System_Models.Insert (System.Name, Result);
      else
         Result := System_Models.Element (System.Name);
      end if;

      return Lui.Models.Object_Model (Result);
   end System_Model;

end Concorde.Systems.Models;
