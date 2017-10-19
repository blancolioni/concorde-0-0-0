with Ada.Containers.Vectors;

with Memor.Element_Vectors;

with Lui.Colours;
with Lui.Rendering;
with Lui.Tables;

with Concorde.Watchers;

with Concorde.Calendar;
with Concorde.Elementary_Functions;
with Concorde.Factions;
with Concorde.Factions.Db;
with Concorde.Factions.History;

with Concorde.Ships.Battles;
with Concorde.Ships.Lists;

with Concorde.Systems.Db;
with Concorde.Systems.Models;

with Concorde.Stars;

with Concorde.Galaxy.Locking;

with Concorde.Updates;

with Concorde.Options;

with Concorde.Trades;

package body Concorde.Galaxy.Model is

   Zoom_Limit : constant := 5.0;

   function Ship_Count_Image
     (Count : Natural)
      return String;

   subtype Faction_Column is Integer range 1 .. 3;

   type Faction_Table is
     new Lui.Tables.Root_Model_Table with null record;

   overriding function Heading_Column_Text
     (Table : Faction_Table;
      Col   : Positive)
      return String
   is ((case Faction_Column (Col) is
           when 1 => "Name",
           when 2 => "Systems",
           when 3 => "Ships"));

   overriding function Cell_Text
     (Table : Faction_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   System_Column_Count : constant := 2;

   subtype System_Column is Integer range 1 .. System_Column_Count;

   package System_Table_Row_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Concorde.Systems.Star_System_Type,
        "="          => Concorde.Systems."=");

   type System_Table is
     new Lui.Tables.Root_Model_Table
     and Concorde.Watchers.Watcher_Interface with
      record
         Rows : System_Table_Row_Vectors.Vector;
      end record;

   overriding procedure On_Object_Changed
     (Table  : in out System_Table;
      Object : Concorde.Watchers.Watched_Object_Interface'Class)
   is null;

   overriding function Heading_Column_Text
     (Table : System_Table;
      Col   : Positive)
      return String
   is ((case System_Column (Col) is
           when 1 => "System",
           when 2 => "Owner"));
--             when 3 => "Tax Income",
--             when 4 => "HQ cash",
--             when 5 => "Payments",
--             when 6 => "Balance"));

   overriding function Cell_Text
     (Table : System_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   overriding function Row_Count
     (Table : System_Table)
      return Natural
   is (Table.Rows.Last_Index);

   subtype Battle_Column is Integer range 1 .. 5;

   type Battle_Table is
     new Lui.Tables.Root_Model_Table with null record;

   overriding function Heading_Column_Text
     (Table : Battle_Table;
      Col   : Positive)
      return String
   is ((case Battle_Column (Col) is
           when 1 => "System",
           when 2 => "Faction 1",
           when 3 => "Fleet 1",
           when 4 => "Faction 2",
           when 5 => "Fleet 2"));

   overriding function Cell_Text
     (Table : Battle_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   overriding function Row_Count
     (Table : Battle_Table)
      return Natural
   is (Battle_Count);

   overriding procedure Select_Row
     (Table : Battle_Table;
      Row   : Positive);

   type Rendered_System is
      record
         X, Y   : Integer;
         Radius : Positive;
         Colour : Lui.Colours.Colour_Type;
      end record;

   package Rendered_System_Vectors is
     new Memor.Element_Vectors
       (Rendered_System, (0, 0, 1, Lui.Colours.Black));

   type Root_Galaxy_Model is
     new Lui.Models.Root_Object_Model
     and Battle_Manager_Interface
     and Concorde.Watchers.Watcher_Interface with
      record
         Show_Capital_Names : Boolean := True;
         Show_System_Names  : Boolean := False;
         Rendered_Systems   : Rendered_System_Vectors.Vector;
         Battles            : Lui.Tables.Model_Table;
         Arena              : access Concorde.Combat.Root_Combat_Arena'Class;
         Needs_Render       : Boolean := True;
         Selected_System    : Concorde.Systems.Star_System_Type;
      end record;

   overriding procedure On_Object_Changed
     (Model  : in out Root_Galaxy_Model;
      Object : Concorde.Watchers.Watched_Object_Interface'Class);

   overriding function Handle_Update
     (Model    : in out Root_Galaxy_Model)
      return Boolean
   is (Model.Needs_Render);

   overriding procedure Render
     (Model    : in out Root_Galaxy_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   overriding procedure On_Model_Removed
     (Model : in out Root_Galaxy_Model;
      Child : not null access Lui.Models.Root_Object_Model'Class);

   overriding function Select_XY
     (Model : in out Root_Galaxy_Model;
      X, Y  : Natural)
      return Lui.Models.Object_Model;

   overriding procedure Zoom
     (Model   : in out Root_Galaxy_Model;
      Z       : in     Integer;
      Control : in     Boolean);

   overriding function Tooltip
     (Model : Root_Galaxy_Model;
      X, Y  : Natural)
      return String;

   overriding procedure On_Battle_End
     (Model   : in out Root_Galaxy_Model;
      Battle  : not null access Concorde.Combat.Root_Combat_Arena'Class);

   overriding procedure After_Transition
     (Model : in out Root_Galaxy_Model);

   procedure Draw_Connection
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      A, B     : Positive);

   procedure Draw_Influence
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      System   : Concorde.Systems.Star_System_Type);

   procedure Draw_Ships
     (Model         : in out Root_Galaxy_Model'Class;
      Renderer      : in out Lui.Rendering.Root_Renderer'Class;
      System        : Concorde.Systems.Star_System_Type;
      System_Radius : Positive;
      Ships         : Concorde.Ships.Lists.List);

   procedure Draw_History
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   procedure Star_System_Screen
     (Model    : in out Root_Galaxy_Model'Class;
      System   : Concorde.Systems.Star_System_Type;
      X, Y     : out Integer);

   function Closest_System
     (Model        : Root_Galaxy_Model'Class;
      X, Y         : Integer;
      Max_Distance : Natural)
      return Concorde.Systems.Star_System_Type;

   function Recent_Battle
     (System   : Concorde.Systems.Star_System_Type;
      Max_Days : Positive)
      return Boolean;

   Unexplored_Colour : constant Lui.Colours.Colour_Type :=
                         (0.5, 0.5, 0.5, 0.6);
   Border_Colour     : constant Lui.Colours.Colour_Type :=
                         (1.0, 1.0, 1.0, 1.0);

   type Galaxy_Model_Access is access all Root_Galaxy_Model'Class;

   Local_Model : Galaxy_Model_Access;

   ----------------------
   -- After_Transition --
   ----------------------

   overriding procedure After_Transition
     (Model : in out Root_Galaxy_Model)
   is
   begin
      Model.Add_Inline_Model
        (Width         => Model.Width,
         Height        => Model.Height,
         Model         =>
           Concorde.Systems.Models.System_Model (Model.Selected_System),
         Attach_Left   => True,
         Attach_Right  => True,
         Attach_Top    => True,
         Attach_Bottom => True);
   end After_Transition;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Faction_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      pragma Unreferenced (Table);
      use Concorde.Factions;
      E : constant Faction_Type :=
            Get (By_Star_Systems, Row);
   begin
      case Faction_Column (Col) is
         when 1 =>
            return E.Name;
         when 2 =>
            declare
               Count : Natural := 0;
            begin
               for I in 1 .. Galaxy_Graph.Last_Vertex_Index loop
                  if Galaxy_Graph.Vertex (I).Owner = E then
                     Count := Count + 1;
                  end if;
               end loop;
               return Natural'Image (Count);
            end;
         when 3 =>
            return Lui.Approximate_Image (E.Current_Ships);
      end case;
   end Cell_Text;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : System_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      System : constant Concorde.Systems.Star_System_Type :=
                 Table.Rows.Element (Row);
   begin
      case System_Column (Col) is
         when 1 =>
            return System.Name;
         when 2 =>
            return System.Owner.Name;
--           when 3 =>
--              return WL.Money.Image
--                (System.Government.Tax_Receipts
--                   (Concorde.Trades.Sales));
--           when 4 =>
--              return WL.Money.Image
--                (System.Government.Headquarters.Cash);
--           when 5 =>
--              return "";
--           when 6 =>
--              return WL.Money.Image
--                (System.Government.Cash);
      end case;
   end Cell_Text;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Battle_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      pragma Unreferenced (Table);
      Arena : constant Concorde.Combat.Ship_Combat.Space_Combat_Arena :=
                Get_Battle (Row);
   begin
      case Battle_Column (Col) is
         when 1 =>
            return Arena.Name;
         when 2 =>
            return Arena.Factions (1).Name;
         when 3 =>
            return Lui.Approximate_Image
              (Arena.Fleet_Size (Arena.Factions (1)));
         when 4 =>
            return Arena.Factions (2).Name;
         when 5 =>
            return Lui.Approximate_Image
              (Arena.Fleet_Size (Arena.Factions (2)));
      end case;
   end Cell_Text;

   --------------------
   -- Closest_System --
   --------------------

   function Closest_System
     (Model        : Root_Galaxy_Model'Class;
      X, Y         : Integer;
      Max_Distance : Natural)
      return Concorde.Systems.Star_System_Type
   is
      Shortest_Distance : Natural := Natural'Last;
      Closest_Reference : Memor.Database_Reference;

      procedure Update
        (Reference : Memor.Database_Reference;
         System    : Rendered_System);

      ------------
      -- Update --
      ------------

      procedure Update
        (Reference : Memor.Database_Reference;
         System    : Rendered_System)
      is
      begin
         if System.X in 1 .. Model.Width
           and then System.Y in 1 .. Model.Height
           and then abs (X - System.X) <= Shortest_Distance
           and then abs (Y - System.Y) <= Shortest_Distance
         then
            declare
               D : constant Natural :=
                     (X - System.X) ** 2 + (Y - System.Y) ** 2;
            begin
               if D < Shortest_Distance then
                  Shortest_Distance := D;
                  Closest_Reference := Reference;
               end if;
            end;
         end if;
      end Update;

   begin
      Model.Rendered_Systems.Iterate (Update'Access);

      if Max_Distance ** 2 >= Shortest_Distance then
         return Concorde.Systems.Db.Element (Closest_Reference);
      else
         return null;
      end if;
   end Closest_System;

   ---------------------
   -- Draw_Connection --
   ---------------------

   procedure Draw_Connection
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      A, B     : Positive)
   is
      use Concorde.Factions;
      use Concorde.Systems;
      A_System : constant Star_System_Type := Galaxy_Graph.Vertex (A);
      B_System : constant Star_System_Type := Galaxy_Graph.Vertex (B);
      A_Owner  : constant Concorde.Factions.Faction_Type := A_System.Owner;
      B_Owner  : constant Concorde.Factions.Faction_Type := B_System.Owner;

      Link_Colour    : constant Lui.Colours.Colour_Type :=
                         (if A_Owner /= null and then B_Owner = A_Owner
                          then A_Owner.Colour
                          elsif A_Owner = null or else B_Owner = null
                          then Unexplored_Colour
                          else Border_Colour);
      Link_Width     : constant Positive :=
                         Natural'Min
                           (A_System.Traffic (B_System)
                            + B_System.Traffic (A_System),
                            5)
                         + 1;
      X1, X2, Y1, Y2 : Integer;
   begin
      Model.Star_System_Screen (A_System, X1, Y1);
      Model.Star_System_Screen (B_System, X2, Y2);
      Renderer.Draw_Line (X1, Y1, X2, Y2, Link_Colour, Link_Width);
   end Draw_Connection;

   ------------------
   -- Draw_History --
   ------------------

   procedure Draw_History
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      use Concorde.Calendar;
      Start : Day_Index := 1;
      Today : constant Day_Index := Get_Day (Current_Date);
      Width : constant := 100.0;
      package Relative_Power_Vectors is
        new Memor.Element_Vectors (Non_Negative_Real, 0.0);
      Total : Non_Negative_Real;
      X     : Integer;
   begin
      if Natural (Today) > Model.Height then
         Start := Day_Index (Natural (Today) - Model.Height + 1);
      end if;

      for Date in Day_Index range Start .. Today - 2 loop
         declare
            Xs    : Relative_Power_Vectors.Vector;

            procedure Draw_Power
              (Faction : Concorde.Factions.Faction_Type);

            procedure Update_Power
              (Faction : Concorde.Factions.Faction_Type);

            ----------------
            -- Draw_Power --
            ----------------

            procedure Draw_Power
              (Faction : Concorde.Factions.Faction_Type)
            is
               New_X : constant Natural :=
                         X +
                           Natural (Xs.Element (Faction.Reference)
                                    / Total * Width);
            begin
               Renderer.Draw_Line
                 (X1         => X,
                  Y1         => Natural (Date - Start),
                  X2         => New_X,
                  Y2         => Natural (Date - Start),
                  Colour     => Faction.Colour,
                  Line_Width => 1);
               X := New_X;
            end Draw_Power;

            ------------------
            -- Update_Power --
            ------------------

            procedure Update_Power
              (Faction : Concorde.Factions.Faction_Type)
            is
               Power : constant Non_Negative_Real :=
                         Concorde.Factions.History.Get_Metric
                           (Date,
                            Concorde.Factions.History.Controlled_Systems,
                            Faction);
            begin
               Xs.Replace_Element
                 (Faction.Reference, Power);
               Total := Total + Power;
            end Update_Power;

         begin
            Total := 0.0;
            Concorde.Factions.Db.Scan (Update_Power'Access);

            X := Model.Width - Natural (Width);
            Concorde.Factions.Db.Scan (Draw_Power'Access);

         end;
      end loop;
   end Draw_History;

   --------------------
   -- Draw_Influence --
   --------------------

   procedure Draw_Influence
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      System   : Concorde.Systems.Star_System_Type)
   is
      Boundary : constant Concorde.Systems.System_Influence_Boundary :=
                   System.Influence_Boundary;
      Points   : Lui.Rendering.Buffer_Points (Boundary'Range);
      Bg       : Lui.Colours.Colour_Type := System.Owner.Colour;
   begin
      for I in Points'Range loop
         declare
            X : constant Real := Boundary (I).X;
            Y : constant Real := Boundary (I).Y;
            Z : Real;
         begin
            Model.Get_Screen_Coordinates
              (X, Y, 0.0,
               Points (I).X, Points (I).Y, Z);
         end;
      end loop;

      Bg.Alpha := System.Loyalty * 0.3;
      Renderer.Draw_Polygon
        (Vertices => Points,
         Colour   => Bg,
         Filled   => True);

   end Draw_Influence;

   ----------------
   -- Draw_Ships --
   ----------------

   procedure Draw_Ships
     (Model         : in out Root_Galaxy_Model'Class;
      Renderer      : in out Lui.Rendering.Root_Renderer'Class;
      System        : Concorde.Systems.Star_System_Type;
      System_Radius : Positive;
      Ships         : Concorde.Ships.Lists.List)
   is
      Es   : constant Concorde.Factions.Array_Of_Factions :=
               Concorde.Ships.Battles.Factions_Present (Ships);
      X, Y : Integer;
   begin
      Model.Star_System_Screen (System, X, Y);
      Y := Y + System_Radius + 12;
      for E of Es loop
         Renderer.Draw_String
           (X, Y, 10, E.Colour,
            Ship_Count_Image
              (Concorde.Ships.Battles.Faction_Ship_Count
                   (E, Ships)));
         Y := Y + 12;
      end loop;
   end Draw_Ships;

   ------------------
   -- Galaxy_Model --
   ------------------

   function Galaxy_Model
     return Lui.Models.Object_Model
   is
      use Lui.Models;

   begin
      if Local_Model = null then
         declare
            Result  : Root_Galaxy_Model;
            E_Table : Faction_Table;
            B_Table : Battle_Table;
            S_Table : System_Table;
            E, S, B : Lui.Tables.Model_Table;
            pragma Unreferenced (S);

            procedure Watch_System
              (System : in out Concorde.Systems.Root_Star_System_Type'Class);

            ------------------
            -- Watch_System --
            ------------------

            procedure Watch_System
              (System : in out Concorde.Systems.Root_Star_System_Type'Class)
            is
            begin
               System.Add_Watcher (Local_Model);
            end Watch_System;

         begin
            E_Table.Initialise
              (Name     => "Factions",
               Num_Rows => Concorde.Factions.Db.Active_Count,
               Num_Cols => Faction_Column'Last);
            E := new Faction_Table'(E_Table);

            S_Table.Initialise
              (Name     => "Systems",
               Num_Rows => Concorde.Factions.Db.Active_Count,
               Num_Cols => System_Column_Count);

--              declare
--                 procedure Add_Capital
--                   (Faction : Concorde.Factions.Faction_Type);
--
--                 -----------------
--                 -- Add_Capital --
--                 -----------------
--
--                 procedure Add_Capital
--                   (Faction : Concorde.Factions.Faction_Type)
--                 is
--                 begin
--                    S_Table.Rows.Append (Faction.Capital);
--                 end Add_Capital;
--
--              begin
--                 Concorde.Factions.Db.Scan (Add_Capital'Access);
--              end;

            S := new System_Table'(S_Table);

            B_Table.Initialise
              ("Battles", 0, Battle_Column'Last);
            B := new Battle_Table'(B_Table);

            Result.Battles := B;
            Result.Initialise
              ("Galaxy",
               Last_Render_Layer => 3,
               Tables            => (E, B));

            Result.Drag_Rotation_Behaviour;

            Result.Set_Eye_Position (0.0, 0.0, 2.2);
            Result.Show_Capital_Names :=
              Concorde.Options.Show_Capital_Names;
            Result.Show_System_Names :=
              Concorde.Options.Show_System_Names;
            Local_Model := new Root_Galaxy_Model'(Result);
            Set_Battle_Manager (Battle_Manager (Local_Model));

            Concorde.Systems.Db.Iterate (Watch_System'Access);

         end;
      end if;
      return Lui.Models.Object_Model (Local_Model);
   end Galaxy_Model;

   -------------------
   -- On_Battle_End --
   -------------------

   overriding procedure On_Battle_End
     (Model   : in out Root_Galaxy_Model;
      Battle  : not null access Concorde.Combat.Root_Combat_Arena'Class)
   is
   begin
      if Model.Arena = Battle then
         Model.Arena := null;
         Model.Remove_Inline_Model (Battle);
      end if;
   end On_Battle_End;

   ----------------------
   -- On_Model_Removed --
   ----------------------

   overriding procedure On_Model_Removed
     (Model : in out Root_Galaxy_Model;
      Child : not null access Lui.Models.Root_Object_Model'Class)
   is
   begin
      if Child.all in
        Concorde.Combat.Ship_Combat.Root_Space_Combat_Arena'Class
        and then Model.Arena /= null
      then
         Complete_Battle (Model.Arena);
         Model.Arena := null;
      end if;
   end On_Model_Removed;

   -----------------------
   -- On_Object_Changed --
   -----------------------

   overriding procedure On_Object_Changed
     (Model  : in out Root_Galaxy_Model;
      Object : Concorde.Watchers.Watched_Object_Interface'Class)
   is
      pragma Unreferenced (Object);
   begin
      Model.Needs_Render := True;
   end On_Object_Changed;

   -------------------
   -- Recent_Battle --
   -------------------

   function Recent_Battle
     (System   : Concorde.Systems.Star_System_Type;
      Max_Days : Positive)
      return Boolean
   is
      use Concorde.Calendar;
   begin
      return System.Last_Battle /= Zero_Date
        and then Get_Day (Current_Date) - Get_Day (System.Last_Battle)
        <= Day_Index (Max_Days);
   end Recent_Battle;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Root_Galaxy_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      use type Lui.Rendering.Render_Layer;
   begin
      Concorde.Updates.Begin_Render;

      if Renderer.Current_Render_Layer = 1 then
         for Star_Pass in Boolean loop
            for I in 1 .. Galaxy_Graph.Last_Vertex_Index loop
               declare
                  use type Concorde.Factions.Faction_Type;
                  use Concorde.Systems;
                  System : constant Star_System_Type :=
                             Galaxy_Graph.Vertex (I);
                  X      : constant Real := System.X;
                  Y      : constant Real := System.Y;
                  Z      : constant Real := System.Z;
                  Radius : Non_Negative_Real :=
                             Concorde.Elementary_Functions.Sqrt
                               (System.Main_Object.Radius);
                  System_Radius : constant Positive :=
                                    Natural'Max (1, Natural (Radius));
                  Screen_X : Integer;
                  Screen_Y : Integer;
                  Screen_Z : Real;
                  Owner    : constant Concorde.Factions.Faction_Type :=
                               System.Owner;
                  Colour   : Lui.Colours.Colour_Type :=
                               (if Owner /= null
                                then Owner.Colour
                                else System.Main_Object.Colour);
               begin
                  Model.Get_Screen_Coordinates
                    (X, Y, Z, Screen_X, Screen_Y, Screen_Z);

                  if Screen_Z not in -6.0 .. 0.0 then
                     null;
                  elsif Star_Pass then

                     Radius :=
                       (if Screen_Z > -3.0
                        then Real'Max (Radius * (Screen_Z + 3.0) / 2.0, 1.0)
                        else 1.0);
                     Model.Rendered_Systems.Replace_Element
                       (System.Reference,
                        (Screen_X, Screen_Y, System_Radius, Colour));

                     if System.Owned then
                        Draw_Influence
                          (Model, Renderer, System);
                     end if;

                     if Recent_Battle (System, 5) then
                        declare
                           use Concorde.Calendar;
                           use Concorde.Elementary_Functions;
                           Size : constant Positive :=
                                    Positive'Max
                                      (Natural
                                         (Sqrt
                                            (Real (System.Last_Battle_Size))),
                                       10);
                           Days : constant Natural :=
                                    Natural
                                      (Get_Day (Concorde.Calendar.Clock))
                                    - Natural
                                      (Get_Day (System.Last_Battle));
                        begin
                           if Days < Size then
                              Renderer.Draw_Circle
                                (X          => Screen_X,
                                 Y          => Screen_Y,
                                 Radius     => Size - Days,
                                 Colour     => (1.0, 0.0, 0.0,
                                                1.0
                                                - Real (Days)
                                                / Real (Size)),
                                 Filled     => True,
                                 Line_Width => 1);
                           end if;
                        end;
                     end if;

                     if Screen_Z >= -0.5 then
                        Colour :=
                          Lui.Colours.Brighten (Colour, Screen_Z + 1.0);
                     elsif Screen_Z > -3.2 then
                        Colour :=
                          Lui.Colours.Apply_Alpha
                            (Colour, (4.0 + Screen_Z) / 4.0);
                     else
                        Colour :=
                          Lui.Colours.Apply_Alpha
                            (Colour, 0.2);
                     end if;

                     Renderer.Draw_Circle
                       (X          => Screen_X,
                        Y          => Screen_Y,
                        Radius     => Natural'Max (Natural (Radius), 1),
                        Colour     => Colour,
                        Filled     => True,
                        Line_Width => 1);

                     if Model.Show_System_Names
                       or else (System.Capital
                                and then Model.Show_Capital_Names)
                       or else System.Name'Length < 5
                       or else System.Name (1 .. 4) /= "Star"
                     then
                        Renderer.Draw_String
                          (X      => Screen_X - 20,
                           Y      => Screen_Y - 10,
                           Size   => 12,
                           Colour => Lui.Colours.White,
                           Text   => System.Name);
                     end if;

                     if System.Ships > 0 then
                        declare
                           Ships : Concorde.Ships.Lists.List;
                        begin
                           System.Get_Ships (Ships);
                           Model.Draw_Ships (Renderer, System,
                                             System_Radius, Ships);
                        end;
                     end if;

                     declare
                        --  Radius : Positive := 8;

                        procedure Draw_Claims
                          (Faction : Concorde.Factions.Root_Faction_Type'Class);

                        -----------------
                        -- Draw_Claims --
                        -----------------

                        procedure Draw_Claims
                          (Faction : Concorde.Factions.Root_Faction_Type'Class)
                        is null;
--                             use Concorde.Factions;
--                          begin
--                           if Faction.Is_Set (System, Claim) then
--                               and then not Faction.Owned_System (System)
--                             then
--                                Renderer.Draw_Circle
--                                  (X          => Screen_X,
--                                   Y          => Screen_Y,
--                                   Radius     => Radius,
--                                   Colour     => Faction.Colour,
--                                   Filled     => False,
--                                   Line_Width => 1);
--                                Radius := Radius + 2;
--                             end if;
--                          end Draw_Claims;

                     begin
                        Concorde.Factions.Db.Scan (Draw_Claims'Access);
                     end;

                     if False and then Owner /= null then
                        declare
                           Colour : Lui.Colours.Colour_Type := Owner.Colour;
                        begin
                           Colour.Alpha := 0.4;
                           Renderer.Draw_Circle
                             (X          => Screen_X,
                              Y          => Screen_Y,
                              Radius     => 30,
                              Colour     => Colour,
                              Filled     => True,
                              Line_Width => 1);
                        end;
                     end if;
                  else

                     declare

                        procedure Draw_Connection
                          (To   : Star_System_Type;
                           Cost : Non_Negative_Real);

                        ---------------------
                        -- Draw_Connection --
                        ---------------------

                        procedure Draw_Connection
                          (To   : Star_System_Type;
                           Cost : Non_Negative_Real)
                        is
                           pragma Unreferenced (Cost);
                        begin
                           if To.Index > System.Index then
                              Concorde.Galaxy.Locking.Lock_System
                                (System.all, False);
                              Concorde.Galaxy.Locking.Lock_System
                                (To.all, False);
                              Model.Draw_Connection
                                (Renderer, System.Index, To.Index);
                              Concorde.Galaxy.Locking.Unlock_System
                                (To.all);
                              Concorde.Galaxy.Locking.Unlock_System
                                (System.all);
                           end if;
                        end Draw_Connection;

                     begin
                        Galaxy_Graph.Iterate_Edges_From_Vertex
                          (Galaxy_Graph.Vertex (I), Draw_Connection'Access);
                     end;
                  end if;
               end;
            end loop;
         end loop;
      elsif Renderer.Current_Render_Layer = 2 then
         declare
            use Concorde.Calendar;
         begin
            if Get_Day (Current_Date) > 10 then
               Model.Draw_History (Renderer);
            end if;
         end;

         Model.Needs_Render := False;

      end if;

      Concorde.Updates.Finish_Render;

   end Render;

   ----------------
   -- Select_Row --
   ----------------

   overriding procedure Select_Row
     (Table : Battle_Table;
      Row   : Positive)
   is
      pragma Unreferenced (Table);
      Battle : constant Concorde.Combat.Ship_Combat.Space_Combat_Arena :=
                 Get_Battle (Row);
   begin
      Root_Galaxy_Model (Local_Model.all).Arena := Battle;
      Local_Model.Add_Inline_Model
        (Width         => Local_Model.Width / 2,
         Height        => Local_Model.Height / 2,
         Model         => Battle,
         Attach_Left   => True,
         Attach_Right  => True,
         Attach_Top    => True,
         Attach_Bottom => True);
   end Select_Row;

   ---------------
   -- Select_XY --
   ---------------

   overriding function Select_XY
     (Model : in out Root_Galaxy_Model;
      X, Y  : Natural)
      return Lui.Models.Object_Model
   is
      use type Concorde.Systems.Star_System_Type;
      System : constant Concorde.Systems.Star_System_Type :=
                 Model.Closest_System (X, Y, 10);
   begin
      if Model.Arena /= null then
         Complete_Battle (Model.Arena);
      end if;

      if System /= null then
         if System /= Model.Selected_System then
            Model.Selected_System := System;
            Model.Start_Transition
              (System.X, System.Y, System.Z + 0.05, 2.0);
         end if;
         return null;
      else
         return null;
      end if;

   end Select_XY;

   ----------------------
   -- Ship_Count_Image --
   ----------------------

   function Ship_Count_Image
     (Count : Natural)
      return String
   is
   begin
      if Count < 10 then
         return Result : constant String (1 .. Count) := (others => 'I') do
            null;
         end return;
      elsif Count < 100 then
         declare
            Xs : constant String (1 .. Count / 10) := (others => 'X');
         begin
            return Xs & Ship_Count_Image (Count mod 10);
         end;
      else
         declare
            Result : constant String := Natural'Image (Count);
         begin
            return Result (2 .. Result'Last);
         end;
      end if;
   end Ship_Count_Image;

   ------------------------
   -- Star_System_Screen --
   ------------------------

   procedure Star_System_Screen
     (Model    : in out Root_Galaxy_Model'Class;
      System   : Concorde.Systems.Star_System_Type;
      X, Y     : out Integer)
   is
      Screen_Z : Real;
   begin
      Model.Get_Screen_Coordinates (System.X, System.Y, 0.0,
                                    X, Y, Screen_Z);
   end Star_System_Screen;

   -------------
   -- Tooltip --
   -------------

   overriding function Tooltip
     (Model : Root_Galaxy_Model;
      X, Y  : Natural)
      return String
   is
      use type Concorde.Systems.Star_System_Type;
      System : constant Concorde.Systems.Star_System_Type :=
                 Model.Closest_System (X, Y, 10);
      Star : Concorde.Stars.Star_Type;
   begin
      if System /= null then
         Star :=
           Concorde.Stars.Star_Type
             (System.Main_Object);
         return System.Name & " "
           & Star.Stellar_Class;
      else
         return "";
      end if;
   end Tooltip;

   ----------
   -- Zoom --
   ----------

   overriding procedure Zoom
     (Model   : in out Root_Galaxy_Model;
      Z       : in     Integer;
      Control : in     Boolean)
   is
   begin
      Lui.Models.Root_Object_Model (Model).Zoom (Z, Control);
      if Model.Eye_Z > Zoom_Limit then
         Model.Set_Eye_Position (Model.Eye_X, Model.Eye_Y, Zoom_Limit);
      end if;
   end Zoom;

end Concorde.Galaxy.Model;
