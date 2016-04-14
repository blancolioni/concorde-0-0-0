with Ada.Calendar;

with Memor.Element_Vectors;

with Lui.Colours;
with Lui.Rendering;
with Lui.Tables;

with Concorde.Dates;
with Concorde.Elementary_Functions;
with Concorde.Empires;
with Concorde.Empires.Db;
with Concorde.Empires.History;

with Concorde.Ships.Battles;
with Concorde.Ships.Lists;

with Concorde.Systems.Db;
with Concorde.Systems.Models;

with Concorde.Galaxy.Locking;

with Concorde.Updates;

with Concorde.Options;

package body Concorde.Galaxy.Model is

   System_Radius : constant := 5;

   function Ship_Count_Image
     (Count : Natural)
      return String;

   subtype Empire_Column is Integer range 1 .. 3;

   type Empire_Table is
     new Lui.Tables.Root_Model_Table with null record;

   overriding function Heading_Column_Text
     (Table : Empire_Table;
      Col   : Positive)
      return String
   is ((case Empire_Column (Col) is
           when 1 => "Name",
           when 2 => "Systems",
           when 3 => "Ships"));

   overriding function Cell_Text
     (Table : Empire_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   subtype Battle_Column is Integer range 1 .. 5;

   type Battle_Table is
     new Lui.Tables.Root_Model_Table with null record;

   overriding function Heading_Column_Text
     (Table : Battle_Table;
      Col   : Positive)
      return String
   is ((case Battle_Column (Col) is
           when 1 => "System",
           when 2 => "Empire 1",
           when 3 => "Fleet 1",
           when 4 => "Empire 2",
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
         X, Y : Integer;
      end record;

   package Rendered_System_Vectors is
     new Memor.Element_Vectors (Rendered_System, (0, 0));

   type Root_Galaxy_Model is
     new Lui.Models.Root_Object_Model
     and Battle_Manager_Interface with
      record
         Frames             : Natural := 0;
         Start              : Ada.Calendar.Time;
         FPS                : Real;
         Show_Capital_Names : Boolean := True;
         Show_System_Names  : Boolean := False;
         Rendered_Systems   : Rendered_System_Vectors.Vector;
         Battles            : Lui.Tables.Model_Table;
         Arena              : access Concorde.Combat.Root_Combat_Arena'Class;
      end record;

   overriding function Handle_Update
     (Model    : in out Root_Galaxy_Model)
      return Boolean
   is (True);

   overriding procedure Render
     (Model    : in out Root_Galaxy_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   overriding function Get_Drag_Behaviour
     (Model : Root_Galaxy_Model)
      return Lui.Models.Drag_Behaviour
   is (Lui.Models.Translation);

   overriding procedure On_Model_Removed
     (Model : in out Root_Galaxy_Model;
      Child : not null access Lui.Models.Root_Object_Model'Class);

   overriding function Select_XY
     (Model : Root_Galaxy_Model;
      X, Y  : Natural)
      return Lui.Models.Object_Model;

   overriding function Tooltip
     (Model : Root_Galaxy_Model;
      X, Y  : Natural)
      return String;

   overriding procedure On_Battle_End
     (Model   : in out Root_Galaxy_Model;
      Battle  : not null access Concorde.Combat.Root_Combat_Arena'Class);

   procedure Draw_Connection
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      A, B     : Positive);

   procedure Draw_Influence
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      System   : Concorde.Systems.Star_System_Type);

   procedure Draw_Ships
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      System   : Concorde.Systems.Star_System_Type;
      Ships    : Concorde.Ships.Lists.List);

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
      Max_Days : Natural)
      return Boolean;

   Unexplored_Colour : constant Lui.Colours.Colour_Type :=
                         (0.5, 0.5, 0.5, 0.6);
   Border_Colour     : constant Lui.Colours.Colour_Type :=
                         (1.0, 1.0, 1.0, 1.0);

   Local_Model : Lui.Models.Object_Model;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Empire_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      pragma Unreferenced (Table);
      use Concorde.Empires;
      E : constant Empire_Type :=
            Get (By_Star_Systems, Row);
   begin
      case Empire_Column (Col) is
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
            return Arena.Empires (1).Name;
         when 3 =>
            return Lui.Approximate_Image
              (Arena.Fleet_Size (Arena.Empires (1)));
         when 4 =>
            return Arena.Empires (2).Name;
         when 5 =>
            return Lui.Approximate_Image
              (Arena.Fleet_Size (Arena.Empires (2)));
      end case;
   end Cell_Text;

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
         D : constant Natural :=
               (X - System.X) ** 2 + (Y - System.Y) ** 2;
      begin
         if D < Shortest_Distance then
            Shortest_Distance := D;
            Closest_Reference := Reference;
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
      use Concorde.Empires;
      use Concorde.Systems;
      A_System : constant Star_System_Type := Galaxy_Graph.Vertex (A);
      B_System : constant Star_System_Type := Galaxy_Graph.Vertex (B);
      A_Owner  : constant Concorde.Empires.Empire_Type := A_System.Owner;
      B_Owner  : constant Concorde.Empires.Empire_Type := B_System.Owner;

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
      use Concorde.Dates;
      Start : Date_Type := 1;
      Width : constant := 100.0;
      package Relative_Power_Vectors is
        new Memor.Element_Vectors (Non_Negative_Real, 0.0);
      Total : Non_Negative_Real;
      X     : Integer;
   begin
      if Natural (Current_Date) > Model.Height then
         Start := Date_Type (Natural (Current_Date) - Model.Height + 1);
      end if;

      for Date in Date_Type range Start .. Current_Date - 2 loop
         declare
            Xs    : Relative_Power_Vectors.Vector;

            procedure Draw_Power
              (Empire : Concorde.Empires.Empire_Type);

            procedure Update_Power
              (Empire : Concorde.Empires.Empire_Type);

            ----------------
            -- Draw_Power --
            ----------------

            procedure Draw_Power
              (Empire : Concorde.Empires.Empire_Type)
            is
               New_X : constant Natural :=
                         X +
                           Natural (Xs.Element (Empire.Reference)
                                    / Total * Width);
            begin
               Renderer.Draw_Line
                 (X1         => X,
                  Y1         => Natural (Date - Start),
                  X2         => New_X,
                  Y2         => Natural (Date - Start),
                  Colour     => Empire.Colour,
                  Line_Width => 1);
               X := New_X;
            end Draw_Power;

            ------------------
            -- Update_Power --
            ------------------

            procedure Update_Power
              (Empire : Concorde.Empires.Empire_Type)
            is
               Power : constant Non_Negative_Real :=
                         Concorde.Empires.History.Get_Metric
                           (Date, Concorde.Empires.History.Capacity, Empire);
            begin
               Xs.Replace_Element
                 (Empire.Reference, Power);
               Total := Total + Power;
            end Update_Power;

         begin
            Total := 0.0;
            Concorde.Empires.Db.Scan (Update_Power'Access);

            X := Model.Width - Natural (Width);
            Concorde.Empires.Db.Scan (Draw_Power'Access);

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
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      System   : Concorde.Systems.Star_System_Type;
      Ships    : Concorde.Ships.Lists.List)
   is
      Es   : constant Concorde.Empires.Array_Of_Empires :=
               Concorde.Ships.Battles.Empires_Present (Ships);
      X, Y : Integer;
   begin
      Model.Star_System_Screen (System, X, Y);
      Y := Y + System_Radius + 12;
      for E of Es loop
         Renderer.Draw_String
           (X, Y, 10, E.Colour,
            Ship_Count_Image
              (Concorde.Ships.Battles.Empire_Ship_Count
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
            E_Table : Empire_Table;
            B_Table : Battle_Table;
            E, B    : Lui.Tables.Model_Table;
         begin
            E_Table.Initialise
              (Name     => "Empires",
               Num_Rows => Concorde.Empires.Db.Active_Count,
               Num_Cols => Empire_Column'Last);

            E := new Empire_Table'(E_Table);
            B_Table.Initialise
              ("Battles", 0, Battle_Column'Last);
            B := new Battle_Table'(B_Table);

            Result.Battles := B;
            Result.Initialise ("Galaxy", (E, B));

            Result.Set_Eye_Position (0.0, 0.0, 2.2);
            Result.Show_Capital_Names :=
              Concorde.Options.Show_Capital_Names;
            Result.Show_System_Names :=
              Concorde.Options.Show_System_Names;
            Local_Model := new Root_Galaxy_Model'(Result);
            Set_Battle_Manager (Battle_Manager (Local_Model));
         end;
      end if;
      return Local_Model;
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

   -------------------
   -- Recent_Battle --
   -------------------

   function Recent_Battle
     (System   : Concorde.Systems.Star_System_Type;
      Max_Days : Natural)
      return Boolean
   is
      use Concorde.Dates;
   begin
      return System.Last_Battle > 0
        and then Current_Date - System.Last_Battle <= Date_Type (Max_Days);
   end Recent_Battle;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Root_Galaxy_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
   begin
      if Model.Frames = 0 then
         Model.Start := Ada.Calendar.Clock;
      end if;

      Model.Frames := Model.Frames + 1;

      if Model.Frames > 40 then
         if Model.Frames mod 100 = 0 then
            declare
               use Ada.Calendar;
               Now : constant Time := Clock;
               D   : constant Duration := Now - Model.Start;
            begin
               Model.FPS := Real (Model.Frames) / Real (D);
            end;
         end if;

         declare
            X, Y : Integer;
         begin
            Model.Get_Location (X, Y);
            Renderer.Draw_String
              (X + 4, Y + 20, 14, Lui.Colours.White,
               Lui.Approximate_Image (Model.FPS) & " FPS");
         end;
      end if;

      Concorde.Updates.Begin_Render;

      for Star_Pass in Boolean loop
         for I in 1 .. Galaxy_Graph.Last_Vertex_Index loop
            declare
               use type Concorde.Empires.Empire_Type;
               use Concorde.Systems;
               System : constant Star_System_Type :=
                          Galaxy_Graph.Vertex (I);
               X      : constant Real := System.X;
               Y      : constant Real := System.Y;
               Screen_X : Integer;
               Screen_Y : Integer;
               Screen_Z : Real;
               Owner    : constant Concorde.Empires.Empire_Type :=
                            System.Owner;
               Colour   : constant Lui.Colours.Colour_Type :=
                            (if Owner /= null
                             then Owner.Colour
                             else Lui.Colours.White);
            begin
               Model.Get_Screen_Coordinates
                 (X, Y, 0.0, Screen_X, Screen_Y, Screen_Z);
               if Star_Pass then

                  Model.Rendered_Systems.Replace_Element
                    (System.Reference, (Screen_X, Screen_Y));

                  if System.Owned then
                     Draw_Influence
                       (Model, Renderer, System);
                  end if;

                  if Recent_Battle (System, 5) then
                     declare
                        use Concorde.Elementary_Functions;
                        Size : constant Positive :=
                                 Positive'Max
                                   (Natural
                                      (Sqrt
                                         (Real (System.Last_Battle_Size))),
                                    10);
                        Days : constant Natural :=
                                 Natural (Concorde.Dates.Current_Date)
                                 - Natural (System.Last_Battle);
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

                  Renderer.Draw_Circle
                    (X          => Screen_X,
                     Y          => Screen_Y,
                     Radius     => System_Radius,
                     Colour     => Colour,
                     Filled     => True,
                     Line_Width => 1);

                  if Model.Show_System_Names
                    or else (System.Capital
                             and then Model.Show_Capital_Names)
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
                        Model.Draw_Ships (Renderer, System, Ships);
                     end;
                  end if;

                  declare
                     Radius : Positive := 8;

                     procedure Draw_Claims
                       (Empire : Concorde.Empires.Root_Empire_Type'Class);

                     -----------------
                     -- Draw_Claims --
                     -----------------

                     procedure Draw_Claims
                       (Empire : Concorde.Empires.Root_Empire_Type'Class)
                     is
                        use Concorde.Empires;
                     begin
                        if Empire.Is_Set (System, Claim)
                          and then not Empire.Owned_System (System)
                        then
                           Renderer.Draw_Circle
                             (X          => Screen_X,
                              Y          => Screen_Y,
                              Radius     => Radius,
                              Colour     => Empire.Colour,
                              Filled     => False,
                              Line_Width => 1);
                           Radius := Radius + 2;
                        end if;
                     end Draw_Claims;

                  begin
                     Concorde.Empires.Db.Scan (Draw_Claims'Access);
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
                     Galaxy_Graph.Iterate_Edges
                       (Galaxy_Graph.Vertex (I), Draw_Connection'Access);
                  end;
               end if;
            end;
         end loop;
      end loop;

      declare
         use Concorde.Dates;
      begin
         if Current_Date > 10 then
            Model.Draw_History (Renderer);
         end if;
      end;

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
     (Model : Root_Galaxy_Model;
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
         return Concorde.Systems.Models.System_Model (System);
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
   begin
      if System /= null then
         return System.Name;
      else
         return "";
      end if;
   end Tooltip;

end Concorde.Galaxy.Model;
