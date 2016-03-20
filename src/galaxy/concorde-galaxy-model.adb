with Ada.Calendar;

with Lui.Colours;
with Lui.Rendering;
with Lui.Tables;

with Concorde.AI;
with Concorde.Dates;
with Concorde.Empires;

with Concorde.Galaxy.Locking;

package body Concorde.Galaxy.Model is

   subtype Empire_Column is Integer range 1 .. 4;

   type Empire_Table is
     new Lui.Tables.Root_Model_Table with null record;

   overriding function Heading_Column_Text
     (Table : Empire_Table;
      Col   : Positive)
      return String
   is ((case Empire_Column (Col) is
           when 1 => "Name",
           when 2 => "Systems",
           when 3 => "Ships",
           when 4 => "Attack"));

   overriding function Cell_Text
     (Table : Empire_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   type Root_Galaxy_Model is
     new Lui.Models.Root_Object_Model with
      record
         Frames : Natural := 0;
         Start  : Ada.Calendar.Time;
         FPS    : Real;
      end record;

   overriding procedure Idle_Update
     (Model    : in out Root_Galaxy_Model;
      Updated  : out Boolean);

   overriding procedure Render
     (Model    : in out Root_Galaxy_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   procedure Draw_Connection
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      A, B     : Positive);

   procedure Star_System_Screen
     (Model    : in out Root_Galaxy_Model'Class;
      System   : Concorde.Systems.Star_System_Type;
      X, Y     : out Integer);

   function Recent_Battle
     (System : Concorde.Systems.Star_System_Type;
      Max_Days : Natural)
      return Boolean;

   Unexplored_Colour : constant Lui.Colours.Colour_Type :=
                         (0.5, 0.5, 0.5, 0.6);
   Border_Colour : constant Lui.Colours.Colour_Type :=
                     (1.0, 1.0, 1.0, 1.0);

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
         when 4 =>
            return Lui.Approximate_Image (E.AI.Minimum_Attack_Factor);
      end case;
   end Cell_Text;

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
   -- Galaxy_Model --
   ------------------

   function Galaxy_Model
      return Lui.Models.Object_Model
   is
      Result : Root_Galaxy_Model;
      E_Table : Empire_Table;
      E : Lui.Tables.Model_Table;
   begin
      E_Table.Initialise
        ("Empires", Concorde.Empires.Empire_Count, Empire_Column'Last);
      E := new Empire_Table'(E_Table);
      if True then
         Result.Initialise ("Galaxy", (1 => E));
      else
         Result.Initialise ("Galaxy");
      end if;
      Result.Set_Eye_Position (0.0, 0.0, 2.5);
      return new Root_Galaxy_Model'(Result);
   end Galaxy_Model;

   -----------------
   -- Idle_Update --
   -----------------

   overriding procedure Idle_Update
     (Model    : in out Root_Galaxy_Model;
      Updated  : out Boolean)
   is
      pragma Unreferenced (Model);
   begin
      Updated := True;
   end Idle_Update;

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
      use Concorde.Dates;
      Date : constant Date_Type := Current_Date;

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

         Renderer.Draw_String
           (10, 10, 14, Lui.Colours.White,
            Lui.Approximate_Image (Model.FPS) & " FPS");
      end if;

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

                  if Recent_Battle (System, 5) then
                     declare
                        Size : constant Positive :=
                                 System.Last_Battle_Size;
                        Days : constant Natural :=
                                 Natural (Date - System.Last_Battle);
                     begin
                        if Days < Size then
                           Renderer.Draw_Circle
                             (X          => Screen_X,
                              Y          => Screen_Y,
                              Radius     => Size - Days,
                              Colour     => (1.0, 0.0, 0.0,
                                             1.0 - Real (Days) / Real (Size)),
                              Filled     => True,
                              Line_Width => 1);
                        end if;
                     end;
                  end if;

                  Renderer.Draw_Circle
                    (X          => Screen_X,
                     Y          => Screen_Y,
                     Radius     => (if Owner /= null then 6 else 4),
                     Colour     => Colour,
                     Filled     => True,
                     Line_Width => 1);

                  if System.Capital then
                     Renderer.Draw_String
                       (X      => Screen_X - 20,
                        Y      => Screen_Y - 10,
                        Size   => 12,
                        Colour => Lui.Colours.White,
                        Text   => System.Name);
                  end if;

                  if Owner /= null and then System.Ships > 0 then
                     Renderer.Draw_String
                       (X      => Screen_X + 6,
                        Y      => Screen_Y + 6,
                        Size   => 8,
                        Colour => Colour,
                        Text   => Natural'Image (System.Ships));
                  end if;

                  if Owner /= null and then Owner.Has_Focus (System) then
                     Renderer.Draw_Circle
                       (X          => Screen_X,
                        Y          => Screen_Y,
                        Radius     => 8,
                        Colour     => Colour,
                        Filled     => False,
                        Line_Width => 1);
                  elsif True then
                     declare
                        Radius : Positive := 8;
                     begin
                        for Index in 1 .. Empires.Empire_Count loop
                           declare
                              E : constant Empires.Empire_Type :=
                                    Empires.Get (Index);
                              Focus : constant Boolean := E.Has_Focus (System);
                           begin
                              if Focus then
                                 Renderer.Draw_Circle
                                   (X          => Screen_X,
                                    Y          => Screen_Y,
                                    Radius     => Radius,
                                    Colour     => Empires.Get (Index).Colour,
                                    Filled     => False,
                                    Line_Width => 1);
                                 Radius := Radius + 2;
                              end if;
                           end;
                        end loop;
                     end;
                  end if;
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
                       (To   : Positive;
                        Cost : Non_Negative_Real);

                     ---------------------
                     -- Draw_Connection --
                     ---------------------

                     procedure Draw_Connection
                       (To   : Positive;
                        Cost : Non_Negative_Real)
                     is
                        pragma Unreferenced (Cost);
                     begin
                        if To > System.Index then
                           Concorde.Galaxy.Locking.Lock_System
                             (System, False);
                           Concorde.Galaxy.Locking.Lock_System
                             (Galaxy_Vector (To), False);
                           Model.Draw_Connection
                             (Renderer, System.Index, To);
                           Concorde.Galaxy.Locking.Unlock_System
                             (Galaxy_Vector (To));
                           Concorde.Galaxy.Locking.Unlock_System
                             (System);
                        end if;
                     end Draw_Connection;

                  begin
                     Galaxy_Graph.Iterate_Edges (I, Draw_Connection'Access);
                  end;
               end if;
            end;
         end loop;
      end loop;
   end Render;

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

end Concorde.Galaxy.Model;
