with Ada.Calendar;
with Ada.Containers.Vectors;

with Lui.Colours;
with Lui.Gadgets;
with Lui.Models.Model_3D;
with Lui.Tables;

with Lui.Rendering;

with Concorde.Elementary_Functions;

with Concorde.Components;

with Concorde.Ships.Flight;

with Newton.Flight;

with Concorde.Money;

with Concorde.Commodities.Db;

package body Concorde.Ships.Models is

   Show_Cubes : constant Boolean := True;

   Max_Explode : constant := 10.0;

   Ship_Module_Column_Count : constant := 4;
   subtype Ship_Module_Column is Integer range 1 .. Ship_Module_Column_Count;

   type Ship_Module_Table is
     new Lui.Tables.Root_Model_Table with
      record
         Ship        : Ship_Type;
         Newton_Ship : Concorde.Ships.Flight.Newtonian_Ship;
      end record;

   overriding function Cell_Text
     (Table  : Ship_Module_Table;
      Row    : Positive;
      Column : Positive)
      return String;

   overriding function Heading_Column_Text
     (Table  : Ship_Module_Table;
      Column : Positive)
      return String
   is ((case Ship_Module_Column (Column) is
           when 1 => "Name",
           when 2 => "Type",
           when 3 => "Volume",
           when 4 => "Mass"));

   overriding procedure Select_Row
     (Table : Ship_Module_Table;
      Row   : Positive);

   Ship_Cargo_Column_Count : constant := 4;
   subtype Ship_Cargo_Column is Integer range 1 .. Ship_Cargo_Column_Count;

   package Cargo_Vectors is
     new Ada.Containers.Vectors
       (Positive,
        Concorde.Commodities.Commodity_Type,
        Concorde.Commodities."=");

   type Ship_Cargo_Table is
     new Lui.Tables.Root_Model_Table with
      record
         Ship  : Ship_Type;
         Cargo : Cargo_Vectors.Vector;
      end record;

   overriding function Cell_Text
     (Table  : Ship_Cargo_Table;
      Row    : Positive;
      Column : Positive)
      return String;

   overriding function Heading_Column_Text
     (Table  : Ship_Cargo_Table;
      Column : Positive)
      return String
   is ((case Ship_Module_Column (Column) is
           when 1 => "Commodity",
           when 2 => "Units",
           when 3 => "Mass",
           when 4 => "Value"));

   overriding function Row_Count
     (Table : Ship_Cargo_Table)
      return Natural
   is (Table.Cargo.Last_Index);

   Ship_Engine_Column_Count : constant := 4;
   subtype Ship_Engine_Column is Integer range 1 .. Ship_Engine_Column_Count;

   type Ship_Engine_Table is
     new Lui.Tables.Root_Model_Table with
      record
         Ship        : Ship_Type;
         Newton_Ship : Concorde.Ships.Flight.Newtonian_Ship;
      end record;

   overriding function Cell_Text
     (Table  : Ship_Engine_Table;
      Row    : Positive;
      Column : Positive)
      return String;

   overriding function Heading_Column_Text
     (Table  : Ship_Engine_Table;
      Column : Positive)
      return String;

   type Engine_Toggle_Button is
     new Lui.Gadgets.Root_Button_Gadget with
      record
         Newton_Ship  : Concorde.Ships.Flight.Newtonian_Ship;
         Engine_Index : Positive;
      end record;

   overriding procedure On_Click
     (Button : Engine_Toggle_Button;
      Model  : not null access Lui.Models.Root_Object_Model'Class);

   function Create_Engine_Toggle_Button
     (Newton_Ship  : Concorde.Ships.Flight.Newtonian_Ship;
      Engine_Index : Positive)
      return Lui.Gadgets.Model_Gadget;

   type Rotate_Button is
     new Lui.Gadgets.Root_Button_Gadget with
      record
         Newton_Ship  : Concorde.Ships.Flight.Newtonian_Ship;
         Rotation     : Newton.Flight.Vector_3;
      end record;

   overriding procedure On_Click
     (Button : Rotate_Button;
      Model  : not null access Lui.Models.Root_Object_Model'Class);

   function Create_Rotate_Button
     (Newton_Ship  : Concorde.Ships.Flight.Newtonian_Ship;
      Rotation     : Newton.Flight.Vector_3;
      Label        : String)
      return Lui.Gadgets.Model_Gadget;

   type Ship_Component is
      record
         Module          : Concorde.Modules.Module_Type;
         Colour          : Lui.Colours.Colour_Type;
         Mass            : Non_Negative_Real;
         Orientation     : Lui.Models.Model_3D.Matrix_4;
         Position        : Lui.Models.Model_3D.Vector_3;
         Size            : Lui.Models.Model_3D.Vector_3;
         Shape           : Concorde.Components.Component_Shape;
         Structure_Index : Positive;
         Engine_Index    : Natural;
      end record;

   function To_Orientation_Matrix
     (Orientation : Module_Orientation)
      return Newton.Flight.Matrix_4;

   function Get_Component
     (Mount : Module_Layout_Record;
      Index : Positive)
      return Ship_Component
   is ((Module => Mount.Module,
        Colour => Mount.Module.Component.Colour,
        Mass   => Mount.Module.Mass,
        Orientation => To_Orientation_Matrix (Mount.Orientation),
        Position    => (Real (Mount.Left_Low_Aft.X),
                        Real (Mount.Left_Low_Aft.Y),
                        Real (Mount.Left_Low_Aft.Z)),
        Size        => (Real (Mount.Module.Size.X),
                        Real (Mount.Module.Size.Y),
                        Real (Mount.Module.Size.Z)),
        Shape       => Mount.Module.Component.Shape,
        Structure_Index => Index,
        Engine_Index => 0));

   package Component_Vectors is
     new Ada.Containers.Vectors (Positive, Ship_Component);

   type Ship_Model is
     new Lui.Models.Model_3D.Root_3D_Model with
      record
         Ship         : Ship_Type;
         Newton_Ship  : Flight.Newtonian_Ship;
         Manager      : Newton.Flight.Flight_Manager;
         Components   : Component_Vectors.Vector;
         Explode      : Real := 1.0;
         Last_Update  : Ada.Calendar.Time;
      end record;

   procedure Initialise
     (Model : in out Ship_Model'Class;
      Ship  : in     Ship_Type);

   overriding
   function Tooltip (Model : Ship_Model;
                     X, Y  : Natural)
                     return String;

   overriding
   function Long_Tooltip (Model : Ship_Model;
                          X, Y  : Natural)
                          return String;

   overriding
   function Select_XY (Model : Ship_Model;
                       X, Y  : Natural)
                       return Lui.Models.Object_Model;

   overriding procedure Select_XY
     (Model : in out Ship_Model;
      X, Y  : Natural);

   overriding procedure Create_Scene
     (Model  : in out Ship_Model);

   overriding procedure Render
     (Model    : in out Ship_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   overriding
   procedure Zoom
     (Item    : in out Ship_Model;
      Z       : in     Integer;
      Control : in   Boolean);

   overriding function Handle_Update
     (Model : in out Ship_Model)
      return Boolean
   is (True);

   procedure Draw_Component
     (Model     : in out Ship_Model'Class;
      Matrix    : Lui.Models.Model_3D.Matrix_4;
      Item      : Ship_Component;
      Unit_Size : Real);

   procedure Draw_Cube
     (Model       : in out Ship_Model'Class;
      Colour      : Lui.Colours.Colour_Type;
      X, Y, Z     : Real;
      DX, DY, DZ  : Real;
      Front, Back : Boolean := True;
      Left, Right : Boolean := True;
      Top, Bottom : Boolean := True);

   function Create_Module_Table
     (Newton_Ship : Concorde.Ships.Flight.Newtonian_Ship)
      return Lui.Tables.Model_Table;

   function Create_Cargo_Table
     (Ship : Ship_Type)
      return Lui.Tables.Model_Table;

   function Create_Engine_Table
     (Newton_Ship : Concorde.Ships.Flight.Newtonian_Ship)
      return Lui.Tables.Model_Table;

   function Create_Newton_Ship
     (Ship : Ship_Type)
      return Concorde.Ships.Flight.Newtonian_Ship;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table  : Ship_Module_Table;
      Row    : Positive;
      Column : Positive)
      return String
   is
   begin
      case Ship_Module_Column (Column) is
         when 1 =>
            return Table.Ship.Structure (Row).Module.Name;
         when 2 =>
            return Table.Ship.Structure (Row).Module.Component.Name;
         when 3 =>
            return Lui.Approximate_Image
              (Table.Ship.Structure (Row).Module.Volume);
         when 4 =>
            return Lui.Approximate_Image
              (Table.Ship.Structure (Row).Module.Mass);
      end case;
   end Cell_Text;

   overriding function Cell_Text
     (Table  : Ship_Cargo_Table;
      Row    : Positive;
      Column : Positive)
      return String
   is
      Item : constant Concorde.Commodities.Commodity_Type :=
               Table.Cargo.Element (Row);
   begin
      case Ship_Cargo_Column (Column) is
         when 1 =>
            --  commodity
            return Item.Name;
         when 2 =>
            --  units
            return Quantities.Image
              (Table.Ship.Get_Quantity (Item));
         when 3 =>
            --  mass
            return Lui.Approximate_Image
              (Quantities.To_Real
                 (Table.Ship.Get_Quantity (Item))
               * Item.Unit_Mass / 1000.0) & "kg";
         when 4 =>
            --  value
            return Money.Image
              (Table.Ship.Get_Value (Item));
      end case;
   end Cell_Text;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table  : Ship_Engine_Table;
      Row    : Positive;
      Column : Positive)
      return String
   is
   begin
      case Ship_Engine_Column (Column) is
         when 1 =>
            return Positive'Image (Row);
         when 2 =>
            return Table.Ship.Get_Module
              (Table.Newton_Ship.Engine_Mount (Row)).Name;
         when 3 =>
            return Lui.Approximate_Image
              (Table.Newton_Ship.Engine (Row).Maximum_Thrust / 1.0e6);
         when 4 =>
            return Lui.Approximate_Image
              (Table.Newton_Ship.Engine (Row).Throttle);
      end case;
   end Cell_Text;

   ------------------------
   -- Create_Cargo_Table --
   ------------------------

   function Create_Cargo_Table
     (Ship : Ship_Type)
      return Lui.Tables.Model_Table
   is
      Result : Ship_Cargo_Table;

      procedure Add_Cargo
        (Commodity : Concorde.Commodities.Commodity_Type);

      ---------------
      -- Add_Cargo --
      ---------------

      procedure Add_Cargo
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
         use type Concorde.Quantities.Quantity;
      begin
         if Ship.Get_Quantity (Commodity) > Quantities.Zero then
            Result.Cargo.Append (Commodity);
         end if;
      end Add_Cargo;

   begin
      Result.Initialise ("Cargo", 0, Ship_Cargo_Column_Count);
      Result.Ship := Ship;
      Concorde.Commodities.Db.Scan (Add_Cargo'Access);
      return new Ship_Cargo_Table'(Result);
   end Create_Cargo_Table;

   -------------------------
   -- Create_Engine_Table --
   -------------------------

   function Create_Engine_Table
     (Newton_Ship : Concorde.Ships.Flight.Newtonian_Ship)
      return Lui.Tables.Model_Table
   is
      Result : Ship_Engine_Table;
   begin
      Result.Initialise ("Engines",
                         Newton_Ship.Engine_Count,
                         Ship_Engine_Column_Count);

      Result.Ship := Newton_Ship.Ship;
      Result.Newton_Ship := Newton_Ship;

      return new Ship_Engine_Table'(Result);

   end Create_Engine_Table;

   ---------------------------------
   -- Create_Engine_Toggle_Button --
   ---------------------------------

   function Create_Engine_Toggle_Button
     (Newton_Ship  : Concorde.Ships.Flight.Newtonian_Ship;
      Engine_Index : Positive)
      return Lui.Gadgets.Model_Gadget
   is
      Result : Engine_Toggle_Button :=
                 (Lui.Gadgets.Root_Button_Gadget with
                  Newton_Ship, Engine_Index);
   begin
      Result.Initialise ("Engine" & Positive'Image (Engine_Index));
      return new Engine_Toggle_Button'(Result);
   end Create_Engine_Toggle_Button;

   -------------------------
   -- Create_Module_Table --
   -------------------------

   function Create_Module_Table
     (Newton_Ship : Concorde.Ships.Flight.Newtonian_Ship)
      return Lui.Tables.Model_Table
   is
      Result : Ship_Module_Table;
   begin
      Result.Initialise ("Modules",
                         Newton_Ship.Ship.Structure.Last_Index,
                         Ship_Module_Column_Count);

      Result.Ship := Newton_Ship.Ship;
      Result.Newton_Ship := Newton_Ship;

      return new Ship_Module_Table'(Result);

   end Create_Module_Table;

   ------------------------
   -- Create_Newton_Ship --
   ------------------------

   function Create_Newton_Ship
     (Ship : Ship_Type)
      return Concorde.Ships.Flight.Newtonian_Ship
   is
      Result : constant Concorde.Ships.Flight.Newtonian_Ship :=
                 Concorde.Ships.Flight.Create_Newtonian_Ship
                   (From_Ship   => Ship,
                    Location    => (0.0, 0.0, 0.0),
                    Velocity    => (0.0, 0.0, 0.0),
                    Orientation =>
                      Newton.Flight.Matrices.Unit_Matrix (3));
   begin
      for Mount_Index in 1 .. Ship.Structure.Last_Index loop
         declare
            use Concorde.Components;
            Mount : Module_Layout_Record renames Ship.Structure (Mount_Index);
         begin
            if Mount.Module.Component.Class = Drive then
               declare
                  Component : constant Ship_Component :=
                                Get_Component (Mount, Mount_Index);
                  O_4 : constant Lui.Models.Model_3D.Matrix_4 :=
                          To_Orientation_Matrix
                            (Mount.Orientation);
                  O_3 : Newton.Flight.Matrix_3;
               begin
                  for I in 1 .. 3 loop
                     for J in 1 .. 3 loop
                        O_3 (I, J) := O_4 (I, J);
                     end loop;
                  end loop;
                  Result.Add_Engine_Mount
                    (Mount        => Mounted_Module (Mount_Index),
                     Location     => Component.Position,
                     Orientation  => O_3,
                     Max_Thrust   => Mount.Module.Maximum_Output,
                     Delta_Thrust => Mount.Module.Component.Throttle_Step);
               end;
            end if;
         end;
      end loop;

      return Result;

   end Create_Newton_Ship;

   --------------------------
   -- Create_Rotate_Button --
   --------------------------

   function Create_Rotate_Button
     (Newton_Ship  : Concorde.Ships.Flight.Newtonian_Ship;
      Rotation     : Newton.Flight.Vector_3;
      Label        : String)
      return Lui.Gadgets.Model_Gadget
   is
      Result : Rotate_Button :=
                 (Lui.Gadgets.Root_Button_Gadget with
                  Newton_Ship, Rotation);
   begin
      Result.Initialise (Label);
      return new Rotate_Button'(Result);
   end Create_Rotate_Button;

   ------------------
   -- Create_Scene --
   ------------------

   overriding procedure Create_Scene
     (Model  : in out Ship_Model)
   is
      Unit_Size  : constant Real := 1.0;
      Rotate_3   : constant Newton.Flight.Matrix_3 :=
                     Model.Newton_Ship.Orientation;
      Translate_3 : constant Newton.Flight.Vector_3 :=
                      Model.Newton_Ship.Location;
      Rotate_4   : Lui.Models.Model_3D.Matrix_4 :=
                     Lui.Models.Model_3D.Matrices.Unit_Matrix (4);
   begin

      --  Model.Newton_Ship.Report_Summary_Line;

      for I in 1 .. 3 loop
         for J in 1 .. 3 loop
            Rotate_4 (I, J) := Rotate_3 (I, J);
         end loop;
      end loop;

      Model.Multiply (Rotate_4);

      Model.Translate (Translate_3 (1), Translate_3 (2), Translate_3 (3));

      for Component of Model.Components loop
         Draw_Component (Model, Rotate_4, Component, Unit_Size);
      end loop;

   end Create_Scene;

   -----------------------
   -- Create_Ship_Model --
   -----------------------

   function Create_Ship_Model
     (Ship : Ship_Type)
      return Lui.Models.Object_Model
   is
      Model : Ship_Model;
   begin
      Model.Initialise (Ship);
      declare
         Result : constant Lui.Models.Object_Model :=
                    new Ship_Model'(Model);
      begin
         return Result;
      end;
   end Create_Ship_Model;

   --------------------
   -- Draw_Component --
   --------------------

   procedure Draw_Component
     (Model     : in out Ship_Model'Class;
      Matrix    : Lui.Models.Model_3D.Matrix_4;
      Item      : Ship_Component;
      Unit_Size : Real)
   is
      pragma Unreferenced (Unit_Size);
      Colour : Lui.Colours.Colour_Type := Item.Colour;
      DX     : constant Real := Item.Size (1) / 2.0;
      DY     : constant Real := Item.Size (2) / 2.0;
      DZ     : constant Real := Item.Size (3) / 2.0;
      CX     : constant Real :=
                 Model.Explode * (Item.Position (1) + DX);
      CY     : constant Real :=
                 Model.Explode * (Item.Position (2) + DY);
      CZ     : constant Real :=
                 Model.Explode * (Item.Position (3) + DZ);
   begin

      Model.Begin_Object (Item.Structure_Index);

      if Model.Newton_Ship.Selected_Component_Index
        = Item.Structure_Index
      then
         Colour := (0.8, 0.6, 0.0, 1.0);
      elsif Item.Engine_Index /= 0
        and then Model.Newton_Ship.Engine (Item.Engine_Index).Throttle > 0.0
      then
         declare
            Throttle : constant Real :=
                         Model.Newton_Ship.Engine (Item.Engine_Index).Throttle;
         begin
            Colour :=
              (Throttle / 2.0 + 0.5,
               0.75 * (Throttle / 2.0 + 0.5),
               0.0,
               1.0);
         end;
      end if;

      Model.Push_Matrix;

      Lui.Models.Model_3D.Translate
        (Model, CX, CY, CZ);

      Lui.Models.Model_3D.Multiply
        (Model, Item.Orientation);

      if False then
         Lui.Models.Model_3D.Multiply
           (Model, Matrix);
      end if;

      case Item.Shape is
         when Concorde.Components.Sphere =>
            Model.Icosohedral_Sphere
              (Colour => Colour,
               RX     => DX,
               RY     => DY,
               RZ     => DZ,
               Detail => 2);
         when Concorde.Components.Cylinder =>
            declare
               Detail : constant Positive := 6;
            begin
               Model.Cylinder
                 (Colour => Colour,
                  X      => 0.0,
                  Y      => 0.0,
                  Z      => 0.0,
                  RX     => DX,
                  RY     => DY,
                  DZ     => DZ,
                  Detail => Detail);
            end;

         when Concorde.Components.Cone =>
            declare
               Detail : constant Positive := 6;
            begin
               Model.Cone
                 (Colour => Colour,
                  X      => 0.0,
                  Y      => 0.0,
                  Z      => 0.0,
                  RX     => DX,
                  RY     => DY,
                  DZ     => DZ,
                  Detail => Detail);
            end;

         when Concorde.Components.Conical_Frustum =>
            declare
               Detail : constant Positive := 6;
            begin
               Model.Conical_Frustum
                 (Colour => Colour,
                  X      => 0.0,
                  Y      => 0.0,
                  Z      => 0.0,
                  RX1    => DX,
                  RY1    => DY,
                  RX2    => DX / 2.0,
                  RY2    => DY / 2.0,
                  DZ     => DZ,
                  Detail => Detail);
            end;

         when others =>
            if Show_Cubes then
               declare
                  Xs  : constant Positive :=
                          Positive (Item.Size (1));
                  Ys  : constant Positive :=
                          Positive (Item.Size (2));
                  Zs  : constant Positive :=
                          Positive (Item.Size (3));
                  X, Y, Z : Real;
               begin
                  for IX in 0 .. Xs - 1 loop
                     X := 0.5 - DX +
                       Real (IX) / Real (Xs) * Item.Size (1);
                     for IY in 0 .. Ys - 1 loop
                        Y := 0.5 - DY +
                          Real (IY) / Real (Ys) * Item.Size (2);
                        for IZ in 0 .. Zs - 1 loop
                           if IX = 0 or else IX = Xs - 1
                             or else IY = 0 or else IY = Ys - 1
                             or else IZ = 0 or else IZ = Zs - 1
                           then
                              Z := 0.5 - DZ +
                                Real (IZ) / Real (Zs) * Item.Size (3);
                              Draw_Cube
                                (Model, Colour,
                                 X, Y, Z,
                                 0.5, 0.5, 0.5,
                                 Front  => IZ = Zs - 1,
                                 Back   => IZ = 0,
                                 Left   => IX = 0,
                                 Right  => IX = Xs - 1,
                                 Top    => IY = Ys - 1,
                                 Bottom => IY = 0);
                           end if;
                        end loop;
                     end loop;
                  end loop;
               end;
            else

               Draw_Cube (Model, Colour,
                          0.0, 0.0, 0.0,
                          DX, DY, DZ);

            end if;
      end case;

      Model.Pop_Matrix;

      Model.End_Object;

   end Draw_Component;

   ---------------
   -- Draw_Cube --
   ---------------

   procedure Draw_Cube
     (Model       : in out Ship_Model'Class;
      Colour      : Lui.Colours.Colour_Type;
      X, Y, Z     : Real;
      DX, DY, DZ  : Real;
      Front, Back : Boolean := True;
      Left, Right : Boolean := True;
      Top, Bottom : Boolean := True)
   is
   begin

      if Front then
         Model.Begin_Surface (Colour);
         Model.Vertex (X - DX, Y + DY, Z + DZ);
         Model.Vertex (X - DX, Y - DY, Z + DZ);
         Model.Vertex (X + DX, Y - DY, Z + DZ);
         Model.Vertex (X + DX, Y + DY, Z + DZ);
         Model.End_Surface;
      end if;

      if Back then
         Model.Begin_Surface (Colour);
         Model.Vertex (X - DX, Y - DY, Z - DZ);
         Model.Vertex (X - DX, Y + DY, Z - DZ);
         Model.Vertex (X + DX, Y + DY, Z - DZ);
         Model.Vertex (X + DX, Y - DY, Z - DZ);
         Model.End_Surface;
      end if;

      if Left then
         Model.Begin_Surface (Colour);
         Model.Vertex (X - DX, Y - DY, Z - DZ);
         Model.Vertex (X - DX, Y - DY, Z + DZ);
         Model.Vertex (X - DX, Y + DY, Z + DZ);
         Model.Vertex (X - DX, Y + DY, Z - DZ);
         Model.End_Surface;
      end if;

      if Right then
         Model.Begin_Surface (Colour);
         Model.Vertex (X + DX, Y - DY, Z - DZ);
         Model.Vertex (X + DX, Y + DY, Z - DZ);
         Model.Vertex (X + DX, Y + DY, Z + DZ);
         Model.Vertex (X + DX, Y - DY, Z + DZ);
         Model.End_Surface;
      end if;

      if Top then
         Model.Begin_Surface (Colour);
         Model.Vertex (X + DX, Y + DY, Z - DZ);
         Model.Vertex (X - DX, Y + DY, Z - DZ);
         Model.Vertex (X - DX, Y + DY, Z + DZ);
         Model.Vertex (X + DX, Y + DY, Z + DZ);
         Model.End_Surface;
      end if;

      if Bottom then
         Model.Begin_Surface (Colour);
         Model.Vertex (X - DX, Y - DY, Z - DZ);
         Model.Vertex (X + DX, Y - DY, Z - DZ);
         Model.Vertex (X + DX, Y - DY, Z + DZ);
         Model.Vertex (X - DX, Y - DY, Z + DZ);
         Model.End_Surface;
      end if;

   end Draw_Cube;

   -------------------
   -- Handle_Update --
   -------------------

   overriding function Handle_Update
     (Model : in out Ship_Model)
      return Boolean
   is
      use Ada.Calendar;
      Current_Time : constant Time := Clock;
      Time_Delta   : constant Duration := Current_Time - Model.Last_Update;
   begin
      Model.Last_Update := Current_Time;
      Model.Manager.Update (Newton.Real (Time_Delta));
      Model.Newton_Ship.Update (Newton.Real (Time_Delta));
      return True;
   end Handle_Update;

   -------------------------
   -- Heading_Column_Text --
   -------------------------

   overriding function Heading_Column_Text
     (Table  : Ship_Engine_Table;
      Column : Positive)
      return String
   is
      pragma Unreferenced (Table);
   begin
      case Ship_Engine_Column (Column) is
         when 1 =>
            return "Index";
         when 2 =>
            return "Type";
         when 3 =>
            return "Max Thrust (MN)";
         when 4 =>
            return "Throttle";
      end case;
   end Heading_Column_Text;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Model : in out Ship_Model'Class;
      Ship  : in     Ship_Type)
   is
      Newton_Ship : constant Concorde.Ships.Flight.Newtonian_Ship :=
                      Create_Newton_Ship (Ship);
      Modules     : constant Lui.Tables.Model_Table :=
                      Create_Module_Table (Newton_Ship);
      Cargo       : constant Lui.Tables.Model_Table :=
                      Create_Cargo_Table (Ship);
      Engines     : constant Lui.Tables.Model_Table :=
                      Create_Engine_Table (Newton_Ship);
      Engine_Buttons : Lui.Gadgets.Array_Of_Gadgets
        (1 .. Newton_Ship.Engine_Count);
      Rotation_Buttons : constant Lui.Gadgets.Array_Of_Gadgets :=
                           (Create_Rotate_Button
                              (Newton_Ship, (0.0, 0.0, 0.0), "Stop"),
                            Create_Rotate_Button
                              (Newton_Ship, (0.0, 0.0, 0.1),
                               "Roll Left"),
                            Create_Rotate_Button
                              (Newton_Ship, (0.0, 0.0, -0.1),
                               "Roll Right"),
                            Create_Rotate_Button
                              (Newton_Ship, (-0.1, 0.0, 0.0),
                               "Pitch Up"),
                            Create_Rotate_Button
                              (Newton_Ship, (0.1, 0.0, 0.0),
                               "Pitch Down"),
                            Create_Rotate_Button
                              (Newton_Ship, (0.0, -0.1, 0.0),
                               "Yaw Left"),
                            Create_Rotate_Button
                              (Newton_Ship, (0.0, 0.1, 0.0),
                               "Yaw Right"));
   begin
      for I in Engine_Buttons'Range loop
         Engine_Buttons (I) :=
           Create_Engine_Toggle_Button
             (Newton_Ship, I);
      end loop;

      Model.Ship := Ship;
      Model.Newton_Ship := Newton_Ship;

      declare
         use type Lui.Gadgets.Array_Of_Gadgets;
      begin
         Model.Initialise (Ship.Name, (Modules, Cargo, Engines),
                           Rotation_Buttons & Engine_Buttons);
      end;

      declare
         Engine_Count : Natural := 0;
      begin
         for Mount_Index in 1 .. Ship.Structure.Last_Index loop
            declare
               use Concorde.Components;
               Mount : Module_Layout_Record renames
                         Ship.Structure (Mount_Index);
               Item  : Ship_Component :=
                         Get_Component (Mount, Mount_Index);
            begin
               if Mount.Module.Component.Class = Drive then
                  Engine_Count := Engine_Count + 1;
                  Item.Engine_Index := Engine_Count;
               end if;
               Model.Components.Append (Item);
            end;
         end loop;
      end;

      Model.Manager.Manage (Model.Newton_Ship);
--      Model.Manager.Rotate ((0.0, 1.0, 0.0));
      Model.Last_Update := Ada.Calendar.Clock;
      Model.Add_Property
        ("Empty Mass", Ship.Empty_Mass / 1.0e6, "kt");
      Model.Add_Property
        ("Current Mass",
         Ship.Current_Mass / 1.0e6,
         "kt");
      Model.Add_Property
        ("Full Mass",
         Ship.Standard_Full_Mass / 1.0e6,
         "kt");
      Model.Add_Property
        ("Max. Thrust", Ship.Maximum_Thrust / 1.0e6, "MN");
      Model.Add_Property
        ("Accel (empty)",
         Ship.Maximum_Thrust / Ship.Empty_Mass / 9.8,
         "g");
      Model.Add_Property
        ("Accel (current)",
         Ship.Maximum_Thrust / Ship.Current_Mass / 9.8,
         "g");
      Model.Add_Property
        ("Accel (full)",
         Ship.Maximum_Thrust / Ship.Standard_Full_Mass / 9.8,
         "g");

      declare
         use Concorde.Elementary_Functions;
         Acceleration  : constant Non_Negative_Real :=
                           Ship.Maximum_Thrust
                             / Ship.Standard_Full_Mass;
         Powered_Time  : constant Non_Negative_Real :=
                           Ship.Tank_Size * 4.0;
         Final_Velocity : constant Real :=
                           Acceleration * Powered_Time;
         Powered_Range : constant Real :=
                           Acceleration
                              * (Powered_Time / 2.0) ** 2;
         Coast_Velocity : constant Real :=
                            Acceleration * Powered_Time / 2.0;
         Max_Powered_Range : constant Real :=
                               Acceleration
                                 * (Powered_Time) ** 2 / 1000.0;
         Earth_Moon_Distance : constant Real :=
                                 384_400_000.0;
         Earth_Mars_Min_Distance : constant Real := 5.6E10;
         Moon_Coast_Distance     : constant Real :=
                                     Real'Max
                                       (0.0,
                                        Earth_Moon_Distance
                                        - Powered_Range * 2.0);
         Moon_Accel_Distance     : constant Non_Negative_Real :=
                                     (if Powered_Range < Earth_Moon_Distance
                                      then Powered_Range / 2.0
                                      else Earth_Moon_Distance / 2.0);
         Moon_Accel_Time         : constant Non_Negative_Real :=
                                     Sqrt (2.0 * Moon_Accel_Distance
                                           / Acceleration);
         Mars_Coast_Distance     : constant Real :=
                                     Earth_Mars_Min_Distance
                                       - Powered_Range * 2.0;
         Moon_Coast_Time         : constant Real :=
                                     Moon_Coast_Distance / Coast_Velocity;
         Mars_Coast_Time         : constant Real :=
                                     Mars_Coast_Distance / Coast_Velocity;
      begin
         Model.Add_Property
           ("Powered flight time", Powered_Time, "s");
         Model.Add_Property
           ("Max. Delta-V", Final_Velocity, "m/s");
         Model.Add_Property
           ("Cruise velocity", Coast_Velocity, "m/s");
         Model.Add_Property
           ("Max. Powered Range", Max_Powered_Range, "km");
         if Coast_Velocity > 0.0 then
            Model.Add_Property
              ("Earth-Moon journey",
               (Moon_Coast_Time + 2.0 * Moon_Accel_Time) / 3600.0, "hours");
            Model.Add_Property
              ("Quickest Earth-Mars",
               (Mars_Coast_Time + Powered_Time) / 3600.0 / 24.0, "days");
         end if;
      end;

      Model.Add_Property
        ("Tank", Ship.Tank_Size,
         "m" & Character'Val (16#C2#) & Character'Val (16#B3#));
      Model.Add_Property
        ("Hold", Ship.Hold_Size,
         "m" & Character'Val (16#C2#) & Character'Val (16#B3#));

--        Newton_Ship.Throttle_Engine (1, 0.01);
--        Newton_Ship.Throttle_Engine (2, 0.01);

   end Initialise;

   ------------------
   -- Long_Tooltip --
   ------------------

   overriding function Long_Tooltip
     (Model : Ship_Model;
      X, Y  : Natural)
      return String
   is
      pragma Unreferenced (X);
      pragma Unreferenced (Y);
   begin
      return Model.Ship.Name;
   end Long_Tooltip;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Button : Engine_Toggle_Button;
      Model  : not null access Lui.Models.Root_Object_Model'Class)
   is
      pragma Unreferenced (Model);
   begin
      if Button.Newton_Ship.Engine (Button.Engine_Index).Throttle > 0.0 then
         Button.Newton_Ship.Throttle_Engine (Button.Engine_Index, 0.0);
      else
         Button.Newton_Ship.Throttle_Engine (Button.Engine_Index, 1.0);
      end if;
   end On_Click;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Button : Rotate_Button;
      Model  : not null access Lui.Models.Root_Object_Model'Class)
   is
   begin
      Ship_Model (Model.all).Manager.Rotate (Button.Rotation);
   end On_Click;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model : in out Ship_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
   begin
      Lui.Models.Model_3D.Root_3D_Model (Model).Render (Renderer);
      for I in 1 .. Model.Newton_Ship.Engine_Count loop
         declare
            X    : constant Positive := 2;
            Y    : constant Positive := 2 + 10 * I;
            W    : constant Natural :=
                     Natural (60.0 * Model.Newton_Ship.Engine (I).Power);
            H    : constant Positive := 7;
            TX   : constant Positive := 2 +
                     Natural (60.0 * Model.Newton_Ship.Engine (I).Throttle);
         begin
            if W > 0 then
               Renderer.Draw_Rectangle
                 (X, Y, W, H,
                  Lui.Colours.To_Colour (180, 0, 0), True);
            end if;
            Renderer.Draw_Line
              (X1     => TX,
               Y1     => Y - 1,
               X2     => TX,
               Y2     => Y + 9,
               Colour => Lui.Colours.To_Colour (220, 220, 0));
         end;
      end loop;
   end Render;

   ----------------
   -- Select_Row --
   ----------------

   overriding procedure Select_Row
     (Table : Ship_Module_Table;
      Row   : Positive)
   is
   begin
      Table.Newton_Ship.Select_Component (Row, True);
   end Select_Row;

   ---------------
   -- Select_XY --
   ---------------

   overriding function Select_XY
     (Model : Ship_Model;
      X, Y  : Natural)
      return Lui.Models.Object_Model
   is
      pragma Unreferenced (Model);
      pragma Unreferenced (X);
      pragma Unreferenced (Y);
   begin
      return null;
   end Select_XY;

   ---------------
   -- Select_XY --
   ---------------

   overriding procedure Select_XY
     (Model : in out Ship_Model;
      X, Y  : Natural)
   is
      Object_Id : constant Natural := Model.Get_Object_Id (X, Y);
   begin
      if Object_Id = 0
        or else Model.Newton_Ship.Selected_Component_Index = Object_Id
      then
         Model.Newton_Ship.Select_Component (0);
      else
         Model.Newton_Ship.Select_Component (Object_Id);
      end if;
   end Select_XY;

   ---------------------------
   -- To_Orientation_Matrix --
   ---------------------------

   function To_Orientation_Matrix
     (Orientation : Module_Orientation)
      return Newton.Flight.Matrix_4
   is
      use Concorde.Components;
      Result : Newton.Flight.Matrix_4 :=
                 Newton.Flight.Matrices.Unit_Matrix (4);
   begin
      case Orientation.Axis is
         when X_Axis =>
            Result (1, 1) := 0.0;
            Result (3, 3) := 0.0;
            if Orientation.Forward then
               Result (1, 3) := -1.0;
               Result (3, 1) := 1.0;
            else
               Result (1, 3) := 1.0;
               Result (3, 1) := -1.0;
            end if;
         when Y_Axis =>
            Result (2, 2) := 0.0;
            Result (3, 3) := 0.0;
            if Orientation.Forward then
               Result (3, 2) := -1.0;
               Result (2, 3) := 1.0;
            else
               Result (3, 2) := 1.0;
               Result (2, 3) := -1.0;
            end if;
         when Z_Axis =>
            if Orientation.Forward then
               Result (2, 2) := 1.0;
               Result (3, 3) := 1.0;
            else
               Result (2, 2) := -1.0;
               Result (3, 3) := -1.0;
            end if;
      end case;
      return Result;
   end To_Orientation_Matrix;

   -------------
   -- Tooltip --
   -------------

   overriding function Tooltip
     (Model : Ship_Model;
      X, Y  : Natural)
      return String
   is
      pragma Unreferenced (X);
      pragma Unreferenced (Y);
   begin
      return Model.Ship.Name;
   end Tooltip;

   ----------
   -- Zoom --
   ----------

   overriding procedure Zoom
     (Item    : in out Ship_Model;
      Z       : in     Integer;
      Control : in   Boolean)
   is
   begin
      if Control then
         declare
            New_Value : constant Real :=
                          (if Z > 0
                           then Item.Explode * 1.1
                           else Item.Explode * 0.9);
         begin
            Item.Explode :=
              (if New_Value < 1.0
               then 1.0
               elsif New_Value > Max_Explode
               then Max_Explode
               else New_Value);
         end;
      else
         Lui.Models.Model_3D.Root_3D_Model (Item).Zoom (Z, Control);
      end if;
   end Zoom;

end Concorde.Ships.Models;
