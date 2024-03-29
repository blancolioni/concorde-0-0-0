with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Ada.Text_IO;
with Ada.Float_Text_IO;

with Tropos.Reader;

with Concorde.Elementary_Functions;
with Concorde.Geometry;
with Concorde.Random;
--  with Concorde.Voronoi_Diagrams;

with Concorde.Options;

with Concorde.Stars.Classification;
with Concorde.Stars.Create;
with Concorde.Systems.Create;

with Concorde.Scenarios;

package body Concorde.Galaxy.Create is

   Connect_Systems : constant Boolean := False;

   procedure Cartesian_Location
     (X, Y, Z : out Real);

   procedure Spherical_Location
     (X, Y, Z : out Real);

   procedure Spiral_Location
     (X, Y, Z : out Real);

   function Random_Normal
     (Std_Deviation : Non_Negative_Real)
      return Real;

   function Random_Star_Mass return Non_Negative_Real;

   function Clamp
     (X          : Real;
      Max        : Real := 1.0;
      Min        : Real := -1.0)
      return Real
   is ((if X < Min then Min elsif X > Max then Max else X));

   procedure Create_Scenario_Links;

   ------------------------
   -- Cartesian_Location --
   ------------------------

   procedure Cartesian_Location
     (X, Y, Z : out Real)
   is
      use Concorde.Random;
   begin
      X := Unit_Random * 2.0 - 1.0;
      Y := Unit_Random * 2.0 - 1.0;
      Z := Unit_Random * 2.0 - 1.0;
   end Cartesian_Location;

   ------------------------------
   -- Create_Catalogue_Systems --
   ------------------------------

   procedure Create_Catalogue_Systems
     (Catalogue_Path : String)
   is
      Max_Distance : constant := 900.0;

      Galaxy_Config : constant Tropos.Configuration :=
                        Tropos.Reader.Read_CSV_Config
                          (Path          => Catalogue_Path,
                           Header_Line   => True,
                           Separator     => ',',
                           Extend_Header => False);

      type Catalogue_Star is
         record
            X, Y, Z          : Real;
            Name             : Ada.Strings.Unbounded.Unbounded_String;
            Classification   : Concorde.Stars.Stellar_Classification;
         end record;

      package Catalogue_Vectors is
        new Ada.Containers.Vectors (Positive, Catalogue_Star);

      Catalogue_Stars : Catalogue_Vectors.Vector;
      Max_X, Max_Y, Max_Z : Real := 0.0;

   begin
      Ada.Text_IO.Put_Line ("catalogue stars:"
                            & Natural'Image (Galaxy_Config.Child_Count));

      for Config of Galaxy_Config loop
         declare
            Rec : Catalogue_Star;
            Name : constant String := Config.Get ("proper");
            Spectrum : constant String := Config.Get ("spect");
         begin
            Rec.X := Real (Float'(Config.Get ("x")));
            Rec.Y := Real (Float'(Config.Get ("y")));
            Rec.Z := Real (Float'(Config.Get ("z")));
            if Rec.X ** 2 + Rec.Y ** 2 + Rec.Z ** 2 <= Max_Distance then
               Rec.Name :=
                 Ada.Strings.Unbounded.To_Unbounded_String
                   ((if Name = ""
                    then "Star"
                    & Positive'Image (Catalogue_Stars.Last_Index + 1)
                    else Name));
               Rec.Classification :=
                 Concorde.Stars.Classification.Get_Stellar_Classification
                   (Spectrum);
               Max_X := Real'Max (Max_X, abs Rec.X);
               Max_Y := Real'Max (Max_Y, abs Rec.Y);
               Max_Z := Real'Max (Max_Z, abs Rec.Z);

               if Name /= "" then
                  Ada.Text_IO.Put_Line (Name);
               end if;

               Catalogue_Stars.Append (Rec);
            end if;
         exception
            when Constraint_Error =>
               null;
         end;
      end loop;

      Ada.Text_IO.Put_Line
        ("using"
         & Ada.Containers.Count_Type'Image (Catalogue_Stars.Length)
         & " stars");
      Ada.Text_IO.Put ("max x: ");
      Ada.Float_Text_IO.Put (Float (Max_X), 1, 1, 0);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("max y: ");
      Ada.Float_Text_IO.Put (Float (Max_Y), 1, 1, 0);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("max z: ");
      Ada.Float_Text_IO.Put (Float (Max_Z), 1, 1, 0);
      Ada.Text_IO.New_Line;

      for I in 1 .. Catalogue_Stars.Last_Index loop
         declare
            use Ada.Strings.Unbounded;
            Rec : Catalogue_Star renames Catalogue_Stars (I);
            System : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Systems.Create.New_System
                         (I, To_String (Rec.Name),
                          Rec.X / Max_X, Rec.Y / Max_Y, Rec.Z / Max_Z);
            Star   : constant Concorde.Stars.Star_Type :=
                       Concorde.Stars.Create.New_Main_Sequence_Star
                         (System, To_String (Rec.Name),
                          Concorde.Stars.Classification.Solar_Masses
                            (Rec.Classification));
         begin
            System.Update.Add_Object (Star);
            Galaxy_Graph.Append (System);
         end;
      end loop;

      Create_Scenario_Links;

   end Create_Catalogue_Systems;

   -------------------
   -- Create_Galaxy --
   -------------------

   procedure Create_Galaxy
     (System_Count        : Natural;
      Shape               : Galaxy_Shape;
      DX, DY, DZ          : Real;
      Average_Connections : Positive;
      Name_Generator      : WL.Random.Names.Name_Generator)
   is
      use Concorde.Systems;
      Min_Distance      : constant Non_Negative_Real :=
                            (1.0 / (2.0 * Real (System_Count)));
      Total_Connections : Natural := 0;
--        Influence         : Concorde.Voronoi_Diagrams.Voronoi_Diagram;
--        Create_Voronoi    : constant Boolean :=
--                              Concorde.Options.Create_Voronoi_Diagram;
      Xs, Ys, Zs        : array (1 .. System_Count) of Real;
      Retries           : Natural := 0;
      Class_Count       : array (Concorde.Stars.Stellar_Class_Type)
        of Natural := (others => 0);

   begin
      Ada.Text_IO.Put_Line ("System count:" & System_Count'Img);
      Ada.Text_IO.Put ("Minimum distance: ");
      Ada.Float_Text_IO.Put (Float (Min_Distance), 1, 8, 0);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Imperial centre: "
                            & (if Concorde.Scenarios.Imperial_Centre
                              then "yes"
                              else "no"));

      for I in 1 .. System_Count loop
         declare
            D : Non_Negative_Real := 0.0;
            X, Y, Z : Real := 0.0;
            Imperial_Centre : constant Boolean :=
                                I = 1 and then
                                    Concorde.Scenarios.Imperial_Centre;
         begin
            Xs (I) := 0.0;
            Ys (I) := 0.0;

            while not Imperial_Centre and then D < Min_Distance loop
               Retries := Retries + 1;
               D :=  Non_Negative_Real'Last;
               case Shape is
                  when Cube =>
                     Cartesian_Location (X, Y, Z);
                  when Sphere =>
                     Spherical_Location (X, Y, Z);
                  when Spiral =>
                     Spiral_Location (X, Y, Z);
                     X := X + Random_Normal (DX);
                     Y := Y + Random_Normal (DY);

                     declare
                        use Concorde.Elementary_Functions;
                        Orbit : constant Non_Negative_Real :=
                                  Sqrt (X ** 2 + Y ** 2);
                        Radius_DZ : constant Non_Negative_Real :=
                                      (if Orbit < 0.1
                                       then 1.2 * DZ
                                       else (1.2 / (Orbit + 0.9)) * DZ);
                     begin
                        Z := Z + Random_Normal (Radius_DZ);
                     end;

               end case;

               X := Clamp (X);
               Y := Clamp (Y);
               Z := Clamp (Z);

               for J in 1 .. I - 1 loop
                  declare
                     Test_D : constant Real :=
                                (X - Xs (J)) ** 2 + (Y - Ys (J)) ** 2;
                  begin
                     if J = 1 then
                        if Test_D / 2.0 < D then
                           D := Test_D / 2.0;
                        end if;
                     else
                        if Test_D < D then
                           D := Test_D;
                        end if;
                     end if;
                  end;
               end loop;
            end loop;

            Xs (I) := X;
            Ys (I) := Y;
            Zs (I) := Z;

--              if Create_Voronoi then
--                 Influence.Add_Point (X, Y);
--              end if;

         end;

      end loop;

      Ada.Text_IO.Put_Line
        ("retries:" & Integer'Image (Retries - System_Count));

--        if Create_Voronoi then
--           Influence.Generate;
--
--           Ada.Text_IO.Put_Line ("created Voronoi map");
--        end if;

      for I in 1 .. System_Count loop
         declare
--              Vertex_Count : constant Natural := 0;
--                               (if Create_Voronoi
--                                then Influence.Vertex_Count (I)
--                                else 0);
--              Boundary : Concorde.Systems.System_Influence_Boundary
--                (1 .. Vertex_Count);
            Name     : constant String :=
                         WL.Random.Names.Random_Name
                               (Name_Generator);
            Imperial_Centre : constant Boolean :=
                                I = 1 and then
                                    Concorde.Scenarios.Imperial_Centre;

         begin
--              for J in Boundary'Range loop
--                 Boundary (J) :=
--                   (Influence.Vertex_X (I, J), Influence.Vertex_Y (I, J));
--              end loop;

            declare
               System : constant Concorde.Systems.Star_System_Type :=
                          Concorde.Systems.Create.New_System
                            (I, Name,
                             Xs (I), Ys (I), Zs (I));
               Main_Star : constant Concorde.Stars.Star_Type :=
                             Concorde.Stars.Create.New_Main_Sequence_Star
                               (System, Name,
                                (if Imperial_Centre
                                 then 1.0
                                 else Random_Star_Mass));
               Count     : Natural renames
                             Class_Count
                               (Main_Star.Stellar_Class);
            begin
               System.Update.Add_Object (Main_Star);
               Galaxy_Graph.Append (System);
               Count := Count + 1;
            end;
         end;
      end loop;

      Ada.Text_IO.Put_Line ("created" & System_Count'Img & " systems");

      Ada.Text_IO.Put_Line ("Class   Count");
      for Class in Class_Count'Range loop
         Ada.Text_IO.Put (Class'Img);
         Ada.Text_IO.Set_Col (8);
         Ada.Text_IO.Put_Line (Natural'Image (Class_Count (Class)));
      end loop;

      Create_Scenario_Links;

      if Connect_Systems then
         for I in 1 .. System_Count loop
            declare
               function Sqrt (X : Non_Negative_Real) return Non_Negative_Real
                           renames Concorde.Elementary_Functions.Sqrt;
               System          : constant Star_System_Type :=
                                   Galaxy_Graph.Vertex (I);
               X               : constant Real := System.X;
               Y               : constant Real := System.Y;
               Max_Connections : constant Natural :=
                                   (if Concorde.Scenarios.Imperial_Centre
                                    then Natural'Max (10, Average_Connections)
                                    else Average_Connections);
               Ds                   : array (1 .. Max_Connections)
                 of Non_Negative_Real :=
                   (others => Real'Last);
               To              : array (1 .. Max_Connections) of Natural :=
                                   (others => 0);
               Count           : Natural := Average_Connections;
            begin
               if I = 1 and then Concorde.Scenarios.Imperial_Centre then
                  Count := 0;
               end if;

               for J in 1 .. System_Count loop
                  exit when Count = 0;
                  if I /= J then
                     if Galaxy_Graph.Connected (I, J) then
                        Count := Count - 1;
                     else
                        declare
                           Target : constant Star_System_Type :=
                                      Galaxy_Graph.Vertex (J);
                           D      : constant Non_Negative_Real :=
                                      Sqrt ((X - Target.X) ** 2
                                            + (Y - Target.Y) ** 2);
                           Index  : Natural := Count;
                        begin
                           while Index > 0 and then Ds (Index) > D loop
                              if Index < Count then
                                 Ds (Index + 1) := Ds (Index);
                                 To (Index + 1) := To (Index);
                              end if;
                              Index := Index - 1;
                           end loop;

                           if Index < Count then
                              Ds (Index + 1) := D;
                              To (Index + 1) := J;
                           end if;
                        end;
                     end if;
                  end if;
               end loop;

               for Edge_Index in 1 .. Count loop
                  if To (Edge_Index) > 0 then
                     Galaxy_Graph.Connect
                       (I, To (Edge_Index), Ds (Edge_Index));
                     Galaxy_Graph.Connect
                       (To (Edge_Index), I, Ds (Edge_Index));
                     Total_Connections := Total_Connections + 1;
                  end if;
               end loop;
            end;
         end loop;

         Ada.Text_IO.Put_Line ("created"
                               & Total_Connections'Img
                               & " connections");
         loop
            declare
               use Concorde.Systems.Graphs;
               Components : Sub_Graph_Collection;
               Min_D      : Real := Real'Last;
               From, To   : Natural := 0;
            begin
               Galaxy_Graph.Get_Connected_Components (Components);
               Ada.Text_IO.Put_Line
                 ("connected components:"
                  & Natural'Image (Sub_Graph_Count (Components)));
               exit when Sub_Graph_Count (Components) = 1;

               for I in 1 .. Galaxy_Graph.Last_Vertex_Index loop
                  for J in 1 .. Galaxy_Graph.Last_Vertex_Index loop
                     if I /= J
                       and then not Same_Sub_Graph (Components, I, J)
                     then
                        declare
                           V1 : constant Star_System_Type :=
                                  Galaxy_Graph.Vertex (I);
                           V2 : constant Star_System_Type :=
                                  Galaxy_Graph.Vertex (J);
                           D  : constant Non_Negative_Real :=
                                  Distance (V1, V2);
                        begin
                           if D < Min_D then
                              Min_D := D;
                              From := I;
                              To := J;
                           end if;
                        end;
                     end if;
                  end loop;
               end loop;

               if From /= 0 then
                  Galaxy_Graph.Connect (From, To, Min_D);
                  Galaxy_Graph.Connect (To, From, Min_D);
               end if;

            end;
         end loop;

         Ada.Text_IO.Put_Line
           ("# systems:" & System_Count'Img);
         Ada.Text_IO.Put
           ("ave. conn: ");
         Ada.Float_Text_IO.Put
           (Float (Total_Connections) / Float (System_Count),
            0, 2, 0);
         Ada.Text_IO.New_Line;
      end if;

   end Create_Galaxy;

   ---------------------------
   -- Create_Scenario_Links --
   ---------------------------

   procedure Create_Scenario_Links is
   begin
      if Concorde.Options.Create_Factions
        and then Concorde.Scenarios.Imperial_Centre
      then
         declare
            type Best_Connection is
               record
                  DX, DY : Real;
                  D      : Non_Negative_Real;
                  Index  : Natural;
               end record;

            function Direction_Index (DX, DY : Real) return Positive
            is (if abs DX > 3.0 * abs DY
                then (if DX < 0.0 then 1 else 5)
                elsif abs DY > 3.0 * abs DX
                then (if DY < 0.0 then 3 else 7)
                elsif DX > 0.0
                then (if DY > 0.0 then 8 else 2)
                elsif DY > 0.0 then 6 else 4);

            Best : array (1 .. 8) of Best_Connection :=
                     (others => (0.0, 0.0, 0.0, 0));

            Centre : constant Concorde.Systems.Star_System_Type :=
                       Get_System (1);
         begin
            for J in 2 .. System_Count loop
               declare
                  use Concorde.Systems;
                  System    : constant Star_System_Type :=
                                Get_System (J);
                  DX        : constant Real :=
                                System.X - Centre.X;
                  DY        : constant Real :=
                                System.Y - Centre.Y;
                  DZ        : constant Real :=
                                System.Z - Centre.Z;

                  Direction : constant Positive :=
                                Direction_Index (DX, DY);
                  Distance  : constant Non_Negative_Real :=
                                DX ** 2 + DY ** 2 + DZ ** 2;
               begin
                  if Best (Direction).Index = 0
                    or else Best (Direction).D > Distance
                  then
                     Best (Direction) := (DX, DY, Distance, J);
                  end if;
               end;
            end loop;

            for J in Best'Range loop
               Galaxy_Graph.Connect
                 (1, Best (J).Index, Best (J).D);
               Galaxy_Graph.Connect
                 (Best (J).Index, 1, Best (J).D);
            end loop;
         end;
      end if;

   end Create_Scenario_Links;

   -------------------
   -- Random_Normal --
   -------------------

   function Random_Normal
     (Std_Deviation : Non_Negative_Real)
      return Real
   is
      Samples : constant := 12;
      T       : Real := 0.0;
   begin
      for I in 1 .. Samples loop
         T := T + Concorde.Random.Unit_Random;
      end loop;
      T := T - Real (Samples) / 2.0;
      T := T * Std_Deviation;
      return T;
   end Random_Normal;

   ----------------------
   -- Random_Star_Mass --
   ----------------------

   function Random_Star_Mass return Non_Negative_Real is
      Seed             : constant Unit_Real := Concorde.Random.Unit_Random;
      Solar_Mass_Count : Real;
   begin
      if not Concorde.Options.Realistic_Star_Masses then
         declare
            Solar_Masses : Non_Negative_Real := Seed;
         begin
            for I in 1 .. 3 loop
               Solar_Masses :=
                 Solar_Masses + Concorde.Random.Unit_Random;
            end loop;
            return (Solar_Masses / 4.0) + 0.5;
         end;
      end if;

      if Seed <= 0.99 then
         Solar_Mass_Count :=
           0.1 + 6.0 * Seed - 15.0 * Seed ** 2
             + 11.0 * Seed ** 3;
      else
         declare
            X : constant Real := (Seed - 0.99) * 1.0E4;
            A : constant Real := 0.110833;
            B : constant Real := -14.0358;
            C : constant Real := 445.25;
         begin
            Solar_Mass_Count := A * X ** 2 + B * X + C;
         end;
      end if;
      return Solar_Mass_Count;
   end Random_Star_Mass;

   ------------------------
   -- Spherical_Location --
   ------------------------

   procedure Spherical_Location
     (X, Y, Z : out Real)
   is
      use Concorde.Elementary_Functions;
      R : constant Non_Negative_Real := Concorde.Random.Unit_Random;
      D : Non_Negative_Real;
   begin
      Cartesian_Location (X, Y, Z);
      D := Sqrt (X ** 2 + Y ** 2 + Z ** 2);
      X := X / D * R;
      Y := Y / D * R;
      Z := Z / D * R;
   end Spherical_Location;

   ---------------------
   -- Spiral_Location --
   ---------------------

   procedure Spiral_Location
     (X, Y, Z : out Real)
   is
      use Concorde.Random;
      use Concorde.Elementary_Functions;
      use Concorde.Geometry;
      Degrees     : constant Real := Unit_Random * 1440.0;
      Fermat_R    : constant Real :=
                      Sqrt (Degrees) / 40.0
                      * (if Unit_Random < 0.5 then -1.0 else 1.0);
      Archimedes_R : constant Real :=
                      Degrees / 1440.0
                        * (if Unit_Random < 0.5 then -1.0 else 1.0);
      Theta       : constant Radians := Degrees_To_Radians (Degrees);
      R           : constant Real :=
                       (if True then Archimedes_R else Fermat_R);
   begin
      X := R * Cos (Theta);
      Y := R * Sin (Theta);
      Z := 0.0;
   end Spiral_Location;

end Concorde.Galaxy.Create;
