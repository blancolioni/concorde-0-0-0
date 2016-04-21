with Ada.Text_IO;
with Ada.Float_Text_IO;

with Ada.Numerics.Float_Random;

with Concorde.Elementary_Functions;
with Concorde.Voronoi_Diagrams;

with Concorde.Systems.Create;

with Concorde.Scenarios;

package body Concorde.Galaxy.Create is

   -------------------
   -- Create_Galaxy --
   -------------------

   procedure Create_Galaxy
     (System_Count        : Natural;
      Average_Connections : Positive;
      Reset_Seed          : Boolean;
      Name_Generator      : WL.Random.Names.Name_Generator)
   is
      use Ada.Numerics.Float_Random;
      use Concorde.Systems;
      Min_Distance      : constant Non_Negative_Real :=
                            (1.0 / (2.0 * Real (System_Count)));
      Total_Connections : Natural := 0;
      Gen               : Generator;
      Influence         : Concorde.Voronoi_Diagrams.Voronoi_Diagram;
      Xs, Ys            : array (1 .. System_Count) of Real;
      Retries           : Natural := 0;
   begin
      if Reset_Seed then
         Reset (Gen);
      end if;

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
            X, Y : Real := 0.0;
         begin
            Xs (I) := 0.0;
            Ys (I) := 0.0;
            if I > 1 or else not Concorde.Scenarios.Imperial_Centre then
               while D < Min_Distance loop
                  Retries := Retries + 1;
                  D :=  Non_Negative_Real'Last;
                  X := Real (Random (Gen) * 2.0 - 1.0);
                  Y := Real (Random (Gen) * 2.0 - 1.0);
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
            end if;

            Xs (I) := X;
            Ys (I) := Y;
            Influence.Add_Point (X, Y);

         end;

      end loop;

      Ada.Text_IO.Put_Line
        ("retries:" & Integer'Image (Retries - System_Count));

      Influence.Generate;

      Ada.Text_IO.Put_Line ("created Voronoi map");

      for I in 1 .. System_Count loop
         declare
            Boundary : Concorde.Systems.System_Influence_Boundary
              (1 .. Influence.Vertex_Count (I));
            Name     : constant String :=
                         WL.Random.Names.Random_Name
                           (Name_Generator);
         begin
            for J in Boundary'Range loop
               Boundary (J) :=
                 (Influence.Vertex_X (I, J), Influence.Vertex_Y (I, J));
            end loop;

            declare
               System : constant Concorde.Systems.Star_System_Type :=
                          Concorde.Systems.Create.New_System
                            (I, Name, Xs (I), Ys (I), Boundary,
                             Production => 0.025,
                             Capacity   => 2.0);
            begin
               Galaxy_Graph.Append (System);
            end;
         end;
      end loop;

      Ada.Text_IO.Put_Line ("created" & System_Count'Img & " systems");

      if Concorde.Scenarios.Imperial_Centre then
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
         begin
            for J in 2 .. System_Count loop
               declare
                  DX : constant Real := Xs (J) - Xs (1);
                  DY : constant Real := Ys (J) - Ys (1);
                  Direction : constant Positive := Direction_Index (DX, DY);
                  Distance  : constant Non_Negative_Real :=
                                DX ** 2 + DY ** 2;
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
               Total_Connections := Total_Connections + 1;
            end loop;
         end;
      end if;

      for I in 1 .. System_Count loop
         declare
            function Sqrt (X : Non_Negative_Real) return Non_Negative_Real
                           renames Concorde.Elementary_Functions.Sqrt;
            System : constant Star_System_Type :=
                       Galaxy_Graph.Vertex (I);
            X      : constant Real := System.X;
            Y      : constant Real := System.Y;
            Max_Connections : constant Natural :=
                                (if Concorde.Scenarios.Imperial_Centre
                                 then Natural'Max (10, Average_Connections)
                                 else Average_Connections);
            Ds     : array (1 .. Max_Connections) of Non_Negative_Real :=
                                (others => Real'Last);
            To              : array (1 .. Max_Connections) of Natural :=
                                (others => 0);
            Count  : Natural := Average_Connections;
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
                        Index : Natural := Count;
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
      Ada.Float_Text_IO.Put (Float (Total_Connections) / Float (System_Count),
                             0, 2, 0);
      Ada.Text_IO.New_Line;
   end Create_Galaxy;

end Concorde.Galaxy.Create;
