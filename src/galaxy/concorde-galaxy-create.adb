with Ada.Text_IO;
with Ada.Float_Text_IO;

with Ada.Numerics.Float_Random;

with Concorde.Elementary_Functions;
with Concorde.Systems.Create;

package body Concorde.Galaxy.Create is

   -------------------
   -- Create_Galaxy --
   -------------------

   procedure Create_Galaxy
     (System_Count        : Natural;
      Average_Connections : Positive;
      Reset_Seed          : Boolean)
   is
      use Ada.Numerics.Float_Random;
      use Concorde.Systems;
      Total_Connections : Natural := 0;
      Gen : Generator;
   begin
      if Reset_Seed then
         Reset (Gen);
      end if;

      for I in 1 .. System_Count loop
         declare
            X : constant Real := Real (Random (Gen) * 2.0 - 1.0);
            Y : constant Real := Real (Random (Gen) * 2.0 - 1.0);
            System : constant Star_System_Access :=
                       Concorde.Systems.Create.New_System
                         (I, "System" & I'Img, X, Y,
                          Production => 0.1,
                          Capacity   => 10.0);
         begin
            Galaxy_Graph.Append (Concorde.Systems.Star_System_Type (System));
            Galaxy_Vector.Append (System);
         end;
      end loop;

      for I in 1 .. System_Count loop
         declare
            function Sqrt (X : Non_Negative_Real) return Non_Negative_Real
                           renames Concorde.Elementary_Functions.Sqrt;
            System : constant Star_System_Type :=
                       Galaxy_Graph.Vertex (I);
            X      : constant Real := System.X;
            Y      : constant Real := System.Y;
            Ds     : array (1 .. Average_Connections) of Non_Negative_Real :=
                       (others => Real'Last);
            To     : array (1 .. Average_Connections) of Natural :=
                       (others => 0);
            Count  : Natural := Average_Connections;
         begin
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
