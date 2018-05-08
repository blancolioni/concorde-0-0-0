with Xi.Assets;
with Xi.Materials.Material;
with Xi.Shapes;
with Xi.Render_Operation;

package body Concorde.Ships.Components.Xi_Model is

   Use_Panels : constant Boolean := False;

   Entities : array (Component_Shape) of Xi.Entity.Xi_Entity;

   function Get_Entity
     (Component : not null access constant Root_Component_Type'Class)
      return Xi.Entity.Xi_Entity;

   procedure Create_Panel
     (Entity     : Xi.Entity.Xi_Entity;
      X1, X2     : Xi.Xi_Float;
      Y1, Y2     : Xi.Xi_Float;
      Z1, Z2     : Xi.Xi_Float;
      Clockwise  : Boolean;
      Panel_Size : Xi.Xi_Non_Negative_Float := 1.0);

   ----------------------
   -- Component_Entity --
   ----------------------

   function Component_Entity
     (Component : not null access constant Root_Component_Type'Class)
      return Xi.Entity.Xi_Entity
   is
   begin
      return Get_Entity (Component);
   end Component_Entity;

   ------------------
   -- Create_Panel --
   ------------------

   procedure Create_Panel
     (Entity     : Xi.Entity.Xi_Entity;
      X1, X2     : Xi.Xi_Float;
      Y1, Y2     : Xi.Xi_Float;
      Z1, Z2     : Xi.Xi_Float;
      Clockwise  : Boolean;
      Panel_Size : Xi.Xi_Non_Negative_Float := 1.0)
   is
      use Xi;

      DX : constant Xi_Float := X2 - X1;
      DY : constant Xi_Float := Y2 - Y1;
      DZ : constant Xi_Float := Z2 - Z1;

      Step_X : constant Xi_Signed_Unit_Float :=
                 (if DX < 0.0 then -1.0 elsif DX > 0.0 then 1.0 else 0.0);
      Step_Y : constant Xi_Signed_Unit_Float :=
                 (if DY < 0.0 then -1.0 elsif DY > 0.0 then 1.0 else 0.0);
      Step_Z : constant Xi_Signed_Unit_Float :=
                 (if DZ < 0.0 then -1.0 elsif DZ > 0.0 then 1.0 else 0.0);

      X  : Xi_Float := Xi_Float'Min (X1, X2);
      Y  : Xi_Float := Xi_Float'Min (Y1, Y2);
      Z  : Xi_Float := Xi_Float'Min (Z1, Z2);

      Pieces : constant Natural :=
                 Natural (Xi_Float'Max (abs DX, 1.0)
                          * Xi_Float'Max (abs DY, 1.0)
                          * Xi_Float'Max (abs DZ, 1.0)
                          / Panel_Size / Panel_Size);

      procedure Vertex (A, B : Xi_Unit_Float);

      ------------
      -- Vertex --
      ------------

      procedure Vertex (A, B : Xi_Unit_Float) is
         procedure V (X, Y, Z : Xi_Float);

         -------
         -- V --
         -------

         procedure V (X, Y, Z : Xi_Float) is
         begin
            Entity.Vertex (X, Y, Z);
         end V;

      begin
         Entity.Texture_Coordinate (A, B);

         if DX = 0.0 then
            Entity.Normal (1.0, 0.0, 0.0);
            V (X, Y + A * Panel_Size, Z + B * Panel_Size);
         elsif DY = 0.0 then
            Entity.Normal (0.0, 1.0, 0.0);
            V (X + B * Panel_Size, Y, Z + A * Panel_Size);
         else
            Entity.Normal (0.0, 0.0, 1.0);
            V (X + A * Panel_Size, Y + B * Panel_Size, Z);
         end if;
      end Vertex;

   begin

      for I in 1 .. Pieces loop
         if Clockwise then
            Vertex (0.0, 1.0);
            Vertex (1.0, 0.0);
            Vertex (0.0, 0.0);

            Vertex (0.0, 1.0);
            Vertex (1.0, 1.0);
            Vertex (1.0, 0.0);
         else
            Vertex (0.0, 0.0);
            Vertex (1.0, 0.0);
            Vertex (0.0, 1.0);

            Vertex (1.0, 0.0);
            Vertex (1.0, 1.0);
            Vertex (0.0, 1.0);
         end if;

         if Step_X = 0.0 then
            Y := Y + Step_Y;
            if Y >= Y2 then
               Y := Y1;
               Z := Z + Step_Z;
            end if;
         elsif Step_Y = 0.0 then
            Z := Z + Step_Z;
            if Z >= Z2 then
               Z := Z1;
               X := X + Step_X;
            end if;
         else
            X := X + Step_X;
            if X >= X2 then
               X := X1;
               Y := Y + Step_Y;
            end if;
         end if;

      end loop;
   end Create_Panel;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Component : not null access constant Root_Component_Type'Class)
      return Xi.Entity.Xi_Entity
   is
      use type Xi.Entity.Xi_Entity;
      Entity : Xi.Entity.Xi_Entity renames Entities (Component.Shape);

   begin
      if Entity = null then
         case Component.Shape is
            when Sphere =>
               Entity := Xi.Shapes.Icosohedral_Sphere (2);

            when Cylinder =>
               Entity :=
                 Xi.Shapes.Quadric_Cylinder
                   (Slices => 16,
                    Stacks => 10);

            when Cone =>
               Entity :=
                 Xi.Shapes.Quadric_Cone
                   (Slices => 16,
                    Stacks => 10);

            when Conical_Frustum =>
               Entity :=
                 Xi.Shapes.Quadric_Conical_Frustum
                   (Slices       => 16,
                    Stacks       => 10,
                    Radius_Ratio => 0.5);

            when Rectangular_Prism | Hexagonal_Prism | Cube =>
               if Use_Panels then
                  Entity := Xi.Entity.Create;

                  Entity.Begin_Operation
                    (Xi.Render_Operation.Triangle_List);

                  declare
                     DX : constant := 0.5;
                     DY : constant := 0.5;
                     DZ : constant := 0.5;
                  begin
                     Create_Panel
                       (Entity, -DX, DX, -DY, DY, -DZ, -DZ, True);
                     Create_Panel
                       (Entity, -DX, DX, -DY, DY, DZ, DZ, False);
                     Create_Panel
                       (Entity, -DX, DX, -DY, -DY, -DZ, DZ, True);
                     Create_Panel
                       (Entity, -DX, DX, DY, DY, -DZ, DZ, False);
                     Create_Panel
                       (Entity, -DX, -DX, -DY, DY, -DZ, DZ, True);
                     Create_Panel
                       (Entity, DX, DX, -DY, DY, -DZ, DZ, False);
                  end;

                  Entity.End_Operation;

               else
                  Entity := Xi.Shapes.Cube;
               end if;
         end case;

         declare
            Material  : constant Xi.Materials.Material.Xi_Material :=
                          Xi.Assets.Material
                            ("Concorde.Ships.Components."
                             & Component.Identifier);
         begin
            Entity.Set_Material (Material);
         end;

      end if;

      return Entities (Component.Shape);
   end Get_Entity;

   --------------------------
   -- Scale_Component_Node --
   --------------------------

   procedure Scale_Component_Node
     (Component : not null access constant Root_Component_Type'Class;
      Node      : Xi.Node.Xi_Node)
   is
      Box : Bounding_Box_Type renames Component.Bounding_Box;
   begin
      Node.Scale
        (Box.X2 - Box.X1, Box.Y2 - Box.Y1, Box.Z2 - Box.Z1);
   end Scale_Component_Node;

end Concorde.Ships.Components.Xi_Model;
