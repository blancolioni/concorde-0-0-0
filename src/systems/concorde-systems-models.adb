with Ada.Characters.Handling;

with Lui.Rendering;

with Concorde.Solar_System;

with Concorde.Hash_Table;
with Concorde.Watchers;

with Concorde.Factions;

with Concorde.Systems.Db;

with Concorde.Stars;
with Concorde.Worlds;

with Concorde.Worlds.Tile_Models;

with Concorde.Worlds.Db;

package body Concorde.Systems.Models is

   Zoom_Limit : constant := 40.0;

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
     (Model : in out Root_Star_System_Model;
      X, Y  : Natural)
      return Lui.Models.Object_Model;

   overriding procedure Render
     (Model    : in out Root_Star_System_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   overriding procedure Zoom
     (Model : in out Root_Star_System_Model;
      Z       : in     Integer;
      Control : in     Boolean);

   type Star_System_Model_Access is
     access all Root_Star_System_Model'Class;

   package Model_Table is
     new Concorde.Hash_Table (Star_System_Model_Access);

   System_Models : Model_Table.Map;

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
        (Object : Star_System_Object_Interface'Class);

      procedure Render_Star
        (Star : Concorde.Stars.Root_Star_Type'Class);

      procedure Render_World
        (World    : Concorde.Worlds.Root_World_Type'Class);

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
        (Object   : Star_System_Object_Interface'Class)
      is
      begin
         if Object in Concorde.Stars.Root_Star_Type'Class then
            Render_Star
              (Concorde.Stars.Root_Star_Type'Class (Object));
         else
            Render_World
              (Concorde.Worlds.Root_World_Type'Class (Object));
         end if;
      end Render_System_Object;

      ------------------
      -- Render_World --
      ------------------

      procedure Render_World
        (World    : Concorde.Worlds.Root_World_Type'Class)
      is
         use Concorde.Geometry;
         Position : constant Radians := World.Orbit_Progress;
         X_Offset : constant Real := Cos (Position) * World.Semimajor_Axis;
         Y_Offset : constant Real := Sin (Position) * World.Semimajor_Axis;
         Scale_Factor : constant Non_Negative_Real :=
                          Real'Max (Real (Model.Width / 3), 200.0)
                          / Concorde.Solar_System.Earth_Orbit
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
         Render_System_Object (Object.Object.all);
      end loop;

      Model.Needs_Render := False;
   end Render;

   ---------------
   -- Select_XY --
   ---------------

   overriding function Select_XY
     (Model : in out Root_Star_System_Model;
      X, Y  : Natural)
      return Lui.Models.Object_Model
   is
   begin
      for Render of Model.Rendered_Words loop
         if X in Render.X .. Render.X + Render.W
           and then Y in Render.Y .. Render.Y + Render.H
         then
            return Concorde.Worlds.Tile_Models.World_Tile_Model
              (Render.World);
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
         System.Check_Loaded;
         Result := new Root_Star_System_Model;
         Result.Initialise (System.Name);
         Result.System := System;

         Db.Update (System.Reference, Watch'Access);

--           declare
--              Total_Pop : Real := 0.0;
--           begin
--              for Pop of System.Pops loop
--                 Total_Pop := Total_Pop + Real (Pop.Size);
--              end loop;
--              Result.Add_Property
--                ("Population",
--                 Lui.Approximate_Image (Total_Pop));
--           end;

         declare
            Star : constant Concorde.Stars.Star_Type :=
                     Concorde.Stars.Star_Type (System.Main_Object);
         begin
            Result.Add_Property ("Primary", Star.Stellar_Class);
            Result.Add_Property ("Solar masses",
                                 Lui.Approximate_Image (Star.Solar_Masses));
         end;

         System_Models.Insert (System.Name, Result);
      else
         Result := System_Models.Element (System.Name);
      end if;

      Result.Set_Eye_Position (0.0, 0.0, Zoom_Limit);

      return Lui.Models.Object_Model (Result);
   end System_Model;

   ----------
   -- Zoom --
   ----------

   overriding procedure Zoom
     (Model   : in out Root_Star_System_Model;
      Z       : in     Integer;
      Control : in     Boolean)
   is
   begin
      Lui.Models.Root_Object_Model (Model).Zoom (Z, Control);
      if Model.Eye_Z > Zoom_Limit * 1.1 then
         Model.Parent_Model.Remove_Inline_Model
           (System_Model (Model.System));
      end if;
   end Zoom;

end Concorde.Systems.Models;
