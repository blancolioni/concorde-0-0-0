package body Concorde.Xi_UI.Worlds is

   Max_World_Overlays : constant := 12;

   type Root_World_Overlay is
     new Root_Overlay_Type with
      record
         Active   : Boolean := False;
         World    : Concorde.Worlds.World_Type;
         Name     : Xtk.Label.Xtk_Label;
      end record;

   type World_Overlay_Access is access all Root_World_Overlay'Class;

   Overlays : array (1 .. Max_World_Overlays) of World_Overlay_Access;

   -------------------
   -- World_Overlay --
   -------------------

   function World_Overlay
     (World : Concorde.Worlds.World_Type)
      return Overlay_Type
   is

      function New_Overlay (Index : Positive) return World_Overlay_Access;

      -----------------
      -- New_Overlay --
      -----------------

      function New_Overlay (Index : Positive) return World_Overlay_Access is
         Overlay : constant World_Overlay_Access := new Root_World_Overlay;
      begin
         Overlay.Initialize ("world-info" & Integer'Image (-Index));
         Overlay.Active := False;
         Overlay.World := null;
         Overlay.Name :=
           Xtk.Label.Xtk_Label
             (Overlay.Top_Panel.Get_Child_Widget_By_Id ("world-name"));
         return Overlay;
      end New_Overlay;

      Index : Natural := 0;

   begin

      for I in Overlays'Range loop
         if Overlays (I) = null then
            Overlays (I) := New_Overlay (I);
            Index := I;
            exit;
         elsif not Overlays (I).Active then
            Index := I;
            exit;
         end if;
      end loop;

      if Index = 0 then
         raise Constraint_Error with
           "insufficient world overlays";
      end if;

      declare
         Overlay : constant World_Overlay_Access :=
                     Overlays (Index);
      begin
         Overlay.World := World;
         Overlay.Active := True;
         Overlay.Name.Set_Text (World.Name);

         return Overlay_Type (Overlay);

      end;

   end World_Overlay;

end Concorde.Xi_UI.Worlds;
