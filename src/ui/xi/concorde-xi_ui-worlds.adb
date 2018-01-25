with WL.Localisation;

package body Concorde.Xi_UI.Worlds is

   Max_World_Overlays : constant := 12;

   type Root_World_Overlay is
     new Root_Overlay_Type with
      record
         Active   : Boolean := False;
         World    : Concorde.Worlds.World_Type;
         Name     : Xtk.Label.Xtk_Label;
         Category : Xtk.Label.Xtk_Label;
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

         function Child_Label (Id : String) return Xtk.Label.Xtk_Label
         is (Xtk.Label.Xtk_Label
             (Overlay.Top_Panel.Get_Child_Widget_By_Id (Id)));

      begin
         Overlay.Initialize ("world-info" & Integer'Image (-Index));
         Overlay.Active := False;
         Overlay.World := null;
         Overlay.Name := Child_Label ("world-name");
         Overlay.Category := Child_Label ("world-category");
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
         Category : constant String :=
                      WL.Localisation.Local_Text
                        (Concorde.Worlds.World_Category'Image
                           (World.Category));
      begin
         Overlay.World := World;
         Overlay.Active := True;
         Overlay.Name.Set_Label (World.Name);
         Overlay.Category.Set_Label (Category);

         return Overlay_Type (Overlay);

      end;

   end World_Overlay;

end Concorde.Xi_UI.Worlds;
