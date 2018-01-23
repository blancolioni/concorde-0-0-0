with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with Cairo;
with Cairo.Image_Surface;

with WL.String_Maps;

with Css;

with Xtk.Events;
with Xtk.Widget;

with Concorde.Events;
with Concorde.Offices;
with Concorde.People.Individuals.Portraits;

with Concorde.Factions.Events;

package body Concorde.Xi_UI.Factions is

   type Office_Widgets is
     new Xtk.Events.User_Data_Interface with
      record
         Office           : Concorde.Offices.Office_Type;
         Minister_Name    : Xtk.Label.Xtk_Label;
         Portrait_Widget  : Xtk.Widget.Xtk_Widget;
         Portrait_Surface : Cairo.Cairo_Surface;
      end record;

   type Office_Widget_Access is access all Office_Widgets'Class;

   package Office_Widget_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Office_Widget_Access);

   type Root_Faction_Overlay is
     new Root_Overlay_Type with
      record
         Faction : Concorde.Factions.Faction_Type;
         Title   : Xtk.Label.Xtk_Label;
         Widgets : Office_Widget_Lists.List;
      end record;

   type Faction_Overlay_Access is access all Root_Faction_Overlay'Class;

   package Overlay_Maps is
     new WL.String_Maps (Faction_Overlay_Access);

   Overlay_Map : Overlay_Maps.Map;

   function Draw_Portrait
     (Widget    : not null access Xtk.Widget.Xtk_Widget_Record'Class;
      Context   : Cairo.Cairo_Context;
      User_Data : Xtk.Events.User_Data)
      return Xtk.Events.Event_Response;

   function New_Faction_Overlay
     (Faction : Concorde.Factions.Faction_Type)
      return Faction_Overlay_Access;

   procedure On_Office_Assigned
     (Event  : Concorde.Events.Root_Event_Type'Class;
      Object : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   -------------------
   -- Draw_Portrait --
   -------------------

   function Draw_Portrait
     (Widget    : not null access Xtk.Widget.Xtk_Widget_Record'Class;
      Context   : Cairo.Cairo_Context;
      User_Data : Xtk.Events.User_Data)
      return Xtk.Events.Event_Response
   is
      pragma Unreferenced (Widget);
      Ws : constant Office_Widget_Access :=
             Office_Widget_Access (User_Data);
   begin
      Cairo.Set_Source_Surface (Context, Ws.Portrait_Surface, 0.0, 0.0);
      Cairo.Paint (Context);
      return Xtk.Events.Propagate_Event;
   end Draw_Portrait;

   ---------------------
   -- Faction_Overlay --
   ---------------------

   function Faction_Overlay
     (Faction : Concorde.Factions.Faction_Type)
      return Overlay_Type
   is
   begin
      if not Overlay_Map.Contains (Faction.Identifier) then
         Overlay_Map.Insert
           (Faction.Identifier, New_Faction_Overlay (Faction));
      end if;
      return Overlay_Type (Overlay_Map.Element (Faction.Identifier));
   end Faction_Overlay;

   -------------------------
   -- New_Faction_Overlay --
   -------------------------

   function New_Faction_Overlay
     (Faction : Concorde.Factions.Faction_Type)
      return Faction_Overlay_Access
   is
      Overlay : constant Faction_Overlay_Access :=
                  new Root_Faction_Overlay;

      procedure Add_Widgets (Office : Concorde.Offices.Office_Type);

      -----------------
      -- Add_Widgets --
      -----------------

      procedure Add_Widgets (Office : Concorde.Offices.Office_Type) is
         use type Xtk.Widget.Xtk_Widget;
         use type Concorde.People.Individuals.Individual_Type;
         Panel    : constant Xtk.Panel.Xtk_Panel := Overlay.Top_Panel;
         Name     : constant Xtk.Label.Xtk_Label :=
                      Xtk.Label.Xtk_Label
                        (Panel.Get_Child_Widget_By_Id
                           ("faction-" & Office.Identifier & "-name"));
         Portrait : constant Xtk.Widget.Xtk_Widget :=
                      (Panel.Get_Child_Widget_By_Id
                         ("faction-" & Office.Identifier & "-portrait"));
         Widgets  : Office_Widget_Access;

      begin
         if Portrait = null then
            return;
         end if;

         if Faction.Has_Minister (Office) then
            Name.Set_Label (Faction.Minister (Office).Full_Name);
         else
            Name.Set_Label ("Vacant");
         end if;

         declare
            Surface     : constant Cairo.Cairo_Surface :=
                            Cairo.Image_Surface.Create
                              (Cairo.Image_Surface.Cairo_Format_ARGB32,
                               102, 102);
            Cr          : constant Cairo.Cairo_Context :=
                            Cairo.Create (Surface);

         begin
            Cairo.Save (Cr);
            Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Clear);
            Cairo.Paint (Cr);
            Cairo.Restore (Cr);

            if Faction.Has_Minister (Office) then
               Concorde.People.Individuals.Portraits.Draw_Portrait
                 (Cr, Faction.Minister (Office), 102, 102);
            end if;

            Cairo.Destroy (Cr);

            Widgets :=
              new Office_Widgets'
                (Office           => Office,
                 Minister_Name    => Name,
                 Portrait_Widget  => Portrait,
                 Portrait_Surface => Surface);

         end;

         Portrait.On_Draw (Draw_Portrait'Access, Widgets);
         Overlay.Widgets.Append (Widgets);

      end Add_Widgets;

   begin
      Overlay.Initialize ("faction-info-panel");

      Overlay.Faction := Faction;

      Overlay.Title :=
          Xtk.Label.Xtk_Label
            (Overlay.Top_Panel.Get_Child_Widget_By_Id
               ("info-faction-name"));

            Overlay.Title.Set_Label ("House " & Faction.Name);

      Concorde.Offices.Scan_Offices (Add_Widgets'Access);

      Faction.Update.Add_Handler
        (Signal  => Concorde.Factions.Events.Signal_Office_Changed,
         Handler => On_Office_Assigned'Access);

      return Overlay;

   end New_Faction_Overlay;

   ------------------------
   -- On_Office_Assigned --
   ------------------------

   procedure On_Office_Assigned
     (Event  : Concorde.Events.Root_Event_Type'Class;
      Object : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is
      use Concorde.Offices;
      use Concorde.People.Individuals;
      Ev      : Concorde.Factions.Events.Office_Changed_Event'Class renames
                  Concorde.Factions.Events.Office_Changed_Event'Class
                    (Event);
      Overlay : constant Faction_Overlay_Access :=
                  (if Overlay_Map.Contains (Object.Identifier)
                   then Overlay_Map.Element (Object.Identifier)
                   else null);
   begin
      if Ev.Old_Minister /= null then
         Ada.Text_IO.Put_Line
           (Object.Identifier & ": " & Ev.Office.Name
            & ": fired " & Ev.Old_Minister.Full_Name);
      end if;
      if Ev.New_Minister /= null then
         Ada.Text_IO.Put_Line
           (Object.Identifier & ": " & Ev.Office.Name
            & ": appointed " & Ev.New_Minister.Full_Name);
      end if;

      if Overlay /= null then
         for W of Overlay.Widgets loop
            if W.Office = Ev.Office then
               if Ev.New_Minister /= null then
                  W.Minister_Name.Set_Label (Ev.New_Minister.Full_Name);
               else
                  W.Minister_Name.Set_Label ("Vacant");
               end if;
               exit;
            end if;
         end loop;
      end if;
   end On_Office_Assigned;

end Concorde.Xi_UI.Factions;
