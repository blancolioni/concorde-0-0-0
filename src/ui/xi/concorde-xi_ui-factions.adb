with Cairo;
with Cairo.Image_Surface;
with Css;

with Xtk.Events;

with Concorde.People.Individuals.Portraits;

package body Concorde.Xi_UI.Factions is

   type Root_Faction_Overlay is
     new Root_Overlay_Type
     and Xtk.Events.User_Data_Interface with
      record
         Faction          : Concorde.Factions.Faction_Type;
         Title            : Xtk.Label.Xtk_Label;
         Leader_Name      : Xtk.Label.Xtk_Label;
         Portrait_Widget  : Xtk.Widget.Xtk_Widget;
         Portrait_Surface : Cairo.Cairo_Surface;
      end record;

   type Faction_Overlay_Access is access all Root_Faction_Overlay'Class;

   function Draw_Portrait
     (Widget    : not null access Xtk.Widget.Xtk_Widget_Record'Class;
      Context   : Cairo.Cairo_Context;
      User_Data : Xtk.Events.User_Data)
      return Xtk.Events.Event_Response;

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
      Overlay : constant Faction_Overlay_Access :=
                  Faction_Overlay_Access (User_Data);
   begin
      Cairo.Set_Source_Surface (Context, Overlay.Portrait_Surface, 0.0, 0.0);
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
      Overlay : constant Faction_Overlay_Access :=
                  new Root_Faction_Overlay;
   begin
      Overlay.Initialize ("faction-info-panel");

      declare
         Panel       : constant Xtk.Panel.Xtk_Panel :=
                         Overlay.Top_Panel;
         Title       : constant Xtk.Label.Xtk_Label :=
                         Xtk.Label.Xtk_Label
                           (Panel.Get_Child_Widget_By_Id
                              ("info-faction-name"));
         Leader_Name : constant Xtk.Label.Xtk_Label :=
                         Xtk.Label.Xtk_Label
                           (Panel.Get_Child_Widget_By_Id ("leader-name"));
         Portrait    : constant Xtk.Widget.Xtk_Widget :=
                         (Panel.Get_Child_Widget_By_Id
                            ("faction-leader-portrait"));
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

         Concorde.People.Individuals.Portraits.Draw_Portrait
           (Cr, Faction.Leader, 102, 102);
         Cairo.Destroy (Cr);

         Title.Set_Label ("House " & Faction.Name);
         Leader_Name.Set_Label (Faction.Leader.Full_Name);

         Overlay.Faction := Faction;
         Overlay.Title := Title;
         Overlay.Leader_Name := Leader_Name;
         Overlay.Portrait_Widget := Portrait;
         Overlay.Portrait_Surface := Surface;

         Portrait.On_Draw (Draw_Portrait'Access, Overlay);

         return Overlay_Type (Overlay);
      end;

   end Faction_Overlay;

end Concorde.Xi_UI.Factions;
