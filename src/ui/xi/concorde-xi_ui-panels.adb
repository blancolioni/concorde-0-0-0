with Cairo;
with Cairo.Image_Surface;
with Css;

with Xtk.Events;

with Concorde.People.Individuals.Portraits;

package body Concorde.Xi_UI.Panels is

   type Faction_Info_Panel_Record is
     new Root_Panel_Interface
     and Xtk.Events.User_Data_Interface with
      record
         Faction          : Concorde.Factions.Faction_Type;
         Panel            : Xtk.Panel.Xtk_Panel;
         Title            : Xtk.Label.Xtk_Label;
         Leader_Name      : Xtk.Label.Xtk_Label;
         Portrait_Widget  : Xtk.Widget.Xtk_Widget;
         Portrait_Surface : Cairo.Cairo_Surface;
      end record;

   type Faction_Info_Panel_Access is
     access all Faction_Info_Panel_Record'Class;

   overriding function Get_Xtk_Panel
     (Panel : Faction_Info_Panel_Record)
      return Xtk.Panel.Xtk_Panel
   is (Panel.Panel);

   overriding procedure Show
     (Panel : in out Faction_Info_Panel_Record;
      X, Y  : Natural);

   overriding procedure Move
     (Panel : in out Faction_Info_Panel_Record;
      X, Y  : Natural);

   overriding procedure Hide
     (Panel : in out Faction_Info_Panel_Record);

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
      Panel : Faction_Info_Panel_Record
        renames Faction_Info_Panel_Record (User_Data.all);
   begin
      Cairo.Set_Source_Surface (Context, Panel.Portrait_Surface, 0.0, 0.0);
      Cairo.Paint (Context);
      return Xtk.Events.Propagate_Event;
   end Draw_Portrait;

   ------------------------
   -- Faction_Info_Panel --
   ------------------------

   function Faction_Info_Panel
     (Faction : Concorde.Factions.Faction_Type)
      return Panel_Type
   is
      Panel : constant Xtk.Widget.Xtk_Widget :=
                Get_Widget ("faction-info-panel");
      Title : constant Xtk.Label.Xtk_Label :=
                Xtk.Label.Xtk_Label
                  (Panel.Get_Child_Widget_By_Id ("info-faction-name"));
      Leader_Name : constant Xtk.Label.Xtk_Label :=
                      Xtk.Label.Xtk_Label
                        (Panel.Get_Child_Widget_By_Id ("leader-name"));
      Portrait : constant Xtk.Widget.Xtk_Widget :=
                   (Panel.Get_Child_Widget_By_Id ("faction-leader-portrait"));
      Surface  : constant Cairo.Cairo_Surface :=
                   Cairo.Image_Surface.Create
                     (Cairo.Image_Surface.Cairo_Format_ARGB32,
                      102, 102);
      Cr       : constant Cairo.Cairo_Context :=
                   Cairo.Create (Surface);
      Result   : Faction_Info_Panel_Access;
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

      Result :=
        new Faction_Info_Panel_Record'
          (Faction          => Faction,
           Panel            => Xtk.Panel.Xtk_Panel (Panel),
           Title            => Title,
           Leader_Name      => Leader_Name,
           Portrait_Widget  => Portrait,
           Portrait_Surface => Surface);

      Portrait.On_Draw (Draw_Portrait'Access, Result);

      return Panel_Type (Result);

   end Faction_Info_Panel;

   ----------
   -- Hide --
   ----------

   overriding procedure Hide
     (Panel : in out Faction_Info_Panel_Record)
   is
   begin
      Panel.Panel.Hide;
   end Hide;

   ----------
   -- Move --
   ----------

   overriding procedure Move
     (Panel : in out Faction_Info_Panel_Record;
      X, Y  : Natural)
   is
   begin
      Panel.Panel.Set_Style
        ("top", Css.Pixels (Y));
      Panel.Panel.Set_Style
        ("left", Css.Pixels (X));
   end Move;

   ----------
   -- Show --
   ----------

   overriding procedure Show
     (Panel : in out Faction_Info_Panel_Record;
      X, Y  : Natural)
   is
   begin
      Panel.Move (X, Y);
      Panel.Panel.Show_All;
   end Show;

end Concorde.Xi_UI.Panels;
