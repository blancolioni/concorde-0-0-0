with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with Cairo;

with WL.Localisation;
with WL.String_Maps;

with Css;

with Xtk.Events;

with Xtk.Table_Element;
with Xtk.Widget;

with Concorde.Events;
with Concorde.Ministries;
with Concorde.People.Individuals;

with Concorde.Factions.Events;

with Concorde.Xi_UI.Portraits;

package body Concorde.Xi_UI.Factions is

   type Table_Row is
     new Xtk.Events.User_Data_Interface with
      record
         Ministry         : Concorde.Ministries.Ministry_Type;
         Minister         : Concorde.People.Individuals.Individual_Type;
         Ministry_Name    : Xtk.Label.Xtk_Label;
         Minister_Name    : Xtk.Label.Xtk_Label;
         Portrait_Widget  : Concorde.Xi_UI.Portraits.Xtk_Portrait;
         Portrait_Surface : Cairo.Cairo_Surface;
      end record;

   type Table_Row_Access is access all Table_Row'Class;

   package Table_Row_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Table_Row_Access);

   type Root_Faction_Overlay is
     new Root_Overlay_Type with
      record
         Faction : Concorde.Factions.Faction_Type;
         Title   : Xtk.Label.Xtk_Label;
         Table   : Xtk.Table_Element.Xtk_Table;
         Rows    : Table_Row_Lists.List;
      end record;

   type Faction_Overlay_Access is access all Root_Faction_Overlay'Class;

   package Overlay_Maps is
     new WL.String_Maps (Faction_Overlay_Access);

   Overlay_Map : Overlay_Maps.Map;

   function New_Faction_Overlay
     (Faction : Concorde.Factions.Faction_Type)
      return Faction_Overlay_Access;

   procedure On_Ministry_Assigned
     (Event  : Concorde.Events.Root_Event_Type'Class;
      Object : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   procedure Update_Table
     (Overlay : Faction_Overlay_Access);

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

   begin
      Overlay.Initialize ("faction-info-panel");

      Overlay.Faction := Faction;

      Overlay.Title :=
          Xtk.Label.Xtk_Label
            (Overlay.Top_Panel.Get_Child_Widget_By_Id
               ("info-faction-name"));

      Overlay.Title.Set_Label ("House " & Faction.Name);

      Overlay.Table :=
        Xtk.Table_Element.Xtk_Table
          (Overlay.Top_Panel.Get_Child_Widget_By_Id
             ("info-table"));

      Update_Table (Overlay);

      Faction.Update.Add_Handler
        (Signal  => Concorde.Factions.Events.Signal_Ministry_Changed,
         Handler => On_Ministry_Assigned'Access);

      return Overlay;

   end New_Faction_Overlay;

   --------------------------
   -- On_Ministry_Assigned --
   --------------------------

   procedure On_Ministry_Assigned
     (Event  : Concorde.Events.Root_Event_Type'Class;
      Object : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is
      use Cairo;
      use Concorde.People.Individuals;
      Ev      : Concorde.Factions.Events.Ministry_Changed_Event'Class renames
                  Concorde.Factions.Events.Ministry_Changed_Event'Class
                    (Event);
      Overlay : constant Faction_Overlay_Access :=
                  (if Overlay_Map.Contains (Object.Identifier)
                   then Overlay_Map.Element (Object.Identifier)
                   else null);
   begin
      if Ev.Old_Minister /= null then
         Ada.Text_IO.Put_Line
           (Object.Identifier & ": " & Ev.Ministry.Name
            & ": fired " & Ev.Old_Minister.Full_Name);
      end if;
      if Ev.New_Minister /= null then
         Ada.Text_IO.Put_Line
           (Object.Identifier & ": " & Ev.Ministry.Name
            & ": appointed " & Ev.New_Minister.Full_Name);
      end if;

      Update_Table (Overlay);

--        if Overlay /= null then
--           for W of Overlay.Widgets loop
--              if W.Office = Ev.Office then
--                 if Ev.New_Minister /= null then
--                    W.Minister_Name.Set_Label (Ev.New_Minister.Full_Name);
--                 else
--                    W.Minister_Name.Set_Label ("Vacant");
--                 end if;
--                 if W.Portrait_Surface /= Null_Surface then
--                    Surface_Destroy (W.Portrait_Surface);
--                 end if;
--                 W.Portrait_Surface := Create_Surface (Ev.New_Minister);
--                 W.Portrait_Widget.Queue_Draw;
--                 exit;
--              end if;
--           end loop;
--        end if;
   end On_Ministry_Assigned;

   ------------------
   -- Update_Table --
   ------------------

   procedure Update_Table
     (Overlay : Faction_Overlay_Access)
   is
      procedure Update_Widgets (Ministry : Concorde.Ministries.Ministry_Type);

      --------------------
      -- Update_Widgets --
      --------------------

      procedure Update_Widgets
        (Ministry : Concorde.Ministries.Ministry_Type)
      is

         use Concorde.People.Individuals;
         use Concorde.Ministries;

         Found : Boolean := False;

         use type Xtk.Widget.Xtk_Widget;
         use type Concorde.People.Individuals.Individual_Type;

         procedure Update_Row
           (Row : Table_Row_Access);

         ----------------
         -- Update_Row --
         ----------------

         procedure Update_Row
           (Row : Table_Row_Access)
         is
         begin
            Row.Minister_Name.Set_Label
              (if Ministry.Minister = null
               then WL.Localisation.Local_Text ("vacant")
               else Ministry.Minister.Full_Name);
            Row.Portrait_Widget.Set_Portrait
              (Ministry.Minister);
         end Update_Row;

      begin

         for Row of Overlay.Rows loop
            if Row.Ministry = Ministry then
               declare
                  Current : constant Individual_Type :=
                              Individual_Type (Ministry.Minister);
               begin
                  if Row.Minister /= Current then
                     Update_Row (Row);
                  end if;
               end;
               Found := True;
               exit;
            end if;
         end loop;

         if not Found then
            declare
               Row : constant Table_Row_Access :=
                       new Table_Row'
                         (Ministry         => Ministry,
                          Minister         => Ministry.Minister,
                          Minister_Name    => null,
                          Ministry_Name    => null,
                          Portrait_Widget  => null,
                          Portrait_Surface => Cairo.Null_Surface);
               TR  : constant Xtk.Table_Element.Xtk_Table_Row :=
                       Xtk.Table_Element.Xtk_New;
               TD  : Xtk.Table_Element.Xtk_Table_Data;
            begin
               Xtk.Table_Element.Xtk_New (TD);
               Concorde.Xi_UI.Portraits.Xtk_New (Row.Portrait_Widget);
               Row.Portrait_Widget.Set_Attribute
                 ("class", "portrait-medium");
--                 Row.Portrait_Widget.On_Draw
--                   (Draw_Portrait'Access, Row);
               TD.Add_Child (Row.Portrait_Widget);
               TR.Add_Child (TD);
               Xtk.Label.Xtk_New (Row.Ministry_Name, Ministry.Name);
               Xtk.Table_Element.Xtk_New (TD);
               TD.Add_Child (Row.Ministry_Name);
               TR.Add_Child (TD);
               Xtk.Label.Xtk_New (Row.Minister_Name, "Jimbo");
               Xtk.Table_Element.Xtk_New (TD);
               TD.Add_Child (Row.Minister_Name);
               TR.Add_Child (TD);
               Overlay.Table.Add_Child (TR);
               Overlay.Rows.Append (Row);
               Update_Row (Row);
            end;
         end if;
      end Update_Widgets;

   begin
      Overlay.Faction.Scan_Ministries (Update_Widgets'Access);
      Overlay.Table.Show_All;
   end Update_Table;

end Concorde.Xi_UI.Factions;
