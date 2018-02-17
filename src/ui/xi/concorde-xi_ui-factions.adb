with Ada.Containers.Vectors;

with Ada.Text_IO;

--  with Cairo;
--
--  with WL.Localisation;
with WL.String_Maps;

with Css;

with Xtk.Grids.Models;
with Xtk.Grids.Views;
with Xtk.Values.Renderers;

with Xtk.Widget;

with Concorde.Events;
with Concorde.Ministries;
with Concorde.People.Individuals;

with Concorde.Factions.Events;

with Concorde.Xi_UI.Individuals;
--  with Concorde.Xi_UI.Portraits;

package body Concorde.Xi_UI.Factions is

   type Overlay_Row is
      record
         Ministry : Concorde.Ministries.Ministry_Type;
         Minister : Concorde.People.Individuals.Individual_Type;
      end record;

   package Ministry_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Overlay_Row);

   subtype Column_Index is Positive range 1 .. 2;

   Minister_Column : constant Column_Index := 1;
   Ministry_Column : constant Column_Index := 2;

   type Faction_Overlay_Model_Record is
     new Xtk.Grids.Models.Xtk_Grid_Model_Record with
      record
         Ministries : Ministry_Vectors.Vector;
      end record;

   overriding function Row_Count
     (Model : Faction_Overlay_Model_Record)
      return Natural
   is (Model.Ministries.Last_Index);

   overriding function Col_Count
     (Model : Faction_Overlay_Model_Record)
      return Natural
   is (Column_Index'Last);

   overriding function Cell_Value
     (Model : Faction_Overlay_Model_Record;
      Row   : Positive;
      Col   : Positive)
      return Xtk.Values.Xtk_Value_Interface'Class;

   overriding procedure Set_Cell_Value
     (Model : in out Faction_Overlay_Model_Record;
      Row   : Positive;
      Col   : Positive;
      Value : Xtk.Values.Xtk_Value_Interface'Class)
   is null;

   type Faction_Overlay_Model is access all Faction_Overlay_Model_Record'Class;

   type Root_Faction_Overlay is
     new Root_Overlay_Type with
      record
         Faction : Concorde.Factions.Faction_Type;
         Title   : Xtk.Label.Xtk_Label;
         Grid    : Xtk.Grids.Views.Xtk_Grid_View;
         Model   : Faction_Overlay_Model;
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

   ----------------
   -- Cell_Value --
   ----------------

   overriding function Cell_Value
     (Model : Faction_Overlay_Model_Record;
      Row   : Positive;
      Col   : Positive)
      return Xtk.Values.Xtk_Value_Interface'Class
   is
   begin
      case Column_Index (Col) is
         when Minister_Column =>
            return Concorde.Xi_UI.Individuals.To_Value
              (Model.Ministries.Element (Row).Minister);
         when Ministry_Column =>
            return Xtk.Values.To_Xtk_Value
              (Model.Ministries.Element (Row).Ministry.Name);
      end case;
   end Cell_Value;

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

      Overlay.Grid :=
        Xtk.Grids.Views.Xtk_Grid_View
          (Overlay.Top_Panel.Get_Child_Widget_By_Id
             ("info-table"));

      Overlay.Model :=
        new Faction_Overlay_Model_Record'
          (Xtk.Grids.Models.Xtk_Grid_Model_Record with
             Ministries => <>);

      Overlay.Grid.Set_Model (Overlay.Model);
      Overlay.Grid.Append_Column
        (1, Concorde.Xi_UI.Individuals.Portrait_Renderer);
      Overlay.Grid.Append_Column
        (2, Xtk.Values.Renderers.Text_Renderer);
      Overlay.Grid.Append_Column
        (1, Xtk.Values.Renderers.Text_Renderer);

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
--        use Cairo;
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

      use Concorde.Ministries;
      use Concorde.People.Individuals;

      New_Vector     : Ministry_Vectors.Vector;
      Old_Vector     : Ministry_Vectors.Vector renames
                         Overlay.Model.Ministries;
      Layout_Changed : Boolean := False;

      procedure Add_Ministry (Ministry : Concorde.Ministries.Ministry_Type);

      ------------------
      -- Add_Ministry --
      ------------------

      procedure Add_Ministry (Ministry : Concorde.Ministries.Ministry_Type) is
      begin
         New_Vector.Append ((Ministry, Ministry.Minister));
      end Add_Ministry;

   begin
      Overlay.Faction.Scan_Ministries (Add_Ministry'Access);

      if New_Vector.Last_Index >= Old_Vector.Last_Index then
         for I in 1 .. Old_Vector.Last_Index loop
            if New_Vector.Element (I).Ministry /=
              Old_Vector.Element (I).Ministry
            then
               Layout_Changed := True;
               exit;
            end if;
         end loop;
      else
         Layout_Changed := True;
      end if;

      if Layout_Changed then
         Overlay.Model.Ministries := New_Vector;
         Overlay.Model.Layout_Changed;
      else
         for I in 1 .. New_Vector.Last_Index loop
            if I <= Old_Vector.Last_Index then
               if Old_Vector.Element (I).Minister
                 /= New_Vector.Element (I).Minister
               then
                  Old_Vector (I).Minister := New_Vector.Element (I).Minister;
                  Overlay.Model.Cell_Changed (I, Minister_Column);
               end if;
            else
               Old_Vector.Append (New_Vector.Element (I));
               Overlay.Model.Row_Added (I - 1);
            end if;
         end loop;
      end if;

      Overlay.Grid.Show_All;

   end Update_Table;

end Concorde.Xi_UI.Factions;
