with Xtk.Grids.Views;
with Xtk.Values.Renderers;

package body Concorde.Xi_UI.Ships is

   type Root_Ships_Overlay is
     new Root_Overlay_Type with
      record
         Grid    : Xtk.Grids.Views.Xtk_Grid_View;
         Model   : Ship_Summary_Model;
      end record;

   type Ships_Overlay_Access is access all Root_Ships_Overlay'Class;

   --------------
   -- Add_Ship --
   --------------

   procedure Add_Ship
     (Model : in out Ship_Summary_Model_Record;
      Ship  : not null access constant Concorde.Ships.Root_Ship_Type'Class)
   is
   begin
      Model.Append_Row;
      Model.Set_Cell_Value
        (Row   => Model.Row_Count,
         Col   => 1,
         Value => To_Value (Ship));
   end Add_Ship;

   ----------------------
   -- New_Ship_Summary --
   ----------------------

   function New_Ship_Summary return Ship_Summary_Model is
   begin
      return Summary : constant Ship_Summary_Model :=
        new Ship_Summary_Model_Record
      do
         Summary.Create (1);
      end return;
   end New_Ship_Summary;

   -----------------
   -- Remove_Ship --
   -----------------

   procedure Remove_Ship
     (Model : in out Ship_Summary_Model_Record;
      Ship  : not null access constant Concorde.Ships.Root_Ship_Type'Class)
   is
      use Concorde.Ships;
      Ship_Row : Natural := 0;
   begin
      for Row_Index in 1 .. Model.Row_Count loop
         if Ship_Value (Model.Cell_Value (Row_Index, 1)).Ship
           = Ship_Type (Ship)
         then
            Ship_Row := Row_Index;
            exit;
         end if;
      end loop;

      if Ship_Row > 0 then
         Model.Delete_Row (Ship_Row);
      end if;
   end Remove_Ship;

   -------------------
   -- Ships_Overlay --
   -------------------

   function Ships_Overlay
     (Model : Ship_Summary_Model)
      return Overlay_Type
   is
      Overlay : constant Ships_Overlay_Access :=
                  new Root_Ships_Overlay;
   begin
      Overlay.Initialize ("ship-summary-panel");
      Overlay.Model := Model;
      Overlay.Grid :=
        Xtk.Grids.Views.Xtk_Grid_View
          (Overlay.Top_Panel.Get_Child_Widget_By_Id
             ("ship-summary-grid"));
      Overlay.Grid.Set_Model (Overlay.Model);
      Overlay.Grid.Append_Column
        (Source_Column => 1,
         Renderer      => Xtk.Values.Renderers.Text_Renderer);
      return Overlay_Type (Overlay);
   end Ships_Overlay;

   --------------
   -- To_Value --
   --------------

   function To_Value
     (Ship : not null access constant Concorde.Ships.Root_Ship_Type'Class)
      return Xtk.Values.Xtk_Value_Interface'Class
   is
   begin
      return V : Ship_Value do
         V.Ship := Concorde.Ships.Ship_Type (Ship);
      end return;
   end To_Value;

end Concorde.Xi_UI.Ships;
