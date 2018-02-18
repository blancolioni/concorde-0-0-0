with Xtk.Grids.Models.List_Model;
with Xtk.Values;

with Concorde.Ships;

package Concorde.Xi_UI.Ships is

   function To_Value
     (Ship : not null access constant Concorde.Ships.Root_Ship_Type'Class)
      return Xtk.Values.Xtk_Value_Interface'Class;

   type Ship_Summary_Model_Record is
     new Xtk.Grids.Models.List_Model.Xtk_List_Model_Record with private;

   type Ship_Summary_Model is access all Ship_Summary_Model_Record'Class;

   function New_Ship_Summary return Ship_Summary_Model;

   procedure Add_Ship
     (Model : in out Ship_Summary_Model_Record;
      Ship  : not null access constant Concorde.Ships.Root_Ship_Type'Class);

   procedure Remove_Ship
     (Model : in out Ship_Summary_Model_Record;
      Ship  : not null access constant Concorde.Ships.Root_Ship_Type'Class);

   function Ships_Overlay
     (Model : Ship_Summary_Model)
      return Overlay_Type;

private

   type Ship_Value is
     new Xtk.Values.Xtk_Value_Interface with
      record
         Ship : Concorde.Ships.Ship_Type;
      end record;

   overriding function To_String
     (Value : Ship_Value)
      return String
   is (Value.Ship.Short_Description);

   type Ship_Summary_Model_Record is
     new Xtk.Grids.Models.List_Model.Xtk_List_Model_Record with null record;

end Concorde.Xi_UI.Ships;
