with Xi.Camera;
with Xi.Matrices;
with Xi.Node;
with Xi.Scene;

--  with Concorde.Scripts;

with Concorde.Calendar;
with Concorde.Xi_UI;

with Concorde.Ships.Vessels;

package Concorde.Ships.Xi_Model is

   type Active_Ship is private;

   procedure Transit_To_Ship
     (Ship  : not null access constant
        Concorde.Ships.Vessels.Root_Vessel_Type'Class;
      Start : Concorde.Calendar.Time;
      Model : in out Concorde.Xi_UI.Root_Xi_Model'Class);

   function Get_Active_Ship
     (Ship : not null access constant
        Concorde.Ships.Vessels.Root_Vessel_Type'Class)
      return Active_Ship;

   function Get_Ship
     (Active : Active_Ship)
      return Concorde.Ships.Ship_Type;

   function Activate_Ship
     (Ship     : not null access constant
        Concorde.Ships.Vessels.Root_Vessel_Type'Class;
      Time     : Concorde.Calendar.Time;
      Scene    : Xi.Scene.Xi_Scene;
      Primary  : Xi.Node.Xi_Node;
      Selector : Xi.Node.Xi_Node)
      return Active_Ship;

   procedure Deactivate_Ship
     (Ship     : not null access constant
        Concorde.Ships.Vessels.Root_Vessel_Type'Class);

   procedure Update_Ship_Position
     (Ship          : Active_Ship;
      Time          : Concorde.Calendar.Time;
      Relative_To   : Xi.Matrices.Vector_3;
      Camera        : Xi.Camera.Xi_Camera;
      Show_Selector : Boolean);

   function Local_Camera
     (Ship : Active_Ship)
      return Xi.Camera.Xi_Camera;

   function Position_Node
     (Ship : Active_Ship)
      return Xi.Node.Xi_Node;

--     function Script
--       (Ship : Active_Ship)
--        return Concorde.Scripts.Concorde_Script;
--
--     procedure Set_Script
--       (Ship   : Active_Ship;
--        Script : Concorde.Scripts.Concorde_Script);

private

   type Active_Ship_Record;

   type Active_Ship is access Active_Ship_Record;

end Concorde.Ships.Xi_Model;
