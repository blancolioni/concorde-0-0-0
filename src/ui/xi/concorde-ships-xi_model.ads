with Xi.Camera;
with Xi.Node;
with Xi.Scene;

with Concorde.Scripts;

with Concorde.Xi_UI;

package Concorde.Ships.Xi_Model is

   type Active_Ship is private;

   procedure Transit_To_Ship
     (Ship   : Ship_Type;
      Model  : in out Concorde.Xi_UI.Root_Xi_Model'Class);

   function Get_Active_Ship
     (Ship : Ship_Type)
      return Active_Ship;

   function Activate_Ship
     (Ship    : Ship_Type;
      Scene   : Xi.Scene.Xi_Scene;
      Primary : Xi.Node.Xi_Node)
      return Active_Ship;

   procedure Update_Ship
     (Ship : Active_Ship);

   function Local_Camera
     (Ship : Active_Ship)
      return Xi.Camera.Xi_Camera;

   function Ship_Node
     (Ship : Active_Ship)
      return Xi.Node.Xi_Node;

   function Script
     (Ship : Active_Ship)
      return Concorde.Scripts.Concorde_Script;

   procedure Set_Script
     (Ship   : Active_Ship;
      Script : Concorde.Scripts.Concorde_Script);

private

   type Active_Ship_Record;

   type Active_Ship is access Active_Ship_Record;

end Concorde.Ships.Xi_Model;
