with Xi;                               use Xi;

with Xi.Camera;
with Xi.Entity.Manual;
with Xi.Frame_Event;
with Xi.Keyboard;
with Xi.Main;
with Xi.Mouse;
with Xi.Node;
with Xi.Scene;
with Xi.Render_Operation;

with Xtk.Fixed;
with Xtk.Label;

with Concorde.Galaxy;
with Concorde.Systems;

package body Concorde.Xi_UI.Galaxies is

   type Root_Galaxy_Model is
     new Root_Xi_Model with
      record
         Top_Panel : Xtk.Panel.Xtk_Panel;
         Mouse_Status : Xtk.Label.Xtk_Label;
      end record;

   overriding function Top_Panel
     (Model : Root_Galaxy_Model)
      return Xtk.Panel.Xtk_Panel
   is (Model.Top_Panel);

   Main_Model : aliased Root_Galaxy_Model;

   type Galaxy_Frame_Listener is
     new Xi.Frame_Event.Xi_Frame_Listener_Interface with
      record
         null;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Galaxy_Frame_Listener;
      Event    : Xi.Frame_Event.Xi_Frame_Event);

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out Galaxy_Frame_Listener;
      Event    : Xi.Frame_Event.Xi_Frame_Event)
   is
      pragma Unreferenced (Listener);
      pragma Unreferenced (Event);
   begin
      if Xi.Mouse.Current_Mouse.Wheel_Up
        or else Xi.Keyboard.Key_Down (Xi.Keyboard.Character_Key ('w'))
      then
         Main_Model.On_Wheel_Up;
      elsif Xi.Mouse.Current_Mouse.Wheel_Down
        or else Xi.Keyboard.Key_Down (Xi.Keyboard.Character_Key ('s'))
      then
         Main_Model.On_Wheel_Down;
      end if;
      if Xi.Keyboard.Key_Down (Xi.Keyboard.Key_Esc) then
         Xi.Main.Leave_Main_Loop;
      end if;

   end Frame_Started;

   ------------------
   -- Galaxy_Model --
   ------------------

   function Galaxy_Model
      return Xi_Model
   is
      Scene    : constant Xi.Scene.Xi_Scene := Xi.Scene.Create_Scene;
      Camera   : constant Xi.Camera.Xi_Camera := Scene.Active_Camera;
      Node     : constant Xi.Node.Xi_Node := Scene.Create_Node ("galaxy");
      Entity   : Xi.Entity.Manual.Xi_Manual_Entity;
   begin
      Xi.Entity.Manual.Xi_New (Entity);
      Entity.Begin_Operation (Xi.Render_Operation.Point_List);

      for I in 1 .. Concorde.Galaxy.System_Count loop
         declare
            System : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Galaxy.Get_System (I);
         begin
            Entity.Vertex
              (Xi_Float (System.X),
               Xi_Float (System.Y),
               Xi_Float (System.Z));
         end;
      end loop;
      Entity.End_Operation;

      Camera.Set_Position (0.0, 0.0, 1.0);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Look_At (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
      Camera.Frustum (-1.0, 1.0, -1.0, 1.0, 0.1, 50.0);
      Node.Set_Entity (Entity);

      declare
         Listener : constant Xi.Frame_Event.Xi_Frame_Listener :=
                      new Galaxy_Frame_Listener;
      begin
         Xi.Main.Add_Frame_Listener (Listener);
      end;

      Xtk.Label.Xtk_New (Main_Model.Mouse_Status, "");
      Main_Model.Mouse_Status.Set_Rectangle ((410.0, 0.0, 180.0, 40.0));

      declare
         Fixed : Xtk.Fixed.Xtk_Fixed;
      begin
         Xtk.Fixed.Xi_New (Fixed);
         Fixed.Set_Rectangle ((0.0, 0.0, 600.0, 100.0));
         Fixed.Add (Main_Model.Mouse_Status);

         Xtk.Panel.Xtk_New
           (Panel  => Main_Model.Top_Panel,
            Region => (20.0, 50.0, 600.0, 100.0),
            Top    => Fixed);
      end;

      Main_Model.Scene := Scene;
      return Main_Model'Access;
   end Galaxy_Model;

end Concorde.Xi_UI.Galaxies;
