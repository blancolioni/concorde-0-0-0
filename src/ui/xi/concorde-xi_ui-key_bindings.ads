package Concorde.Xi_UI.Key_Bindings is

   type Key_Binding is
     (No_Binding,
      Exit_Model,
      Move_Forward, Move_Backward,
      Move_Left, Move_Right,
      Move_Up, Move_Down);

   type Array_Of_Key_Bindings is array (Positive range <>) of Key_Binding;

   function Current_Active_Bindings
     return Array_Of_Key_Bindings;

   procedure Load_Key_Bindings;

end Concorde.Xi_UI.Key_Bindings;
