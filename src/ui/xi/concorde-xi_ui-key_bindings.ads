package Concorde.Xi_UI.Key_Bindings is

   type Array_Of_Key_Bindings is
     array (Positive range <>) of User_Command;

   function Current_Active_Bindings
     return Array_Of_Key_Bindings;

   procedure Load_Key_Bindings;

end Concorde.Xi_UI.Key_Bindings;
