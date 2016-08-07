with Ada.Characters.Latin_1;

with Xi.Keyboard;

package body Concorde.Xi_UI.Key_Bindings is

   subtype Available_Key is
     Character range Ada.Characters.Latin_1.ESC .. '~';

   type Key_Binding_Array is array (Available_Key) of Key_Binding;

   Current_Bindings : Key_Binding_Array :=
       (others => No_Binding);

   --  Shift, Control, Alt, Meta : Key_Binding := No_Binding;

   -----------------------------
   -- Current_Active_Bindings --
   -----------------------------

   function Current_Active_Bindings
      return Array_Of_Key_Bindings
   is
      Result : Array_Of_Key_Bindings (1 .. 255);
      Count  : Natural := 0;
   begin
      for Ch in Current_Bindings'Range loop
         if Xi.Keyboard.Key_Down (Xi.Keyboard.Character_Key (Ch)) then
            declare
               Binding : constant Key_Binding :=
                           Current_Bindings (Ch);
            begin
               if Binding /= No_Binding then
                  Count := Count + 1;
                  Result (Count) := Binding;
               end if;
            end;
         end if;
      end loop;

      return Result (1 .. Count);

   end Current_Active_Bindings;

   -----------------------
   -- Load_Key_Bindings --
   -----------------------

   procedure Load_Key_Bindings is
   begin
      Current_Bindings :=
        ('w' => Move_Forward, 's' => Move_Backward,
         'a' => Move_Left, 'd' => Move_Right,
         'e' => Move_Up, 'c' => Move_Down,
         Ada.Characters.Latin_1.ESC => Exit_Model,
         others => No_Binding);

   end Load_Key_Bindings;

end Concorde.Xi_UI.Key_Bindings;
