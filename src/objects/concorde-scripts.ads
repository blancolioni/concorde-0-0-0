with WL.Handles;

private with Concorde.Calendar;

package Concorde.Scripts is

   type Concorde_Script is private;

   Null_Script : constant Concorde_Script;

   procedure Execute
     (Script : Concorde_Script);

   function Complete
     (Script : Concorde_Script)
      return Boolean;

private

   type Root_Script_Type is abstract tagged
      record
         Last_Execution : Concorde.Calendar.Time;
      end record;

   procedure Execute
     (Script : in out Root_Script_Type)
   is abstract;

   function Complete
     (Script : Root_Script_Type)
      return Boolean
      is abstract;

   package Script_Handles is
     new WL.Handles (Root_Script_Type'Class);

   type Concorde_Script is new Script_Handles.Handle_Type with null record;

   Null_Script : constant Concorde_Script :=
                   (Script_Handles.Null_Handle with null record);

end Concorde.Scripts;
