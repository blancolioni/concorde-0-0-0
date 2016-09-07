package body Concorde.Scripts is

   --------------
   -- Complete --
   --------------

   function Complete
     (Script : Concorde_Script)
      return Boolean
   is
   begin
      if Script = Null_Script then
         return True;
      else
         return Script.Get.Complete;
      end if;
   end Complete;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Script : Concorde_Script)
   is
   begin
      if Script = Null_Script then
         null;
      else
         Script.Set.Execute;
      end if;
   end Execute;

end Concorde.Scripts;
