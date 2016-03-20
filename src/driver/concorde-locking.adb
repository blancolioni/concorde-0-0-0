package body Concorde.Locking is

   ----------
   -- Lock --
   ----------

   protected body Lock is

      ------------
      -- Shared --
      ------------

      entry Shared when not Exclusive_Locked is
      begin
         Shared_Lock_Count := Shared_Lock_Count + 1;
      end Shared;

      ---------------
      -- Exclusive --
      ---------------

      entry Exclusive when
        Shared_Lock_Count = 0
        and then not Exclusive_Locked
      is
      begin
         Exclusive_Locked := True;
      end Exclusive;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         if Exclusive_Locked then
            Exclusive_Locked := False;
         elsif Shared_Lock_Count > 0 then
            Shared_Lock_Count := Shared_Lock_Count - 1;
         else
            raise Constraint_Error with
              "unlocked called on unlocked focus";
         end if;
      end Unlock;

   end Lock;

end Concorde.Locking;
