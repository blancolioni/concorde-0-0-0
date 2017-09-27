package Concorde.Locking is

   protected type Lock is
      entry Shared;
      entry Exclusive;
      procedure Unlock;
   private
      Shared_Lock_Count : Natural := 0;
      Exclusive_Locked  : Boolean := False;
   end Lock;

end Concorde.Locking;
