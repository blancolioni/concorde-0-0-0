with Concorde.Locking;

package body Concorde.Galaxy.Locking is

   type Array_Of_Locks is array (Positive range <>) of Concorde.Locking.Lock;

   System_Locks : access Array_Of_Locks;

   ------------------
   -- Init_Locking --
   ------------------

   procedure Init_Locking is
   begin
      System_Locks := new Array_Of_Locks (1 .. Galaxy_Vector.Last_Index);
   end Init_Locking;

   -----------------
   -- Lock_System --
   -----------------

   procedure Lock_System
     (System    : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Exclusive : Boolean)
   is
   begin
      if Exclusive then
         System_Locks (System.Index).Exclusive;
      else
         System_Locks (System.Index).Shared;
      end if;
   end Lock_System;

   -------------------
   -- Unlock_System --
   -------------------

   procedure Unlock_System
     (System    : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
   is
   begin
      System_Locks (System.Index).Unlock;
   end Unlock_System;

end Concorde.Galaxy.Locking;
