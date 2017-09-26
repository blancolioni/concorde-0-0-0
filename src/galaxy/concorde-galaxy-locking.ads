package Concorde.Galaxy.Locking is

   procedure Init_Locking;

   procedure Lock_System
     (System    : Concorde.Systems.Star_System_Type;
      Exclusive : Boolean);

   procedure Unlock_System
     (System    : Concorde.Systems.Star_System_Type);

end Concorde.Galaxy.Locking;
