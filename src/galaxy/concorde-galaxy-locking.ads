package Concorde.Galaxy.Locking is

   procedure Init_Locking;

   procedure Lock_System
     (System    : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Exclusive : Boolean);

   procedure Unlock_System
     (System    : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class);

end Concorde.Galaxy.Locking;
