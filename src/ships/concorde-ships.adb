package body Concorde.Ships is

   -----------
   -- Alive --
   -----------

   function Alive
     (Ship : Root_Ship_Type'Class)
      return Boolean
   is
   begin
      return Ship.Alive;
   end Alive;

   -----------------
   -- Destination --
   -----------------

   function Destination
     (Ship : Root_Ship_Type'Class)
      return access constant Concorde.Systems.Root_Star_System_Type'Class
   is
   begin
      return Ship.Destination;
   end Destination;

   -----------
   -- Owner --
   -----------

   function Owner
     (Ship : Root_Ship_Type'Class)
      return access Concorde.Empires.Root_Empire_Type'Class
   is
   begin
      return Ship.Owner;
   end Owner;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship : in out Root_Ship_Type'Class;
      System : access constant Concorde.Systems.Root_Star_System_Type'Class)
   is
   begin
      Ship.Destination := System;
   end Set_Destination;

   ----------------
   -- Set_System --
   ----------------

   procedure Set_System
     (Ship : in out Root_Ship_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
   is
   begin
      Ship.System := System;
      if System = Ship.Destination then
         Ship.Destination := null;
      end if;
   end Set_System;

   ------------
   -- System --
   ------------

   function System
     (Ship : Root_Ship_Type'Class)
      return access constant Concorde.Systems.Root_Star_System_Type'Class
   is
   begin
      return Ship.System;
   end System;

end Concorde.Ships;
