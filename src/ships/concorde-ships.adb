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
   -- Count_Ships --
   -----------------

   function Count_Ships
     (Test : not null access function
        (Ship : Ship_Type)
      return Boolean)
      return Natural
   is
      Result : Natural := 0;
   begin
      for Ship of Ship_Vector loop
         if Test (Ship) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count_Ships;

   ------------
   -- Damage --
   ------------

   function Damage
     (Ship : Root_Ship_Type'Class)
      return Unit_Real
   is
   begin
      return Non_Negative_Real (Ship.Max_HP - Ship.HP)
        / Non_Negative_Real (Ship.Max_HP);
   end Damage;

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
