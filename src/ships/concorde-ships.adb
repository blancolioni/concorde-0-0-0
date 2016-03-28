with WL.Random;

with Concorde.Components;
with Concorde.Systems;

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
      Total : Non_Negative_Real := 0.0;
   begin
      for Module of Ship.Structure loop
         Total := Total + Module.Damage;
      end loop;
      return Total / Non_Negative_Real (Ship.Structure.Length);
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

   ------------------------
   -- Get_Weapon_Modules --
   ------------------------

   function Get_Weapon_Modules
     (Ship : Root_Ship_Type'Class)
      return Concorde.Modules.Array_Of_Modules
   is
      use Concorde.Components;
      use Concorde.Modules;
      Result : Array_Of_Modules (1 .. Ship.Structure.Last_Index);
      Count  : Natural := 0;
   begin
      for Module of Ship.Structure loop
         if Module.Component.Class = Weapon then
            Count := Count + 1;
            Result (Count) := Module;
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Weapon_Modules;

   ---------
   -- Hit --
   ---------

   procedure Hit
     (Ship : in out Root_Ship_Type'Class;
      Damage : Natural)
   is
   begin
      for I in 1 .. Damage loop
         declare
            Index : constant Positive :=
                      WL.Random.Random_Number (1, Ship.Structure.Last_Index);
         begin
            Ship.Structure.Element (Index).Hit;
         end;
      end loop;
      Ship.Alive := Ship.Damage < 1.0;
   end Hit;

   ---------------
   -- Long_Name --
   ---------------

   function Long_Name (Ship : Root_Ship_Type'Class) return String is
   begin
      return Ship.Identity & " " & Ship.Name & " [" & Ship.System.Name & "]";
   end Long_Name;

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

   -----------------------
   -- Short_Description --
   -----------------------

   function Short_Description (Ship : Root_Ship_Type'Class) return String is
   begin
      return Ship.Long_Name & " dmg"
        & Natural'Image (Natural (Ship.Damage * 100.0)) & "%";
   end Short_Description;

   ----------
   -- Size --
   ----------

   function Size
     (Ship : Root_Ship_Type'Class)
      return Natural
   is
   begin
      return Ship.Size;
   end Size;

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

   ------------------
   -- Update_Power --
   ------------------

   procedure Update_Power
     (Ship : in out Root_Ship_Type'Class)
   is
   begin
      for Module of Ship.Structure loop
         Module.Draw_Power (Module.Component.Max_Power_Draw);
      end loop;
   end Update_Power;

end Concorde.Ships;
