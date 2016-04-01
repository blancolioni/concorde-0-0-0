with WL.Random;

with Concorde.Components;
with Concorde.Systems;

with Concorde.Random;

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
      use Concorde.Components;
      Total : Non_Negative_Real := 0.0;
      Count : Non_Negative_Real := 0.0;
   begin
      for Module of Ship.Structure loop
         if Module.Module.Component.Class = Strut then
            null;
         else
            Total := Total + Module.Module.Damage;
            Count := Count + 1.0;
         end if;
      end loop;
      if Count > 0.0 then
         return Total / Count;
      else
         return 1.0;
      end if;
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
   -- Get_Damaged_Mounts --
   ------------------------

   function Get_Damaged_Mounts
     (Ship : Root_Ship_Type'Class)
      return Array_Of_Mounted_Modules
   is
      use Concorde.Components;
      Result : Array_Of_Mounted_Modules
        (1 .. Natural (Ship.Structure.Last_Index));
      Count  : Natural := 0;
   begin
      for I in 1 .. Ship.Structure.Last_Index loop
         if Ship.Structure (I).Module.Component.Class in Weapon_Class then
            Count := Count + 1;
            Result (Count) := Mounted_Module (I);
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Damaged_Mounts;

   --------------------------
   -- Get_Effective_Mounts --
   --------------------------

   function Get_Effective_Mounts
     (Ship : Root_Ship_Type'Class)
      return Array_Of_Mounted_Modules
   is
      use Concorde.Components;
      Result : Array_Of_Mounted_Modules
        (1 .. Natural (Ship.Structure.Last_Index));
      Count  : Natural := 0;
   begin
      for I in 1 .. Ship.Structure.Last_Index loop
         if Ship.Structure (I).Module.Damage < 1.0 then
            Count := Count + 1;
            Result (Count) := Mounted_Module (I);
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Effective_Mounts;

   ----------------
   -- Get_Module --
   ----------------

   function Get_Module
     (Ship  : Root_Ship_Type'Class;
      Mount : Mounted_Module)
      return Concorde.Modules.Module_Type
   is
   begin
      return Ship.Structure (Positive (Mount)).Module;
   end Get_Module;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
     (Ship  : Root_Ship_Type'Class;
      Mount : Mounted_Module)
      return Module_Orientation
   is
   begin
      return Ship.Structure (Positive (Mount)).Orientation;
   end Get_Orientation;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
     (Ship  : Root_Ship_Type'Class;
      Mount : Mounted_Module)
      return Module_Position
   is
   begin
      return Ship.Structure (Positive (Mount)).Left_Low_Aft;
   end Get_Position;

   -----------------------
   -- Get_Weapon_Mounts --
   -----------------------

   function Get_Weapon_Mounts
     (Ship : Root_Ship_Type'Class)
      return Array_Of_Mounted_Modules
   is
      use Concorde.Components;
      Result : Array_Of_Mounted_Modules
        (1 .. Natural (Ship.Structure.Last_Index));
      Count  : Natural := 0;
   begin
      for I in 1 .. Ship.Structure.Last_Index loop
         if Ship.Structure (I).Module.Component.Class in Weapon_Class then
            Count := Count + 1;
            Result (Count) := Mounted_Module (I);
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Weapon_Mounts;

   --------------------------
   -- Has_Effective_Weapon --
   --------------------------

   function Has_Effective_Weapon
     (Ship : Root_Ship_Type'Class)
      return Boolean
   is
   begin
      for Mount of Ship.Structure loop
         if Mount.Module.Effectiveness > 0.0 then
            return True;
         end if;
      end loop;
      return False;
   end Has_Effective_Weapon;

   ---------
   -- Hit --
   ---------

   procedure Hit
     (Ship : in out Root_Ship_Type'Class;
      Damage : Natural)
   is
   begin
      for I in 1 .. Damage loop
         exit when not Ship.Alive;
         declare
            Mounts : constant Array_Of_Mounted_Modules :=
                       Ship.Get_Effective_Mounts;
            Index : constant Positive :=
                       WL.Random.Random_Number
                         (Mounts'First, Mounts'Last);
            Module : constant Concorde.Modules.Module_Type :=
                       Ship.Structure
                         (Positive (Mounts (Index))).Module;
         begin
            Module.Hit;
            declare
               Chance : constant Unit_Real :=
                          Module.Explosion_Chance;
            begin
--                 Ada.Text_IO.Put_Line
--                   ("chance of explosion:"
--                    & Natural'Image (Natural (Chance * 100.0))
--                    & "%");
               if Concorde.Random.Unit_Random < Chance then
                  Module.Start_Explosion;
               end if;
            end;
         end;

         declare
            Ship_Damage : constant Unit_Real := Ship.Damage;
         begin
            Ship.Alive := Ship_Damage < 0.95;
         end;
      end loop;

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
      return Size_Type
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

   -------------------
   -- Update_Damage --
   -------------------

   procedure Update_Damage
     (Ship : in out Root_Ship_Type'Class)
   is
      New_Hits : Natural := 0;
   begin
      for Mount of Ship.Structure loop
         declare
            Module : constant Concorde.Modules.Module_Type :=
                       Mount.Module;
         begin
            Module.Update_Damage;
            if Module.Exploding then
               if Module.Explosion_Timer = 0 then
                  New_Hits := New_Hits + Module.Explosion_Size;
               end if;
            end if;
         end;
      end loop;
      if New_Hits > 0 then
         Ship.Hit (New_Hits);
      end if;
   end Update_Damage;

   ------------------
   -- Update_Power --
   ------------------

   procedure Update_Power
     (Ship : in out Root_Ship_Type'Class)
   is
   begin
      for Module of Ship.Structure loop
         Module.Module.Draw_Power
           (Module.Module.Maximum_Power_Draw);
      end loop;
   end Update_Power;

end Concorde.Ships;
