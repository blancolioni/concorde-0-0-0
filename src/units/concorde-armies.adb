with Concorde.Surfaces;
with Concorde.Worlds;

package body Concorde.Armies is

   ------------------
   -- Add_Regiment --
   ------------------

   procedure Add_Regiment
     (Army     : in out Root_Army_Type'Class;
      Regiment : Concorde.Regiments.Regiment_Type)
   is
   begin
      Army.Regiments.Append (Regiment);
   end Add_Regiment;

   ---------------------
   -- Clear_Regiments --
   ---------------------

   procedure Clear_Regiments
     (Army : in out Root_Army_Type'Class)
   is
   begin
      Army.Regiments.Clear;
   end Clear_Regiments;

   ---------------
   -- Regiments --
   ---------------

   function Regiments
     (Army : Root_Army_Type'Class)
      return Array_Of_Regiments
   is
   begin
      return Rs : Array_Of_Regiments (1 .. Army.Regiments.Last_Index) do
         for I in Rs'Range loop
            Rs (I) := Army.Regiments.Element (I);
         end loop;
      end return;
   end Regiments;

   ---------------------
   -- Remove_Regiment --
   ---------------------

   procedure Remove_Regiment
     (Army     : in out Root_Army_Type'Class;
      Regiment : Concorde.Regiments.Regiment_Type)
   is
      Position : Regiment_Vectors.Cursor :=
                   Army.Regiments.Find (Regiment);
   begin
      if Regiment_Vectors.Has_Element (Position) then
         Army.Regiments.Delete (Position);
      else
         raise Constraint_Error with
           "no such regiment " & Regiment.Name & " in army " & Army.Name;
      end if;
   end Remove_Regiment;

   -----------------
   -- Set_Manager --
   -----------------

   overriding procedure Set_Manager
     (Army        : in out Root_Army_Type;
      Manager     : Concorde.Managers.Manager_Type)
   is
   begin
      Army.Manager := Manager;
   end Set_Manager;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Army       : in out Root_Army_Type;
      New_Name   : String)
   is
   begin
      Army.Army_Name := Ada.Strings.Unbounded.To_Unbounded_String (New_Name);
   end Set_Name;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Army_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

   ---------------------
   -- Update_Location --
   ---------------------

   procedure Update_Location
     (Army : Army_Type)
   is
   begin
      if Army.Is_World_Location then
         Army.Current_World.Update.Add_Army
           (Sector     =>
              Concorde.Surfaces.Surface_Tile_Index
                (Concorde.Locations.World_Sector
                     (Army.Current_Location)),
            Army => Army);
      end if;
   end Update_Location;

end Concorde.Armies;
