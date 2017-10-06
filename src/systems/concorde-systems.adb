with Ada.Text_IO;

with Concorde.Constants;
with Concorde.Solar_System;

with Concorde.Elementary_Functions;

with Concorde.Worlds;
with Concorde.Stars;

with Concorde.Factions;

with Concorde.Worlds.Lists;
with Concorde.Worlds.Create;

package body Concorde.Systems is

   ----------------
   -- Add_Object --
   ----------------

   procedure Add_Object
     (System   : in out Root_Star_System_Type'Class;
      Object   : not null access Star_System_Object_Interface'Class)
   is
   begin
      if System.Objects.Is_Empty then
         System.Main_Object :=
           Main_Star_System_Object_Interface'Class (Object.all)'Access;
      end if;
      System.Objects.Append
        ((Object => Object));
   end Add_Object;

   --------------
   -- Add_Ship --
   --------------

   procedure Add_Ship
     (System : in out Root_Star_System_Type'Class;
      Ship   : Concorde.Ships.Ship_Type)
   is
   begin
      System.Ships.Append (Ship);
   end Add_Ship;

   -----------------
   -- Add_Traffic --
   -----------------

   procedure Add_Traffic
     (From  : in out Root_Star_System_Type'Class;
      To    : not null access constant Root_Star_System_Type'Class;
      Count : Positive := 1)
   is
      To_System : constant Star_System_Type := Star_System_Type (To);
   begin
      for Position in From.Edges.Iterate loop
         declare
            Edge : Edge_Info := Edge_Info_Lists.Element (Position);
         begin
            if Edge.To = To_System then
               Edge.Traffic := Edge.Traffic + Count;
               From.Edges.Replace_Element (Position, Edge);
               return;
            end if;
         end;
      end loop;

      From.Edges.Append ((To_System, Count));

   end Add_Traffic;

   --------------
   -- Arriving --
   --------------

   procedure Arriving
     (System : in out Root_Star_System_Type'Class;
      Ship   : Concorde.Ships.Ship_Type;
      Time   : Concorde.Calendar.Time)
   is
      Event : Ship_Event;
   begin
      Event.Set_Time_Stamp (Time);
      Event.Ship := Ship;
      System.Signal
        (Signal_Ship_Arrived, Event);
   end Arriving;

   ------------
   -- Battle --
   ------------

   procedure Battle
     (System : in out Root_Star_System_Type'Class;
      Size   : Positive)
   is
   begin
      System.Last_Battle := Concorde.Calendar.Clock;
      System.Battle_Size := Size;
   end Battle;

   -------------
   -- Capital --
   -------------

   function Capital (System : Root_Star_System_Type'Class)
                     return Boolean
   is
   begin
      return System.Capital;
   end Capital;

   -------------------------
   -- Clear_Ship_Movement --
   -------------------------

   procedure Clear_Ship_Movement
     (System : in out Root_Star_System_Type'Class)
   is
   begin
      System.Edges.Clear;
   end Clear_Ship_Movement;

   -----------------------
   -- Create_Ship_Event --
   -----------------------

   function Create_Ship_Event
     (Time_Stamp : Concorde.Calendar.Time;
      Ship       : Concorde.Ships.Ship_Type)
      return Concorde.Events.Root_Event_Type'Class
   is
   begin
      return Event : Ship_Event do
         Event.Set_Time_Stamp (Time_Stamp);
         Event.Ship := Ship;
      end return;
   end Create_Ship_Event;

   ---------------
   -- Departing --
   ---------------

   procedure Departing
     (System : in out Root_Star_System_Type'Class;
      Ship   : Concorde.Ships.Ship_Type;
      Time   : Concorde.Calendar.Time)
   is
      Event : Ship_Event;
   begin
      Event.Set_Time_Stamp (Time);
      Event.Ship := Ship;
      System.Signal
        (Signal_Ship_Departed, Event);
   end Departing;

   --------------
   -- Distance --
   --------------

   function Distance
     (System_1, System_2 : Star_System_Type)
      return Non_Negative_Real
   is
      function Sqrt (X : Non_Negative_Real) return Non_Negative_Real
                     renames Concorde.Elementary_Functions.Sqrt;
   begin
      return Sqrt ((System_1.X - System_2.X) ** 2
                   + (System_1.Y - System_2.Y) ** 2
                   + (System_1.Z - System_2.Z) ** 2);
   end Distance;

   ---------------
   -- Get_Ships --
   ---------------

   procedure Get_Ships
     (System    : Root_Star_System_Type'Class;
      Result    : in out Concorde.Ships.Lists.List)
   is
   begin
      for Ship of System.Ships loop
         if Ship.Alive then
            Result.Append (Ship);
         end if;
      end loop;
   end Get_Ships;

   -----------
   -- Index --
   -----------

   function Index (System : Root_Star_System_Type'Class) return Positive is
   begin
      return System.Index;
   end Index;

   ------------------------
   -- Influence_Boundary --
   ------------------------

   function Influence_Boundary
     (System : Root_Star_System_Type'Class)
      return System_Influence_Boundary
   is
   begin
      return System.Boundary.all;
   end Influence_Boundary;

   ------------------------
   -- Jump_Arrival_Point --
   ------------------------

   function Jump_Arrival_Point
     (Arrival_System   : Root_Star_System_Type'Class;
      Departure_System : not null access Root_Star_System_Type'Class)
      return Newton.Vector_3
   is
      use Newton.Matrices;
      Start  : constant Newton.Vector_3 :=
                 (Departure_System.X, Departure_System.Y, Departure_System.Z);
      Finish : constant Newton.Vector_3 :=
                 (Arrival_System.X, Arrival_System.Y, Arrival_System.Z);

      Vector : constant Newton.Vector_3 := abs (Start - Finish);
      Distance : constant Non_Negative_Real :=
                   Arrival_System.Main_Object.Mass
                     / Concorde.Solar_System.Solar_Mass
                   * 5.0 * Concorde.Solar_System.Earth_Orbit;
   begin
      return Vector * Distance;
   end Jump_Arrival_Point;

   --------------------------
   -- Jump_Departure_Point --
   --------------------------

   function Jump_Departure_Point
     (From : Root_Star_System_Type'Class;
      To   : not null access Root_Star_System_Type'Class)
      return Newton.Vector_3
   is
      use Newton.Matrices;
      Start  : constant Newton.Vector_3 :=
                 (From.X, From.Y, From.Z);
      Finish : constant Newton.Vector_3 :=
                 (To.X, To.Y, To.Z);

      Vector : constant Newton.Vector_3 := abs (Finish - Start);
      Distance : constant Non_Negative_Real :=
                   From.Main_Object.Mass
                     / Concorde.Solar_System.Solar_Mass
                   * 10.0 * Concorde.Solar_System.Earth_Orbit;
   begin
      return Vector * Distance;
   end Jump_Departure_Point;

   -----------------
   -- Last_Battle --
   -----------------

   function Last_Battle
     (System : Root_Star_System_Type'Class)
      return Concorde.Calendar.Time
   is
   begin
      return System.Last_Battle;
   end Last_Battle;

   ----------------------
   -- Last_Battle_Size --
   ----------------------

   function Last_Battle_Size
     (System : Root_Star_System_Type'Class)
      return Natural
   is
   begin
      return System.Battle_Size;
   end Last_Battle_Size;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Star_System : in out Root_Star_System_Type)
   is
      List   : Concorde.Worlds.Lists.List;
   begin
      Concorde.Worlds.Create.Create_Worlds
        (Db.Reference (Star_System),
         Concorde.Stars.Star_Type (Star_System.Main_Object),
         List);

      for World of List loop
         Star_System.Add_Object
           (World);
      end loop;

      Ada.Text_IO.Put_Line
        (Star_System.Name & ":"
         & Ada.Containers.Count_Type'Image
           (List.Length) & " planets");

   end Load;

   -------------
   -- Loyalty --
   -------------

   function Loyalty
     (System : Root_Star_System_Type'Class)
      return Unit_Real
   is
   begin
      return System.Loyalty;
   end Loyalty;

   -----------------
   -- Main_Object --
   -----------------

   function Main_Object
     (System : Root_Star_System_Type'Class)
      return access Main_Star_System_Object_Interface'Class
   is
   begin
      return System.Main_Object;
   end Main_Object;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Star_System : Root_Star_System_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Star_System);
   begin
      return Db.Get_Database;
   end Object_Database;

   --------------
   -- Owned_By --
   --------------

   function Owned_By
     (System : Root_Star_System_Type'Class;
      Faction : Concorde.Factions.Root_Faction_Type'Class)
      return Boolean
   is
      use type Memor.Database_Reference;
   begin
      return System.Owned
        and then System.Owner.Reference = Faction.Reference;
   end Owned_By;

   -----------
   -- Owner --
   -----------

   function Owner
     (System : Root_Star_System_Type'Class)
      return access constant Concorde.Factions.Root_Faction_Type'Class
   is
   begin
      return System.Owner;
   end Owner;

   ------------
   -- Period --
   ------------

   function Period (Object : Star_System_Object_Interface'Class)
                    return Non_Negative_Real
   is
      use Concorde.Constants;
      use Concorde.Elementary_Functions;
   begin
      return 2.0 * Pi * Sqrt (Object.Semimajor_Axis ** 3
                              / (Gravitational_Constant
                                * Object.Primary.Mass));
   end Period;

   -------------------------------
   -- Primary_Relative_Position --
   -------------------------------

--     function Primary_Relative_Position
--       (Object : Star_System_Object_Interface'Class)
--        return Newton.Vector_3
--     is
--        use Newton.Matrices;
--        use Concorde.Geometry;
--        R     : constant Non_Negative_Real := Object.Semimajor_Axis;
--        Theta : constant Radians := Object.Orbit_Progress;
--     begin
--        return (R * Cos (Theta), 0.0, R * Sin (Theta));
--     end Primary_Relative_Position;

   -----------------------
   -- Remove_Dead_Ships --
   -----------------------

   procedure Remove_Dead_Ships
     (System : in out Root_Star_System_Type'Class)
   is
      Found    : Boolean := False;
      New_List : Concorde.Ships.Lists.List;
   begin
      for Ship of System.Ships loop
         if Ship.Alive then
            New_List.Append (Ship);
         else
            Found := True;
         end if;
      end loop;
      if Found then
         System.Ships := New_List;
      end if;
   end Remove_Dead_Ships;

   -----------------
   -- Remove_Ship --
   -----------------

   procedure Remove_Ship
     (System : in out Root_Star_System_Type'Class;
      Ship   : Concorde.Ships.Ship_Type)
   is
      use Concorde.Ships.Lists;
      Position : Cursor := System.Ships.Find (Ship);
   begin
      pragma Assert (Has_Element (Position),
                     "could not find ship " & Ship.Name
                     & " at " & System.Name);
      System.Ships.Delete (Position);
   end Remove_Ship;

   -------------------------
   -- Scan_System_Objects --
   -------------------------

   procedure Scan_System_Objects
     (System  : Root_Star_System_Type'Class;
      Process : not null access
        procedure (System_Object : Star_System_Object_Interface'Class))
   is
   begin
      for System_Object of System.Objects loop
         Process (System_Object.Object.all);
      end loop;
   end Scan_System_Objects;

   -------------------------
   -- Scan_System_Objects --
   -------------------------

   procedure Scan_System_Objects
     (System  : Root_Star_System_Type'Class;
      Process : not null access
        procedure (System_Object : not null access constant
                     Star_System_Object_Interface'Class))
   is
   begin
      for System_Object of System.Objects loop
         Process (System_Object.Object);
      end loop;
   end Scan_System_Objects;

   ------------------
   -- Scan_Systems --
   ------------------

   procedure Scan_Systems
     (Test    : not null access
        function (System : Star_System_Type) return Boolean;
      Process : not null access
        procedure (System : Star_System_Type))
   is
   begin
      Db.Scan (Test, Process);
   end Scan_Systems;

   ------------------
   -- Scan_Systems --
   ------------------

   procedure Scan_Systems
     (Process : not null access
        procedure (System : Star_System_Type))
   is
   begin
      Db.Scan (Process);
   end Scan_Systems;

   -----------------
   -- Set_Capital --
   -----------------

   procedure Set_Capital
     (System     : in out Root_Star_System_Type'Class;
      Is_Capital : Boolean)
   is
   begin
      System.Capital := Is_Capital;
      System.Loyalty := 1.0;
      System.Original_Owner := null;
   end Set_Capital;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (System : in out Root_Star_System_Type'Class;
      New_Owner : not null access constant
        Concorde.Factions.Root_Faction_Type'Class)
   is
   begin
      if System.Owner /= null then
         if System.Original_Owner = null then
            System.Original_Owner := System.Owner;
            System.Loyalty := 0.1;
         elsif New_Owner = System.Original_Owner then
            System.Loyalty := 1.0;
            System.Original_Owner := null;
         end if;
      else
         System.Original_Owner := null;
         System.Loyalty := 1.0;
      end if;

      System.Owner := New_Owner;
   end Set_Owner;

   -----------
   -- Ships --
   -----------

   function Ships
     (System : Root_Star_System_Type'Class)
      return Natural
   is
   begin
      return Natural (System.Ships.Length);
   end Ships;

   -------------
   -- Traffic --
   -------------

   function Traffic
     (From : Root_Star_System_Type'Class;
      To   : not null access constant Root_Star_System_Type'Class)
      return Natural
   is
      To_System : constant Star_System_Type := Star_System_Type (To);
   begin
      for Edge of From.Edges loop
         if Edge.To = To_System then
            return Edge.Traffic;
         end if;
      end loop;
      return 0;
   end Traffic;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Star_System_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

   --------------------
   -- Update_Systems --
   --------------------

   procedure Update_Systems
     (Test    : not null access
        function (System : Root_Star_System_Type'Class) return Boolean;
      Update  : not null access
        procedure (System : in out Root_Star_System_Type'Class))
   is
   begin
      Db.Iterate (Test, Update);
   end Update_Systems;

   --------------------
   -- Update_Systems --
   --------------------

   procedure Update_Systems
     (Update : not null access
        procedure (System : in out Root_Star_System_Type'Class))
   is
   begin
      Db.Iterate (Update);
   end Update_Systems;

   -------
   -- X --
   -------

   function X (System : Root_Star_System_Type'Class) return Real is
   begin
      return System.X;
   end X;

   -------
   -- Y --
   -------

   function Y (System : Root_Star_System_Type'Class) return Real is
   begin
      return System.Y;
   end Y;

   -------
   -- Z --
   -------

   function Z (System : Root_Star_System_Type'Class) return Real is
   begin
      return System.Z;
   end Z;

end Concorde.Systems;
