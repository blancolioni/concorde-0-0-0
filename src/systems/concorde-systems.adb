with Concorde.Elementary_Functions;

package body Concorde.Systems is

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
      Ship   : Concorde.Ships.Ship_Type)
   is
   begin
      System.Arriving.Append (Ship);
   end Arriving;

   ------------
   -- Attack --
   ------------

   procedure Battle
     (System     : in out Root_Star_System_Type'Class;
      Ship_Count : Positive)
   is
   begin
      System.Last_Battle := Concorde.Dates.Current_Date;
      System.Battle_Size := Ship_Count;
   end Battle;

   --------------
   -- Capacity --
   --------------

   function Capacity (System : Root_Star_System_Type'Class)
                      return Non_Negative_Real
   is
   begin
      return System.Capacity;
   end Capacity;

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
      System.Arriving.Clear;
      System.Departing.Clear;
      System.Edges.Clear;
   end Clear_Ship_Movement;

   --------------------------
   -- Commit_Ship_Movement --
   --------------------------

   procedure Commit_Ship_Movement
     (System : not null access Root_Star_System_Type'Class)
   is
   begin
      for Ship of System.Departing loop
         System.Remove_Ship (Ship);
      end loop;
      for Ship of System.Arriving loop
         System.Add_Ship (Ship);
         Ship.Set_System (System);
      end loop;
   end Commit_Ship_Movement;

   ---------------
   -- Departing --
   ---------------

   procedure Departing
     (System : in out Root_Star_System_Type'Class;
      Ship   : Concorde.Ships.Ship_Type)
   is
   begin
      System.Departing.Append (Ship);
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
                   + (System_1.Y - System_2.Y) ** 2);
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

   -----------------
   -- Last_Battle --
   -----------------

   function Last_Battle
     (System : Root_Star_System_Type'Class)
      return Concorde.Dates.Date_Type
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

   -----------
   -- Owner --
   -----------

   function Owner
     (System : Root_Star_System_Type'Class)
      return access Concorde.Empires.Root_Empire_Type'Class
   is
   begin
      return System.Owner;
   end Owner;

   ----------------
   -- Production --
   ----------------

   function Production (System : Root_Star_System_Type'Class)
                        return Non_Negative_Real
   is
   begin
      return System.Production;
   end Production;

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

   ------------------
   -- Set_Capacity --
   ------------------

   procedure Set_Capacity
     (System : in out Root_Star_System_Type'Class;
      New_Capacity : Non_Negative_Real)
   is
   begin
      System.Capacity := New_Capacity;
   end Set_Capacity;

   -----------------
   -- Set_Capital --
   -----------------

   procedure Set_Capital
     (System : in out Root_Star_System_Type'Class)
   is
   begin
      System.Capital := True;
   end Set_Capital;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (System : in out Root_Star_System_Type'Class;
      New_Owner : not null access
        Concorde.Empires.Root_Empire_Type'Class)
   is
   begin
      System.Owner := New_Owner;
   end Set_Owner;

   --------------------
   -- Set_Production --
   --------------------

   procedure Set_Production
     (System : in out Root_Star_System_Type'Class;
      New_Production : Non_Negative_Real)
   is
   begin
      System.Production := New_Production;
   end Set_Production;

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

end Concorde.Systems;
