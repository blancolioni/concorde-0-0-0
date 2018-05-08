with Concorde.Systems;

package body Concorde.Ships is

   -----------------------
   -- Clear_Destination --
   -----------------------

   procedure Clear_Destination
     (Ship   : in out Root_Ship_Type'Class)
   is
   begin
      Ship.Moving := False;
   end Clear_Destination;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship         : in out Root_Ship_Type'Class;
      Destination  : Concorde.Locations.Object_Location;
      Start_Time   : Concorde.Calendar.Time;
      Journey_Time : Duration)
   is
      use Concorde.Calendar;
   begin
      Ship.Destination := Destination;
      Ship.Moving := True;
      Ship.Jumping := False;
      Ship.Start_Time := Start_Time;
      Ship.Arrival_Time := Start_Time + Journey_Time;
      if Ship.Has_Market then
         Ship.Leave_Market;
      end if;
   end Set_Destination;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship         : in out Root_Ship_Type'Class;
      Destination  : not null access constant
        Concorde.Systems.Star_System_Object_Interface'Class;
      Start_Time   : Concorde.Calendar.Time;
      Journey_Time : Duration)
   is
   begin
      Ship.Set_Destination
        (Concorde.Locations.Geosynchronous_Orbit (Destination),
         Start_Time, Journey_Time);
   end Set_Destination;

   --------------------------
   -- Set_Jump_Destination --
   --------------------------

   procedure Set_Jump_Destination
     (Ship         : in out Root_Ship_Type'Class;
      System       : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Start_Time   : Concorde.Calendar.Time;
      Journey_Time : Duration)
   is
      use Concorde.Calendar;
   begin
      Ship.Destination :=
        Concorde.Locations.System_Transfer_Orbit
          (System, Ship.Current_System);
      Ship.Moving := True;
      Ship.Jumping := True;
      Ship.Start_Time := Start_Time;
      Ship.Arrival_Time := Start_Time + Journey_Time;
   end Set_Jump_Destination;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Ship : in out Root_Ship_Type;
      Name : String)
   is
   begin
      Ship.Ship_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

end Concorde.Ships;
