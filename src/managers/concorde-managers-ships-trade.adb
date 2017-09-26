with Concorde.Locations;
with Concorde.Signals.Standard;

package body Concorde.Managers.Ships.Trade is

   type Ship_Arrival_Handler is
     new Root_Ship_Event_Handler with null record;

   overriding procedure Handle_Ship_Event
     (Handler : in out Ship_Arrival_Handler;
      Ship    : Concorde.Ships.Ship_Type);

   ------------------
   -- Add_Waypoint --
   ------------------

   procedure Add_Waypoint
     (Manager : in out Root_Ship_Trade_Manager'Class;
      World   : Concorde.Worlds.World_Type)
   is
   begin
      Manager.Route.Append (World);
   end Add_Waypoint;

   ------------
   -- Create --
   ------------

   function Create_Manager
     (Ship    : Concorde.Ships.Ship_Type;
      Start   : Concorde.Worlds.World_Type)
      return Ship_Trade_Manager
   is
   begin
      return Manager : constant Ship_Trade_Manager :=
        new Root_Ship_Trade_Manager
      do
         Manager.Create (Ship);
         Manager.Route.Append (Start);
         Ship.Update.Add_Handler
           (Concorde.Signals.Standard.Object_Arrived,
            new Ship_Arrival_Handler);
      end return;
   end Create_Manager;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Ship     : Concorde.Ships.Ship_Type;
      From, To : Concorde.Worlds.World_Type)
      return Ship_Trade_Manager
   is
   begin
      return Manager : constant Ship_Trade_Manager :=
        Create_Manager (Ship, From)
      do
         Manager.Add_Waypoint (To);
      end return;
   end Create_Manager;

   ---------------------
   -- Delete_Waypoint --
   ---------------------

   procedure Delete_Waypoint
     (Manager : in out Root_Ship_Trade_Manager'Class;
      World   : Concorde.Worlds.World_Type)
   is
      Position : World_Lists.Cursor := Manager.Route.Find (World);
   begin
      pragma Assert (World_Lists.Has_Element (Position));
      Manager.Route.Delete (Position);
   end Delete_Waypoint;

   -----------------------
   -- Handle_Ship_Event --
   -----------------------

   overriding procedure Handle_Ship_Event
     (Handler : in out Ship_Arrival_Handler;
      Ship    : Concorde.Ships.Ship_Type)
   is
      pragma Unreferenced (Handler);
   begin
      Ship.Log_Trade ("arrived at "
                & Concorde.Locations.Short_Name
                  (Ship.Current_Location));
   end Handle_Ship_Event;

   ------------------
   -- On_Activated --
   ------------------

   overriding procedure On_Activated
     (Manager : in out Root_Ship_Trade_Manager)
   is
   begin
      Manager.Ship.Log_Trade ("activated");
   end On_Activated;

   -------------
   -- On_Idle --
   -------------

   overriding procedure On_Idle
     (Manager : in out Root_Ship_Trade_Manager)
   is
   begin
      Manager.Ship.Log_Trade ("idle");
   end On_Idle;

end Concorde.Managers.Ships.Trade;
