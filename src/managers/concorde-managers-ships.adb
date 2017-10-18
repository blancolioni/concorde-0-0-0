with Concorde.Signals.Standard;

with Concorde.Calendar;
with Concorde.Random;

with Concorde.Objects.Queues;

with Concorde.Galaxy;
with Concorde.Systems.Graphs;

with Concorde.Ships.Navigation;

package body Concorde.Managers.Ships is

   type Ship_Idle_Handler is
     new Root_Ship_Event_Handler with null record;

   overriding procedure Handle_Ship_Event
     (Handler : in out Ship_Idle_Handler;
      Ship    : Concorde.Ships.Ship_Type);

   type Ship_Arrival_Handler is
     new Concorde.Objects.Object_Handler_Interface with
      record
         Manager  : Ship_Manager;
      end record;

   overriding procedure Handle
     (Handler : in out Ship_Arrival_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   type Ship_Arrival_Event is
     new Concorde.Events.Root_Event_Type with
      record
         Ship        : Concorde.Ships.Ship_Type;
         Arrived_At  : Concorde.Locations.Object_Location;
         Jump        : Boolean;
      end record;

   ------------
   -- Create --
   ------------

   procedure Create
     (Manager : not null access Root_Ship_Manager'Class;
      Ship    : Concorde.Ships.Ship_Type)
   is
   begin
      Manager.Ship := Ship;
      Manager.Object := Ship;
      Ship.Update.Add_Handler
        (Concorde.Signals.Standard.Object_Activated,
         new Object_Activated_Handler'(Manager => Manager_Type (Manager)));
      Ship.Update.Add_Handler
        (Concorde.Signals.Standard.Object_Idle,
         new Ship_Idle_Handler'(Manager => Ship_Manager (Manager)));
      Ship.Update.Add_Handler
        (Concorde.Signals.Standard.Object_Arrived,
         new Ship_Arrival_Handler'(Manager => Ship_Manager (Manager)));
   end Create;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out Root_Ship_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is
   begin
      Handler.Manager.Time := Event.Time_Stamp;
      Root_Ship_Event_Handler'Class (Handler).Handle_Ship_Event
        (Concorde.Ships.Ship_Type (Object));
   end Handle;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out Ship_Arrival_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is
      Manager : constant Ship_Manager := Handler.Manager;
      Ship    : constant Concorde.Ships.Ship_Type :=
                  Concorde.Ships.Ship_Type (Object);
      Arrive : constant Ship_Arrival_Event'Class :=
                 Ship_Arrival_Event'Class (Event);
   begin
      Manager.Time := Event.Time_Stamp;
      Ship.Update.Set_Location (Arrive.Arrived_At);
      if Arrive.Jump then
         Ship.Current_System.Update.Signal
           (Concorde.Systems.Signal_Ship_Arrived,
            Concorde.Systems.Create_Ship_Event
              (Event.Time_Stamp, Ship));
      end if;

      if not Manager.Journey.Is_Empty then
         Manager.Next_Waypoint;
      else
         Manager.On_Idle;
      end if;

   end Handle;

   -----------------------
   -- Handle_Ship_Event --
   -----------------------

   overriding procedure Handle_Ship_Event
     (Handler : in out Ship_Idle_Handler;
      Ship    : Concorde.Ships.Ship_Type)
   is
      pragma Unreferenced (Ship);
   begin
      Handler.Manager.On_Idle;
   end Handle_Ship_Event;

   -------------------
   -- Next_Waypoint --
   -------------------

   procedure Next_Waypoint
     (Manager : in out Root_Ship_Manager'Class)
   is
      use Concorde.Calendar;
      Waypoint : constant Journey_Element_Type :=
                   Manager.Journey.First_Element;
      Departure    : constant Time := Manager.Time;
      Journey_Time : Duration;
   begin
      Manager.Journey.Delete_First;

      case Waypoint.Class is
         when World_Element =>
            Manager.Ship.Log_Movement
              ("heading to world: " & Waypoint.World.Name
               & " currently at "
               & Concorde.Locations.Short_Name
                 (Concorde.Locations.To_System_Point
                      (Waypoint.World.Current_Location,
                       Departure)));
            Journey_Time :=
              Concorde.Ships.Navigation.Journey_Time
                (Manager.Ship, Departure,
                 Waypoint.World.Location_At (Departure));
            Manager.Ship.Update.Set_Destination
              (Waypoint.World, Departure, Journey_Time);

         when System_Element =>
            Manager.Ship.Log_Movement
              ("heading to system orbit: "
               & Concorde.Locations.Short_Name
                 (Waypoint.Point));
            Journey_Time :=
              Concorde.Ships.Navigation.Journey_Time
                (Manager.Ship, Manager.Time,
                 Waypoint.Point);
            Manager.Ship.Update.Set_Destination
              (Waypoint.Point, Departure, Journey_Time);

         when Jump_Element =>
            Manager.Ship.Log_Movement
              ("jumping to " & Waypoint.Target.Name);

            Manager.Ship.Current_System.Update.Departing
              (Manager.Ship, Manager.Time);

            Journey_Time :=
              Duration (100_000.0
                        * (Concorde.Random.Unit_Random + 0.5));
            Manager.Ship.Update.Set_Jump_Destination
              (Waypoint.Target, Departure, Journey_Time);
      end case;

      declare
         Arrival      : constant Time := Departure + Journey_Time;
      begin
         Manager.Ship.Log_Movement
           ("journey time"
            & Natural'Image (Natural (Real (Journey_Time) / 3600.0 - 0.5))
            & " hours; arrival at "
            & Image (Arrival, Include_Time_Fraction => True));

         Concorde.Objects.Queues.Next_Event
           (Manager.Ship, Arrival,
            Concorde.Signals.Standard.Object_Arrived,
            Ship_Arrival_Event'
              (Concorde.Events.Root_Event_Type with
               Ship => Manager.Ship,
               Arrived_At => Manager.Ship.Destination,
               Jump       => Waypoint.Class = Jump_Element));
      end;

   end Next_Waypoint;

   ------------------
   -- On_Activated --
   ------------------

   overriding procedure On_Activated (Manager : in out Root_Ship_Manager) is
   begin
      if not Manager.Journey.Is_Empty then
         Manager.Ship.Update.Set_Location (Manager.Ship.Destination);
         Manager.Next_Waypoint;
      else
         if Manager.Ship.Orbiting_World
           and then Manager.Ship.Current_World.Has_Market
         then
            Manager.Ship.Update.Set_Market
              (Manager.Ship.Current_World.Market);
         end if;
         Root_Ship_Manager'Class (Manager).On_Idle;
      end if;
   end On_Activated;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Manager : not null access Root_Ship_Manager'Class;
      World   : not null access constant
        Concorde.Worlds.Root_World_Type'Class)
   is
      use Concorde.Calendar;
      use type Concorde.Systems.Star_System_Type;
      Target_System : constant Concorde.Systems.Star_System_Type :=
                        World.System;
      Current_System : constant Concorde.Systems.Star_System_Type :=
                         Manager.Ship.Current_System;
      Journey : Journey_Element_Lists.List;
   begin
      if Target_System /= Current_System then
         declare
            use Concorde.Galaxy;
            Path : constant Concorde.Systems.Graphs.Array_Of_Vertices :=
                     Shortest_Path (Current_System, Target_System);
            Current : Concorde.Systems.Star_System_Type := Current_System;
         begin
            for Waypoint_Vertex of Path loop
               declare
                  Waypoint : constant Concorde.Systems.Star_System_Type :=
                               Concorde.Systems.Get (Waypoint_Vertex);
               begin
                  Manager.Ship.Log_Movement
                    ("waypoint: " & Waypoint.Name);
                  Journey.Append
                    (Journey_Element_Type'
                       (Class  => System_Element,
                        Point  =>
                          Concorde.Locations.System_Transfer_Orbit
                            (Current, Waypoint)));
                  Journey.Append
                    (Journey_Element_Type'
                       (Class  => Jump_Element,
                        Target => Waypoint));
                  Current := Waypoint;
               end;
            end loop;
         end;
      end if;

      Journey.Append
        (Journey_Element_Type'
           (Class  => World_Element,
            World  => Concorde.Worlds.World_Type (World)));
      Manager.Journey := Journey;

      Manager.Next_Waypoint;

   end Set_Destination;

end Concorde.Managers.Ships;
