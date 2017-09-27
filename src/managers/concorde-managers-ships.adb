with Concorde.Signals.Standard;

with Concorde.Dates;
with Concorde.Objects.Queues;

package body Concorde.Managers.Ships is

   type Ship_Idle_Handler is
     new Root_Ship_Event_Handler with null record;

   overriding procedure Handle_Ship_Event
     (Handler : in out Ship_Idle_Handler;
      Ship    : Concorde.Ships.Ship_Type);

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
      pragma Unreferenced (Event);
   begin
      Root_Ship_Event_Handler'Class (Handler).Handle_Ship_Event
        (Concorde.Ships.Ship_Type (Object));
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

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Manager : not null access Root_Ship_Manager'Class;
      World   : not null access constant
        Concorde.Worlds.Root_World_Type'Class)
   is
      use Concorde.Dates;
      Arrival_Date : constant Date_Type :=
                       Add_Seconds
                         (Current_Date, 100_000.0);
   begin
      Manager.Ship.Log_Movement
        ("moving to " & World.Name & "; arrival "
         & To_Date_And_Time_String (Arrival_Date));
      Concorde.Objects.Queues.Next_Event
        (Manager.Ship, Arrival_Date);
   end Set_Destination;

end Concorde.Managers.Ships;
