private with Concorde.Events;

with Concorde.Ships;

package Concorde.Managers.Ships is

   type Root_Ship_Manager is
     abstract new Root_Manager_Type with private;

   procedure On_Idle (Manager : in out Root_Ship_Manager) is abstract;

   procedure Create
     (Manager : not null access Root_Ship_Manager'Class;
      Ship    : Concorde.Ships.Ship_Type);

   type Ship_Manager is access all Root_Ship_Manager'Class;

private

   type Root_Ship_Manager is
     abstract new Root_Manager_Type with
      record
         Ship : Concorde.Ships.Ship_Type;
      end record;

   type Root_Ship_Event_Handler is
     abstract new Concorde.Objects.Object_Handler_Interface with
      record
         Manager : Ship_Manager;
      end record;

   overriding procedure Handle
     (Handler : in out Root_Ship_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   procedure Handle_Ship_Event
     (Handler : in out Root_Ship_Event_Handler;
      Ship    : Concorde.Ships.Ship_Type)
   is abstract;

end Concorde.Managers.Ships;
