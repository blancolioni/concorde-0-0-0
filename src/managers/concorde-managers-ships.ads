private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Concorde.Events;

with Concorde.Locations;
with Concorde.Ships;
with Concorde.Systems;
with Concorde.Worlds;

package Concorde.Managers.Ships is

   type Root_Ship_Manager is
     abstract new Root_Manager_Type with private;

   overriding procedure On_Activated (Manager : in out Root_Ship_Manager);

   procedure On_Idle (Manager : in out Root_Ship_Manager) is abstract;

   procedure Create
     (Manager : not null access Root_Ship_Manager'Class;
      Ship    : Concorde.Ships.Ship_Type);

   procedure Set_Destination
     (Manager : not null access Root_Ship_Manager'Class;
      World   : not null access constant
        Concorde.Worlds.Root_World_Type'Class);

   type Ship_Manager is access all Root_Ship_Manager'Class;

private

   type Journey_Element_Class is
     (World_Element, System_Element, Jump_Element);

   type Journey_Element_Type (Class : Journey_Element_Class) is
      record
         case Class is
            when World_Element =>
               World  : Concorde.Worlds.World_Type;
            when System_Element =>
               Point  : Concorde.Locations.Object_Location;
            when Jump_Element =>
               Target : Concorde.Systems.Star_System_Type;
         end case;
      end record;

   package Journey_Element_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Journey_Element_Type);

   type Root_Ship_Manager is
     abstract new Root_Manager_Type with
      record
         Ship    : Concorde.Ships.Ship_Type;
         Journey : Journey_Element_Lists.List;
      end record;

   procedure Next_Waypoint
     (Manager : in out Root_Ship_Manager'Class);

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
