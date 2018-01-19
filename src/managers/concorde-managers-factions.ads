private with Concorde.Events;

with Concorde.Factions;

package Concorde.Managers.Factions is

   type Root_Faction_Manager is
     new Root_Manager_Type with private;

   procedure Create
     (Manager : not null access Root_Faction_Manager'Class;
      Faction : Concorde.Factions.Faction_Type);

   type Faction_Manager is access all Root_Faction_Manager'Class;

   function Create_Manager
     (Faction : Concorde.Factions.Faction_Type)
      return Faction_Manager;

private

   type Root_Faction_Manager is
     new Root_Manager_Type with
      record
         Faction : Concorde.Factions.Faction_Type;
      end record;

   overriding procedure On_Activated
     (Manager : in out Root_Faction_Manager);

   type Root_Faction_Event_Handler is
     abstract new Concorde.Objects.Object_Handler_Interface with
      record
         Manager : Faction_Manager;
      end record;

   overriding procedure Handle
     (Handler : in out Root_Faction_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   procedure Handle_Faction_Event
     (Handler : in out Root_Faction_Event_Handler;
      Faction     : Concorde.Factions.Faction_Type)
   is abstract;

end Concorde.Managers.Factions;
