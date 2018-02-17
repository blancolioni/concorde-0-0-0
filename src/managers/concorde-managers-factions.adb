with Concorde.Objects.Queues;
with Concorde.Signals.Standard;

with Concorde.Commodities;
with Concorde.Facilities;
with Concorde.Trades;
with Concorde.Worlds;

with Concorde.People.Individuals;

package body Concorde.Managers.Factions is

   ------------
   -- Create --
   ------------

   procedure Create
     (Manager : not null access Root_Faction_Manager'Class;
      Faction     : Concorde.Factions.Faction_Type)
   is
   begin
      Manager.Object := Faction;
      Manager.Faction := Faction;
      Faction.Update.Add_Handler
        (Concorde.Signals.Standard.Object_Activated,
         new Object_Activated_Handler'(Manager => Manager_Type (Manager)));
   end Create;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Faction : Concorde.Factions.Faction_Type)
      return Faction_Manager
   is
   begin
      return Manager : constant Faction_Manager := new Root_Faction_Manager do
         Manager.Create (Faction);
         Faction.Update.Set_Manager (Manager_Type (Manager));
      end return;
   end Create_Manager;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out Root_Faction_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is
      pragma Unreferenced (Object);
   begin
      Handler.Manager.Time := Event.Time_Stamp;
      Handler.Manager.On_Activated;
   end Handle;

   ------------------
   -- On_Activated --
   ------------------

   overriding procedure On_Activated
     (Manager : in out Root_Faction_Manager)
   is
      Changed        : Boolean := False;
      Remaining_Work : Work_Item_Queue.Heap;
   begin
      while not Manager.Work_Queue.Is_Empty loop
         declare
            Priority : constant Concorde.Work.Work_Priority :=
                         Manager.Work_Queue.First_Key;
            Work     : constant Concorde.Work.Work_Item :=
                         Manager.Work_Queue.First_Element;
         begin
            Manager.Work_Queue.Delete_First;
            if Manager.Faction.Has_Delegated_Power (Work.Power) then
               Manager.Faction.Find_With_Power (Work.Power)
                 .Director.Manager.Add_Work_Item (Work);
               Changed := True;
            elsif Manager.Faction.Has_Power (Work.Power) then
               Manager.Faction.Leader.Manager.Add_Work_Item (Work);
               Changed := True;
            else
               Remaining_Work.Insert (Priority, Work);
            end if;
         end;
      end loop;

      if Changed then
         Manager.Work_Queue := Remaining_Work;
      end if;

      Concorde.Objects.Queues.Next_Event
        (Manager.Faction, Manager.Time, Delay_Days => 1);
   end On_Activated;

end Concorde.Managers.Factions;
