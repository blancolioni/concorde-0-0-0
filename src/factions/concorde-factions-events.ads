with Concorde.Events;
with Concorde.Signals;

with Concorde.People.Individuals;

package Concorde.Factions.Events is

   Signal_Office_Changed : constant Concorde.Signals.Signal_Type;

   type Office_Changed_Event is
     new Concorde.Events.Root_Event_Type with
      record
         Office : Concorde.Offices.Office_Type;
         Old_Minister : Concorde.People.Individuals.Individual_Type;
         New_Minister : Concorde.People.Individuals.Individual_Type;
      end record;

private

   Signal_Office_Changed : constant Concorde.Signals.Signal_Type :=
                             "signal-faction-office-changed";

end Concorde.Factions.Events;
