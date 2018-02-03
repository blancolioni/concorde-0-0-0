with Concorde.Events;
with Concorde.Signals;

with Concorde.People.Individuals;

package Concorde.Factions.Events is

   Signal_Ministry_Changed : constant Concorde.Signals.Signal_Type;

   type Ministry_Changed_Event is
     new Concorde.Events.Root_Event_Type with
      record
         Ministry     : Concorde.Ministries.Ministry_Type;
         Old_Minister : Concorde.People.Individuals.Individual_Type;
         New_Minister : Concorde.People.Individuals.Individual_Type;
      end record;

private

   Signal_Ministry_Changed : constant Concorde.Signals.Signal_Type :=
                               "signal-faction-ministry-changed";

end Concorde.Factions.Events;
