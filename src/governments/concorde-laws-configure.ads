with Tropos;

limited with Concorde.People.Communities;
limited with Concorde.Factions;
limited with Concorde.People.Individuals;

package Concorde.Laws.Configure is

   function Configure_Law
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type;

   function Vassal_Context
     (Ruler  : not null access constant
        Concorde.Factions.Root_Faction_Type'Class;
      Vassal : not null access constant
        Concorde.Factions.Root_Faction_Type'Class)
      return Law_Context;

   function Individual_Context
     (Individual : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
      return Law_Context;

   function Community_Context
     (Community : not null access constant
        Concorde.People.Communities.Root_Community_Type'Class)
      return Law_Context;

end Concorde.Laws.Configure;
