with Tropos;

limited with Concorde.People.Individuals;
limited with Concorde.Worlds;

package Concorde.Laws.Configure is

   function Configure_Law
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type;

   function World_Context
     (World : not null access constant
        Concorde.Worlds.Root_World_Type'Class)
      return Law_Context;

   function Individual_Context
     (Individual : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
      return Law_Context;

end Concorde.Laws.Configure;
