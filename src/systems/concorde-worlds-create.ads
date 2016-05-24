with Concorde.Stars;
with Concorde.Systems;

with Concorde.Worlds.Lists;

package Concorde.Worlds.Create is

   procedure Create_Worlds
     (System : Concorde.Systems.Star_System_Type;
      Star   : Concorde.Stars.Star_Type;
      List   : in out Concorde.Worlds.Lists.List);

end Concorde.Worlds.Create;
