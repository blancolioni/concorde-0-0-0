with Concorde.Stars;
with Concorde.Systems;

with Concorde.Worlds.Lists;

package Concorde.Worlds.Create is

   procedure Create_Worlds
     (System : Concorde.Systems.Star_System_Type;
      Star   : Concorde.Stars.Star_Type;
      List   : in out Concorde.Worlds.Lists.List);

   procedure Create_Sector_Layout
     (World       : in out Root_World_Type'Class);

   procedure Create_Resources
     (World : in out Root_World_Type'Class);

end Concorde.Worlds.Create;
