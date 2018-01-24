with Concorde.Xi_UI.Galaxies;

with Concorde.Systems.Xi_Model;
with Concorde.Worlds.Xi_Model;

package body Concorde.Xi_UI.Model_Manager is

   Top_Model : Xi_Model := null;
   pragma Unreferenced (Top_Model);

   --------------------
   -- Load_Top_Model --
   --------------------

   procedure Load_Top_Model
     (Time       : Concorde.Calendar.Time;
      Faction    : Concorde.Factions.Faction_Type;
      Renderer   : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
   is
   begin
      Top_Model := Model (null, Time, Faction, Renderer);
   end Load_Top_Model;

   -----------
   -- Model --
   -----------

   function Model
     (For_Object : access constant
        Concorde.Objects.Root_Object_Type'Class;
      Time       : Concorde.Calendar.Time;
      Faction    : Concorde.Factions.Faction_Type;
      Renderer   : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
      return Xi_Model
   is
      use type Concorde.Objects.Object_Type;
   begin
      if For_Object = null then
         return Concorde.Xi_UI.Galaxies.Galaxy_Model (Faction, Renderer);
      elsif For_Object.all in Concorde.Systems.Root_Star_System_Type'Class then
         return Concorde.Systems.Xi_Model.System_Model
           (System  => Concorde.Systems.Star_System_Type (For_Object),
            Faction => Faction,
            View    => Concorde.Systems.Xi_Model.Schematic,
            Target  => Renderer);
      elsif For_Object.all in Concorde.Worlds.Root_World_Type'Class then
         return Concorde.Worlds.Xi_Model.World_Model
           (Concorde.Worlds.World_Type (For_Object), Time,
            Faction, Renderer);
      else
         return null;
      end if;
   end Model;

end Concorde.Xi_UI.Model_Manager;
