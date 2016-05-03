with Tropos.Reader;

with Concorde.Paths;

package body Concorde.Terrain.Surface_Maps is

   Local_Surface_Maps : array (Concorde.Worlds.World_Category)
     of access Surface_Map;
   Have_Surface_Maps  : Boolean := False;

   procedure Configure_Surface_Maps;

   ----------------------------
   -- Configure_Surface_Maps --
   ----------------------------

   procedure Configure_Surface_Maps is
      World_Config : constant Tropos.Configuration :=
                       Tropos.Reader.Read_Config
                         (Concorde.Paths.Config_File
                            ("star-systems/world-category.txt"));
   begin
      for Category_Config of World_Config loop
         declare
            Category : constant Concorde.Worlds.World_Category :=
                         Concorde.Worlds.World_Category'Value
                           (Category_Config.Config_Name);
            Surface_Config : constant Tropos.Configuration :=
                               Category_Config.Child ("terrain");
            Map            : Surface_Map (1 .. Surface_Config.Child_Count);
            Count          : Natural := 0;
         begin
            for Config of Surface_Config loop
               Count := Count + 1;
               Map (Count) := (Terrain => Get (Config.Config_Name),
                               Frequency =>
                                 Unit_Real (Float'(Config.Value)));
            end loop;
            Local_Surface_Maps (Category) :=
              new Surface_Map'(Map);
         end;
      end loop;
      Have_Surface_Maps := True;
   end Configure_Surface_Maps;

   ---------------------
   -- Get_Surface_Map --
   ---------------------

   function Get_Surface_Map
     (Category       : Concorde.Worlds.World_Category;
      Water_Coverage : Unit_Real)
      return Surface_Map
   is
      No_Surface : Surface_Map (1 .. 0);
   begin
      if not Have_Surface_Maps then
         Configure_Surface_Maps;
      end if;

      if Local_Surface_Maps (Category) = null then
         return No_Surface;
      end if;

      return Map : Surface_Map :=
        Local_Surface_Maps (Category).all
      do
         for I in Map'Range loop
            if Map (I).Terrain.Is_Water then
               Map (I).Frequency := Water_Coverage;
            end if;
         end loop;
      end return;
   end Get_Surface_Map;

end Concorde.Terrain.Surface_Maps;
