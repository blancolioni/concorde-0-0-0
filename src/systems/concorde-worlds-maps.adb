with Concorde.Options;

with Concorde.Maps.Height_Maps;

package body Concorde.Worlds.Maps is

   --     World_Continent_Smoothing : Natural := 0;
   --     World_Fractal_Iterations  : Natural := 0;
      World_Height_Smoothing    : Natural := 0;

   procedure Create_World_Map
     (World : in out Root_World_Type'Class)
   is
      Layout : World_Layout_Type :=
                 (Surface => World.Surface,
                  Sectors => World.Sectors);
      Frequency   : Concorde.Maps.Frequency_Array
        (1 .. Max_Height - Min_Height + 1);
      Freq_Sum    : Non_Negative_Real := 0.0;
   begin
      Frequency (1) := 0.0;
      for I in Min_Height + 1 .. -1 loop
         Frequency (I - Min_Height + 1) :=
           0.6 * World.Hydrosphere / Real (-Min_Height - 1);
      end loop;
      for I in 0 .. Max_Height - 8 loop
         Frequency (I - Min_Height + 1) :=
           (1.0 - World.Hydrosphere) * 0.8 / Real (Max_Height + 1);
      end loop;

      for I in Max_Height - 7 .. Max_Height loop
         Frequency (I - Min_Height + 1) :=
           (1.0 - World.Hydrosphere) * 0.2 / Real (Max_Height + 1);
      end loop;

      for F of Frequency loop
         Freq_Sum := Freq_Sum + F;
      end loop;

      if Freq_Sum /= 1.0 then
         for I in Frequency'Range loop
            Frequency (I) := Frequency (I) / Freq_Sum;
         end loop;
      end if;

      if World_Height_Smoothing = 0 then
         World_Height_Smoothing := Concorde.Options.World_Height_Smoothing;
      end if;

      Concorde.Maps.Height_Maps.Create_Height_Map
        (Layout, Frequency, World_Height_Smoothing);

   end Create_World_Map;

end Concorde.Worlds.Maps;
