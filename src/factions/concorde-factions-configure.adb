with Ada.Text_IO;

with Tropos.Reader;
with Concorde.Paths;

with Concorde.Configure;
with Concorde.Powers.Configure;
with Concorde.Factions.Create;

with Lui.Colours;

with Concorde.Laws.Configure;

with Concorde.Worlds;

package body Concorde.Factions.Configure is

   ---------------------
   -- Create_Factions --
   ---------------------

   procedure Create_Factions (Count : Natural) is
      Faction_Config : constant Tropos.Configuration :=
                        Tropos.Reader.Read_Config
                           (Concorde.Paths.Config_File ("factions.txt"));
      Imperium_Config : constant Tropos.Configuration :=
                          Tropos.Reader.Read_Config
                            (Concorde.Configure.File_Path
                               ("init", "imperium-laws", "txt"));
      Current       : Natural := 0;
   begin
      for Config of Faction_Config loop
         exit when Current >= Count;
         Ada.Text_IO.Put_Line ("New Faction: " & Config.Config_Name);
         declare
            Name : constant String :=
                     Config.Get ("name", Config.Config_Name);
            Capital : constant String :=
                        Config.Get ("capital", Name);
            Colour  : constant Tropos.Configuration :=
                        Config.Child ("colour");
            Red     : constant Natural := Colour.Get (1);
            Green   : constant Natural := Colour.Get (2);
            Blue    : constant Natural := Colour.Get (3);
            Faction : constant Faction_Type :=
                        Concorde.Factions.Create.New_Faction
                          (Name    => Name,
                           Capital => Capital,
                           Colour  =>
                             Lui.Colours.To_Colour
                               (Lui.Colours.Colour_Byte (Red),
                                Lui.Colours.Colour_Byte (Green),
                                Lui.Colours.Colour_Byte (Blue)),
                           Default_Ship_Design =>
                             Config.Get ("design", "defender"));
            House_Ministry : constant Concorde.Ministries.Ministry_Type :=
                               Faction.First_Ministry;

            Laws_Config     : constant Tropos.Configuration :=
                                Tropos.Reader.Read_Config
                                  (Concorde.Configure.File_Path
                                     ("init", "laws", "txt"));
            Capital_Context : constant Concorde.Laws.Law_Context :=
                                Concorde.Laws.Configure.World_Context
                                  (Faction.Capital_World);
         begin

            if Current = 0 then
               Imperium_Faction := Faction;
            end if;

            for Law_Config of Imperium_Config.Child ("faction_laws") loop
               if Current = 0 then
                  House_Ministry.Update.Add_Power
                    (Concorde.Powers.Configure.Configure_Power
                       (Law_Config.Child (1)));
               else
                  declare
                     Context : constant Concorde.Laws.Law_Context :=
                                 Concorde.Laws.Configure.Vassal_Context
                                   (Imperium_Faction, Faction);
                     Law     : constant Concorde.Laws.Law_Type :=
                                 Concorde.Laws.Configure.Configure_Law
                                   (Context => Context,
                                    Config  => Law_Config);
                  begin
                     Imperium_Faction.Update.Add_Law (Law);
                     if Law.Can_Enact then
                        Ada.Text_IO.Put_Line ("Enacting: " & Law.Show);
                        Law.Enact;
                     else
                        Ada.Text_IO.Put_Line ("Cannot enact: " & Law.Show);
                     end if;
                  end;
               end if;
            end loop;

            for Law_Config of Laws_Config loop
               declare
                  Law : constant Concorde.Laws.Law_Type :=
                          Concorde.Laws.Configure.Configure_Law
                            (Context => Capital_Context,
                             Config  => Law_Config);
               begin
                  Faction.Update.Add_Law (Law);
                  if Law.Can_Enact then
                     Ada.Text_IO.Put_Line ("Enacting: " & Law.Show);
                     Law.Enact;
                  else
                     Ada.Text_IO.Put_Line ("Cannot enact: " & Law.Show);
                  end if;
               end;
            end loop;
         end;

         Current := Current + 1;
      end loop;

   end Create_Factions;

end Concorde.Factions.Configure;
