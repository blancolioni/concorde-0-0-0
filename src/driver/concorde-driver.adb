with WL.Command_Line;
with WL.Processes;
with WL.Random.Names;

with Memor;

with Xi.Assets;
with Xi.Main;
with Xi.Render_Window;
with Xi.Shader.Load;

with Xtk;

with Concorde.Paths;

with Concorde.Logging;
with Concorde.Logs;

with Concorde.Factions.Configure;
with Concorde.Galaxy.Create;

with Concorde.Galaxy.Locking;

with Concorde.Factions.Reports;
with Concorde.Factions.Updates;
with Concorde.Updates;

--  with Concorde.Gtk_UI;
with Concorde.Xi_UI.Model_Manager;
with Concorde.Xi_UI.Key_Bindings;

with Concorde.Factions.Logging;

with Concorde.Options;

with Concorde.Configure;

with Concorde.Agents;
with Concorde.Calendar;

procedure Concorde.Driver is

   Name_Generator : WL.Random.Names.Name_Generator;

begin

   WL.Command_Line.Load_Defaults
     (Concorde.Paths.Config_File ("options.txt"));

   Memor.Locking (False);

   WL.Random.Names.Load_Lexicon
     (Name_Generator,
      Concorde.Paths.Config_File ("totro-vowels.txt"),
      Concorde.Paths.Config_File ("totro-consonants.txt"));

   if Concorde.Options.Randomise then
      WL.Random.Randomise;
   end if;

   Concorde.Logging.Start_Logging;

   Concorde.Configure.Load_Configuration;

   if Concorde.Options.Create_Galaxy then
      if Concorde.Options.Galaxy_Shape = "catalogue" then
         Concorde.Galaxy.Create.Create_Catalogue_Systems
           (Concorde.Paths.Config_File
              ("hygdata_v3.csv"));

      else
         declare
            use Xi;
            use type Concorde.Galaxy.Create.Galaxy_Shape;
            Shape : constant Concorde.Galaxy.Create.Galaxy_Shape :=
                      Concorde.Galaxy.Create.Galaxy_Shape'Value
                        (Concorde.Options.Galaxy_Shape);
         begin
            Concorde.Galaxy.Create.Create_Galaxy
              (System_Count        => Concorde.Options.System_Count,
               Shape               => Shape,
               DX                  =>
                 Xi_Float (Concorde.Options.System_X_Deviation) / 100.0,
               DY                  =>
                 Xi_Float (Concorde.Options.System_Y_Deviation) / 100.0,
               DZ                  =>
                 Xi_Float (Concorde.Options.System_Z_Deviation) / 100.0,
               Average_Connections => Concorde.Options.Average_Connections,
               Name_Generator      => Name_Generator);
         end;
      end if;

      if Concorde.Options.Create_Factions then
         Concorde.Factions.Configure.Create_Factions
           (Count => Concorde.Options.Faction_Count);

         if Concorde.Options.Enable_Faction_Logging then
            Concorde.Factions.Logging.Start_Logging;
         end if;

         Concorde.Agents.Enable_Offer_Logging (Enabled => True);

         Concorde.Factions.Updates.Start;
      end if;
   end if;

   if Concorde.Options.Console then
      declare
         Process : WL.Processes.Process_Type;
         Update_Count : constant Natural :=
                      Concorde.Options.Update_Count * 24 * 60;
      begin
         Process.Start_Percentage
           ("Updating",
            Finish => Update_Count);

         Concorde.Galaxy.Locking.Init_Locking;

         for I in 1 .. Update_Count loop
            Concorde.Updates.Advance (60.0);
            Process.Tick;
         end loop;
         Process.Finish;
      end;

   else

      declare
         Window : Xi.Render_Window.Xi_Render_Window;
      begin

         Xi.Main.Init;
         Xi.Assets.Add_Search_Path
           (Concorde.Paths.Config_Path);

         Xi.Assets.Add_Image_Path
           (Concorde.Paths.Config_Path);

         Concorde.Xi_UI.Key_Bindings.Load_Key_Bindings;

         Xi.Shader.Load.Add_Search_Path
           (Concorde.Paths.Config_File ("shaders"));

         Xtk.Initialize;

         Window :=
           Xi.Main.Current_Renderer.Create_Top_Level_Window;

         if Concorde.Options.Full_Screen then
            Window.Set_Full_Screen (True);
         end if;

         Concorde.Xi_UI.Load_UI
           (Window, Concorde.Paths.Config_File ("html/main.html"));

         declare
            Model : Concorde.Xi_UI.Xi_Model;
            Faction_Option : constant String :=
                               Concorde.Options.Faction_Name;
            Faction_Name   : constant String :=
                               (if Faction_Option = ""
                                then "Musitello"
                                else Faction_Option);
            Faction        : constant Concorde.Factions.Faction_Type :=
                               Concorde.Factions.Get_By_Name (Faction_Name);
         begin
            if Concorde.Options.Start_With_Galaxy then
               Model :=
                 Concorde.Xi_UI.Model_Manager.Model
                   (null, Concorde.Calendar.Start, Faction, Window);
            elsif True then
               Model :=
                 Concorde.Xi_UI.Model_Manager.Model
                   (Concorde.Galaxy.Get_System (1),
                    Concorde.Calendar.Start, Faction, Window);
            else
               Model :=
                 Concorde.Xi_UI.Model_Manager.Model
                   (Concorde.Galaxy.Capital_World,
                    Concorde.Calendar.Start, Faction, Window);
            end if;

            Model.Activate;

         end;

         Xi.Main.Main_Loop;

      end;

   end if;

   Concorde.Factions.Reports.Report_Factions;

   Concorde.Logging.Stop_Logging;

   if Concorde.Options.Enable_Faction_Logging then
      Concorde.Factions.Logging.Stop_Logging;
   end if;

   if Concorde.Options.Detailed_Logging then
      Concorde.Logs.Flush_Logs;
   end if;

exception

   when others =>
      if Concorde.Options.Enable_Faction_Logging then
         Concorde.Factions.Logging.Stop_Logging;
      end if;
      Concorde.Logging.Stop_Logging;
      Concorde.Logs.Flush_Logs;
      raise;

end Concorde.Driver;
