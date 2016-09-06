with Ada.Text_IO;

with WL.Processes;
with WL.Random.Names;
with WL.Work;

with Memor;

with Xi.Assets;
with Xi.Main;
with Xi.Render_Window;
with Xi.Shader.Load;

with Xtk;

with Concorde.Paths;

with Concorde.Logging;

with Concorde.Empires.Configure;
with Concorde.Galaxy.Create;

with Concorde.Galaxy.Locking;

with Concorde.Empires.Reports;
with Concorde.Empires.Updates;
with Concorde.Updates;

with Concorde.Gtk_UI;
with Concorde.Xi_UI.Model_Manager;
with Concorde.Xi_UI.Key_Bindings;

with Concorde.Empires.Logging;

with Concorde.Options;
with Concorde.Reports;

with Concorde.Players.Registry;

with Concorde.Configure;

with Concorde.Agents;

procedure Concorde.Driver is

   Name_Generator : WL.Random.Names.Name_Generator;

   Check_Invariants : constant Boolean :=
                        Concorde.Options.Check_Invariants;

   Interface_Name   : constant String :=
                        Concorde.Options.Interface_Name;
   Use_Gtk          : constant Boolean := Interface_Name = "gtk";
   Use_Xi           : constant Boolean := Interface_Name = "xi";

begin

   Memor.Locking (True);
   WL.Work.Set_Task_Count (Concorde.Options.Work_Threads);

   WL.Random.Names.Load_Lexicon
     (Name_Generator,
      Concorde.Paths.Config_File ("totro-vowels.txt"),
      Concorde.Paths.Config_File ("totro-consonants.txt"));

   if Concorde.Options.Randomise then
      WL.Random.Randomise;
   end if;

   Concorde.Players.Registry.Register_Players;

   Concorde.Configure.Load_Configuration;

   if Concorde.Options.Create_Galaxy then
      if Concorde.Options.Galaxy_Shape = "catalogue" then
         Concorde.Galaxy.Create.Create_Catalogue_Systems
           (Concorde.Paths.Config_File
              ("hygdata_v3.csv"));

      else
         declare
            use type Concorde.Galaxy.Create.Galaxy_Shape;
            Shape : constant Concorde.Galaxy.Create.Galaxy_Shape :=
                      Concorde.Galaxy.Create.Galaxy_Shape'Value
                        (Concorde.Options.Galaxy_Shape);
         begin
            Concorde.Galaxy.Create.Create_Galaxy
              (System_Count        => Concorde.Options.Number_Of_Systems,
               Shape               => Shape,
               DX                  => Concorde.Options.System_X_Deviation,
               DY                  => Concorde.Options.System_Y_Deviation,
               DZ                  => Concorde.Options.System_Z_Deviation,
               Average_Connections => Concorde.Options.Average_Connections,
               Reset_Seed          => Concorde.Options.Randomise,
               Name_Generator      => Name_Generator);
         end;
      end if;

      if Concorde.Options.Create_Empires then
         Concorde.Empires.Configure.Create_Empires
           (Count => Concorde.Options.Number_Of_Empires);

         if Concorde.Options.Enable_Empire_Logging then
            Concorde.Empires.Logging.Start_Logging;
         end if;

         Concorde.Agents.Enable_Offer_Logging (Enabled => True);
         Concorde.Logging.Start_Logging;

         Concorde.Empires.Updates.Start;
      end if;
   end if;

   if Concorde.Options.Console then
      declare
         Process : WL.Processes.Process_Type;
      begin
         Process.Start_Percentage
           ("Updating",
            Finish => Concorde.Options.Number_Of_Updates);

         Concorde.Galaxy.Locking.Init_Locking;

         for I in 1 .. Concorde.Options.Number_Of_Updates loop
            Concorde.Updates.Perform_Update
              (Execute_Battles  => True,
               Check_Invariants => Check_Invariants);
            Process.Tick;
         end loop;
         Process.Finish;
      end;

   else

      Concorde.Updates.Start_Updates;

      if Use_Gtk then
         Concorde.Gtk_UI.Start;
      elsif Use_Xi then

         declare
            Window : Xi.Render_Window.Xi_Render_Window;
         begin

            Xi.Main.Init;
            Xtk.Initialize;

            Xi.Assets.Add_Search_Path
              (Concorde.Paths.Config_Path);

            Xi.Assets.Add_Image_Path
              (Concorde.Paths.Config_Path);

            Concorde.Xi_UI.Key_Bindings.Load_Key_Bindings;

            Xi.Shader.Load.Add_Search_Path
              (Concorde.Paths.Config_File ("shaders"));

            Window :=
              Xi.Main.Current_Renderer.Create_Top_Level_Window;

            Window.Set_Full_Screen (True);

--              Concorde.Xi_UI.Model_Manager.Load_Top_Model (Window);

            Concorde.Xi_UI.Model_Manager.Model
              (Concorde.Galaxy.Capital_World, Window).Activate;

            Concorde.Updates.Set_Time_Acceleration (60.0);

            Xi.Main.Main_Loop;

         end;

      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "unknown interface: " & Interface_Name);
      end if;

      Concorde.Updates.Stop_Updates;

   end if;

   WL.Work.Stop_Work_Tasks;

   Concorde.Empires.Reports.Report_Empires;

   if Concorde.Options.Write_Accounts then
      Concorde.Reports.Write_Accounts;
   end if;

   Concorde.Logging.Stop_Logging;

   if Concorde.Options.Enable_Empire_Logging then
      Concorde.Empires.Logging.Stop_Logging;
   end if;

exception

   when others =>
      if Concorde.Options.Enable_Empire_Logging then
         Concorde.Empires.Logging.Stop_Logging;
      end if;
      Concorde.Logging.Stop_Logging;
      WL.Work.Stop_Work_Tasks;
      raise;

end Concorde.Driver;
