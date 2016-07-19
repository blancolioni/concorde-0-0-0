with WL.Processes;
with WL.Random.Names;
with WL.Work;

with Memor;

with Concorde.Paths;

with Concorde.Logging;

with Concorde.Empires.Configure;
with Concorde.Galaxy.Create;

with Concorde.Galaxy.Locking;

with Concorde.Empires.Reports;
with Concorde.Empires.Updates;
with Concorde.Updates;

with Concorde.Gtk_UI;

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

   Concorde.Galaxy.Create.Create_Galaxy
     (System_Count        => Concorde.Options.Number_Of_Systems,
      Shape => Concorde.Galaxy.Create.Spiral,
      Average_Connections => Concorde.Options.Average_Connections,
      Reset_Seed          => Concorde.Options.Randomise,
      Name_Generator      => Name_Generator);

   Concorde.Empires.Configure.Create_Empires
     (Count => Concorde.Options.Number_Of_Empires);

   if Concorde.Options.Enable_Empire_Logging then
      Concorde.Empires.Logging.Start_Logging;
   end if;

   Concorde.Agents.Enable_Offer_Logging (Enabled => True);
   Concorde.Logging.Start_Logging;

   Concorde.Empires.Updates.Start;

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

      Concorde.Gtk_UI.Start;

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
