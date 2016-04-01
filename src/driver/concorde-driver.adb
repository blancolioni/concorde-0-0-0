with WL.Processes;
with WL.Random.Names;

with Concorde.Paths;

with Concorde.Components.Configure;
with Concorde.Ships.Designs;

with Concorde.Empires.Configure;
with Concorde.Galaxy.Create;

with Concorde.Galaxy.Locking;

with Concorde.Empires.Reports;
with Concorde.Empires.Updates;
with Concorde.Updates;

with Concorde.Gtk_UI;

with Concorde.Empires.Logging;

with Concorde.Options;

with Concorde.AI.Configure;

with Concorde.People.Groups.Configure;

procedure Concorde.Driver is

   Name_Generator : WL.Random.Names.Name_Generator;
begin

   WL.Random.Names.Load_Lexicon
     (Name_Generator,
      Concorde.Paths.Config_File ("totro-vowels.txt"),
      Concorde.Paths.Config_File ("totro-consonants.txt"));

   if Concorde.Options.Randomise then
      WL.Random.Randomise;
   end if;

   Concorde.People.Groups.Configure.Configure_Pop_Groups;
   Concorde.Components.Configure.Configure_Components;
   Concorde.Ships.Designs.Configure_Designs;

   Concorde.AI.Configure.Register;

   Concorde.Galaxy.Create.Create_Galaxy
     (System_Count        => Concorde.Options.Number_Of_Systems,
      Average_Connections => Concorde.Options.Average_Connections,
      Reset_Seed          => Concorde.Options.Randomise,
      Name_Generator      => Name_Generator);

   Concorde.Empires.Configure.Create_Empires
     (Count => Concorde.Options.Number_Of_Empires);

   if Concorde.Options.Enable_Empire_Logging then
      Concorde.Empires.Logging.Start_Logging;
   end if;

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
              (Execute_Battles => True);
            Process.Tick;
         end loop;
         Process.Finish;
         Concorde.Empires.Reports.Report_Empires;

      end;

   else

      Concorde.Updates.Start_Updates;

      Concorde.Gtk_UI.Start;

      Concorde.Updates.Stop_Updates;

   end if;

   if Concorde.Options.Enable_Empire_Logging then
      Concorde.Empires.Logging.Stop_Logging;
   end if;

   Concorde.Empires.Unload;

exception

   when others =>
      if Concorde.Options.Enable_Empire_Logging then
         Concorde.Empires.Logging.Stop_Logging;
      end if;
      raise;

end Concorde.Driver;
