with WL.Processes;
with WL.Random;

with Lui.Colours;

with Concorde.Empires.Create;
with Concorde.Galaxy.Create;

with Concorde.Galaxy.Locking;

with Concorde.Empires.Reports;
with Concorde.Empires.Updates;
with Concorde.Updates;

with Concorde.Gtk_UI;

with Concorde.Empires.Logging;

with Concorde.AI.Defensive;

procedure Concorde.Driver is
   Randomise       : constant Boolean := False;
   Small           : constant Boolean := False;
   Small_Size      : constant Positive := 40;
   Large_Size      : constant Positive := 400;
   Ave_Connections : constant Positive := 4;
   Enable_Logging  : constant Boolean  := True;
   Show_GUI        : constant Boolean := True;
   Update_Count    : constant Natural := 200;
begin

   if Randomise then
      WL.Random.Randomise;
   end if;

   if Small then

      Concorde.Galaxy.Create.Create_Galaxy
        (System_Count        => Small_Size,
         Average_Connections => Ave_Connections,
         Reset_Seed          => Randomise);

      Concorde.Empires.Create.New_Empire
        (Name   => "Byzantium", Capital => "Constantinople",
         Colour => Lui.Colours.To_Colour (102, 2, 60));

      Concorde.Empires.Create.New_Empire
        (Name   => "Mallorea", Capital => "Mal Zeth",
         Colour => Lui.Colours.To_Colour (30, 144, 255));

      Concorde.Empires.Create.New_Empire
        (Name   => "Argan", Capital => "Harvest Plains",
         Colour => Lui.Colours.To_Colour (255, 144, 30));

   else
      Concorde.Galaxy.Create.Create_Galaxy
        (System_Count        => Large_Size,
         Average_Connections => Ave_Connections,
         Reset_Seed          => Randomise);

      Concorde.Empires.Create.New_Empire
        (Name   => "Byzantium", Capital => "Constantinople",
         Colour => Lui.Colours.To_Colour (102, 2, 60),
         AI     => Concorde.AI.Defensive.Defensive_AI);

      Concorde.Empires.Create.New_Empire
        (Name   => "Mallorea", Capital => "Mal Zeth",
         Colour => Lui.Colours.To_Colour (30, 144, 255));

      Concorde.Empires.Create.New_Empire
        (Name   => "Argan", Capital => "Harvest Plains",
         Colour => Lui.Colours.To_Colour (255, 144, 30));

      Concorde.Empires.Create.New_Empire
        (Name    => "ITC", Capital => "New York",
         Colour => Lui.Colours.To_Colour (144, 255, 30));

      Concorde.Empires.Create.New_Empire
        (Name   => "Holy Roman Empire", Capital => "New Vienna",
         Colour => Lui.Colours.To_Colour (160, 160, 0));

      Concorde.Empires.Create.New_Empire
        (Name   => "Caledonia", Capital => "Dublin",
         Colour => Lui.Colours.To_Colour (0, 160, 0));

      Concorde.Empires.Create.New_Empire
        (Name   => "The Hegemony", Capital => "Tau Ceti Centre",
         Colour => Lui.Colours.To_Colour (0, 0, 160));

      Concorde.Empires.Create.New_Empire
        (Name   => "Macedonia", Capital => "Alexandria",
         Colour => Lui.Colours.To_Colour (106, 74, 60));

      Concorde.Empires.Create.New_Empire
        (Name   => "Melnibone", Capital => "Imrryr",
         Colour => Lui.Colours.To_Colour (84, 121, 128));

      Concorde.Empires.Create.New_Empire
        (Name   => "Rome", Capital => "Rome",
         Colour => Lui.Colours.To_Colour (192, 41, 66));

      Concorde.Empires.Create.New_Empire
        (Name   => "The Council", Capital => "Citidel Station",
         Colour => Lui.Colours.To_Colour (85, 98, 112));

      Concorde.Empires.Create.New_Empire
        (Name   => "Rovac", Capital => "Rovac",
         Colour => Lui.Colours.To_Colour (229, 252, 194));

      Concorde.Empires.Create.New_Empire
        (Name   => "Morder", Capital => "Mount Doom",
         Colour => Lui.Colours.To_Colour (89, 79, 79));

   end if;

   if Enable_Logging then
      Concorde.Empires.Logging.Start_Logging;
   end if;

   Concorde.Empires.Updates.Start;

   if Show_GUI then
      Concorde.Updates.Start_Updates;

      Concorde.Gtk_UI.Start;

      Concorde.Updates.Stop_Updates;
   else
      declare
         Process : WL.Processes.Process_Type;
      begin
         Process.Start_Percentage
           ("Updating",
            Finish => Update_Count);

         Concorde.Galaxy.Locking.Init_Locking;

         for I in 1 .. Update_Count loop
            Concorde.Updates.Perform_Update;
            Process.Tick;
         end loop;
         Process.Finish;
         Concorde.Empires.Reports.Report_Empires;

      end;
   end if;

   if Enable_Logging then
      Concorde.Empires.Logging.Stop_Logging;
   end if;

   Concorde.Empires.Unload;

end Concorde.Driver;
