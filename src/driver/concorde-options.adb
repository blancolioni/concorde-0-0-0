with WL.Command_Line;

package body Concorde.Options is

   pragma Style_Checks (Off);

   function Scenario return String is
   begin
      return WL.Command_Line.Find_Option
               ("scenario", ' ');
   end Scenario;

   function Display_Language return String is
   begin
      return WL.Command_Line.Find_Option
               ("display-language", ' ');
   end Display_Language;

   function Faction_Name return String is
   begin
      return WL.Command_Line.Find_Option
               ("faction-name", ' ');
   end Faction_Name;

   function Console return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("console", ' ');
   end Console;

   function Randomise return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("randomise", ' ');
   end Randomise;

   function Create_Voronoi return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("create-voronoi", ' ');
   end Create_Voronoi;

   function Create_Galaxy return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("create-galaxy", ' ');
   end Create_Galaxy;

   function Create_Factions return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("create-factions", ' ');
   end Create_Factions;

   function Faction_Count return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("faction-count", ' ', 0);
   end Faction_Count;

   function System_Count return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("system-count", ' ', 0);
   end System_Count;

   function Initial_Trade_Ships return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("initial-trade-ships", ' ', 0);
   end Initial_Trade_Ships;

   function Update_Count return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("update-count", ' ', 0);
   end Update_Count;

   function Galaxy_Shape return String is
   begin
      return WL.Command_Line.Find_Option
               ("galaxy-shape", ' ');
   end Galaxy_Shape;

   function System_X_Deviation return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("system-x-deviation", ' ', 0);
   end System_X_Deviation;

   function System_Y_Deviation return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("system-y-deviation", ' ', 0);
   end System_Y_Deviation;

   function System_Z_Deviation return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("system-z-deviation", ' ', 0);
   end System_Z_Deviation;

   function Average_Connections return Positive is
   begin
      return WL.Command_Line.Find_Option
               ("average-connections", ' ');
   end Average_Connections;

   function Full_Screen return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("full-screen", ' ');
   end Full_Screen;

   function Display_Width return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("display-width", ' ', 0);
   end Display_Width;

   function Display_Height return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("display-height", ' ', 0);
   end Display_Height;

   function Work_Threads return Positive is
   begin
      return WL.Command_Line.Find_Option
               ("work-threads", ' ');
   end Work_Threads;

   function Start_With_Galaxy return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("start-with-galaxy", ' ');
   end Start_With_Galaxy;

   function Enable_Faction_Logging return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("enable-faction-logging", ' ');
   end Enable_Faction_Logging;

   function Enable_Market_Logging return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("enable-market-logging", ' ');
   end Enable_Market_Logging;

   function Write_Accounts return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("write-accounts", ' ');
   end Write_Accounts;

   function Test_Battle return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("test-battle", ' ');
   end Test_Battle;

   function Create_Voronoi_Diagram return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("create-voronoi-diagram", ' ');
   end Create_Voronoi_Diagram;

   function Realistic_Star_Masses return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("realistic-star-masses", ' ');
   end Realistic_Star_Masses;

   function World_Detail_Factor return Positive is
   begin
      return WL.Command_Line.Find_Option
               ("world-detail-factor", ' ');
   end World_Detail_Factor;

   function World_Sector_Size return Positive is
   begin
      return WL.Command_Line.Find_Option
               ("world-sector-size", ' ');
   end World_Sector_Size;

   function World_Height_Smoothing return Positive is
   begin
      return WL.Command_Line.Find_Option
               ("world-height-smoothing", ' ');
   end World_Height_Smoothing;

   function Log_Folder return String is
   begin
      return WL.Command_Line.Find_Option
               ("log-folder", ' ');
   end Log_Folder;

   function Detailed_Logging return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("detailed-logging", ' ');
   end Detailed_Logging;

   function Show_Clock_Time return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("show-clock-time", ' ');
   end Show_Clock_Time;

   function Update_Speed return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("update-speed", ' ', 0);
   end Update_Speed;

   function Log_Ship_Movement return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("log-ship-movement", ' ');
   end Log_Ship_Movement;

   function Log_Trade_Offers return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("log-trade-offers", ' ');
   end Log_Trade_Offers;

   function Political_Map_Mode return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("political-map-mode", ' ');
   end Political_Map_Mode;

   function Write_Character_Portraits return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("write-character-portraits", ' ');
   end Write_Character_Portraits;

   function Show_Console_Progress return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("show-console-progress", ' ');
   end Show_Console_Progress;

end Concorde.Options;
