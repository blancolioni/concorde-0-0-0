package Concorde.Options is

   function Average_Connections return Positive;
   function Check_Invariants return Boolean;
   function Console return Boolean;
   function Create_Factions return Boolean;
   function Create_Galaxy return Boolean;
   function Create_Voronoi_Diagram return Boolean;
   function Enable_Detailed_Battle_Logging return Boolean;
   function Enable_Faction_Logging return Boolean;
   function Enable_Market_Logging return Boolean;
   function Display_Language return String;
   function Galaxy_Shape return String;
   function Interface_Name return String;
   function Minimum_Size_For_Battle return Natural;
   function Number_Of_Factions return Positive;
   function Number_Of_Systems return Positive;
   function Number_Of_Updates return Natural;
   function Randomise return Boolean;
   function Realistic_Star_Distribution return Boolean;
   function Scenario return String;
   function Show_Battle_Screen return Boolean;
   function Show_Capital_Names return Boolean;
   function Show_System_Names return Boolean;
   function System_X_Deviation return Real;
   function System_Y_Deviation return Real;
   function System_Z_Deviation return Real;
   function Test_Battle return Boolean;
   function Work_Threads return Positive;
   function World_Continent_Smoothing return Positive;
   function World_Detail_Factor return Positive;
   function World_Fractal_Iterations return Positive;
   function World_Height_Smoothing return Positive;
   function World_Sector_Size return Positive;
   function Write_Accounts return Boolean;

   function Boolean_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : Boolean := False)
      return Boolean;

end Concorde.Options;
