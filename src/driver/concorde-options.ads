package Concorde.Options is

   function Average_Connections return Positive;
   function Check_Invariants return Boolean;
   function Console return Boolean;
   function Enable_Detailed_Battle_Logging return Boolean;
   function Enable_Empire_Logging return Boolean;
   function Minimum_Size_For_Battle return Natural;
   function Number_Of_Empires return Positive;
   function Number_Of_Systems return Positive;
   function Number_Of_Updates return Natural;
   function Randomise return Boolean;
   function Show_Capital_Names return Boolean;
   function Show_System_Names return Boolean;

   function Boolean_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : Boolean := False)
      return Boolean;

end Concorde.Options;
