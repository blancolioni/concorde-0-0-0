package Concorde.Dates is

   type Date_Type is private;

   Zero_Date : constant Date_Type;

   function Current_Date return Date_Type;
   function Current_Date_To_String return String;

   function Add_Seconds
     (Day     : Date_Type;
      Seconds : Float)
      return Date_Type;

   procedure Tick (Simulation_Seconds : Duration);

   function To_String (Date : Date_Type) return String;

   function To_Date_And_Time_String (Date : Date_Type) return String;

   function "-" (Left, Right : Date_Type) return Duration;

   function "<" (Left, Right : Date_Type) return Boolean;
   function "<=" (Left, Right : Date_Type) return Boolean;
   function ">" (Left, Right : Date_Type) return Boolean;
   function ">=" (Left, Right : Date_Type) return Boolean;

   function Elapsed_Seconds
     return Non_Negative_Real;

   type Day_Index is new Positive;

   function Get_Day (Date : Date_Type) return Day_Index;

private

   type Date_Type is new Long_Float;

   Zero_Date : constant Date_Type := 0.0;

   function Add_Seconds
     (Day     : Date_Type;
      Seconds : Float)
      return Date_Type
   is (Day + Date_Type (Seconds));

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");

end Concorde.Dates;
