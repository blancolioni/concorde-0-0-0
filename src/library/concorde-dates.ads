package Concorde.Dates is

   type Date_Type is private;

   Zero_Date : constant Date_Type;

   function Current_Date return Date_Type;
   function Current_Date_To_String return String;

   function Add_Seconds
     (Day     : Date_Type;
      Seconds : Real)
      return Date_Type;

   procedure Tick (Simulation_Seconds : Duration);

   function To_String (Date : Date_Type) return String;

   function To_Date_And_Time_String (Date : Date_Type) return String;

   function "-" (Left, Right : Date_Type) return Duration;
   function "+" (Left : Date_Type;
                 Interval : Duration)
                 return Date_Type;

   function "<" (Left, Right : Date_Type) return Boolean;
   function "<=" (Left, Right : Date_Type) return Boolean;
   function ">" (Left, Right : Date_Type) return Boolean;
   function ">=" (Left, Right : Date_Type) return Boolean;

   function Elapsed_Seconds
     return Non_Negative_Real;

   type Day_Index is new Positive;

   function Get_Day (Date : Date_Type) return Day_Index;

   function Relative_Time
     (Start, Finish : Date_Type;
      Current       : Date_Type)
      return Real;

private

   type Date_Type is new Long_Float;

   Zero_Date : constant Date_Type := 0.0;

   function Add_Seconds
     (Day     : Date_Type;
      Seconds : Real)
      return Date_Type
   is (Day + Date_Type (Seconds / 86400.0));

   function "+" (Left     : Date_Type;
                 Interval : Duration)
                 return Date_Type
   is (Left + Date_Type (Interval) / 86400.0);

   function Relative_Time
     (Start, Finish : Date_Type;
      Current       : Date_Type)
      return Real
   is ((Real (Current) - Real (Start)) / (Real (Finish) - Real (Start)));

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");

end Concorde.Dates;
