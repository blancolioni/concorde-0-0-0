package Concorde.Calendar is

   type Time is private;

   subtype Year_Number  is Integer range 1 .. 5001;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number   is Integer range 1 .. 30;

   subtype Day_Duration is Duration range 0.0 .. 86_400.0;

   subtype Hour_Number     is Natural range 0 .. 23;
   subtype Minute_Number   is Natural range 0 .. 59;
   subtype Second_Number   is Natural range 0 .. 59;
   subtype Second_Duration is Day_Duration range 0.0 .. 1.0;

   function Clock return Time;

   function Year    (Date : Time) return Year_Number;
   function Month   (Date : Time) return Month_Number;
   function Day     (Date : Time) return Day_Number;
   function Seconds (Date : Time) return Day_Duration;

   function Hour       (Date : Time) return Hour_Number;
   function Minute     (Date : Time) return Minute_Number;
   function Second     (Date : Time) return Second_Number;
   function Sub_Second (Date : Time) return Second_Duration;

   procedure Split
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration);

   procedure Split
     (Seconds    : Day_Duration;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration);

   procedure Split
     (Date       : Time;
      Year       : out Year_Number;
      Month      : out Month_Number;
      Day        : out Day_Number;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration);

   procedure Split
     (Date          : Time;
      Period        : Duration;
      Cycle_Count   : out Natural;
      Partial_Cycle : out Duration);

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0) return Time;

   function Time_Of
     (Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration := 0.0) return Time;

   function Seconds_Of
     (Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number := 0;
      Sub_Second : Second_Duration := 0.0) return Day_Duration;

   function Image
     (Date                  : Time;
      Include_Time_Fraction : Boolean := False)
      return String;

   function Image
     (Elapsed_Time          : Duration;
      Include_Time_Fraction : Boolean := False) return String;

   function Start_Year return Year_Number;
   function Start return Time;

   procedure Advance (Seconds : Duration);

   function "+" (Left : Time;     Right : Duration) return Time;
   function "+" (Left : Duration; Right : Time)     return Time;
   function "-" (Left : Time;     Right : Duration) return Time;
   function "-" (Left : Time;     Right : Time)     return Duration;

   function "<"  (Left, Right : Time) return Boolean;
   function "<=" (Left, Right : Time) return Boolean;
   function ">"  (Left, Right : Time) return Boolean;
   function ">=" (Left, Right : Time) return Boolean;

   Time_Error : exception;

private

   type Time is range -2 ** 63 .. 2 ** 63 - 1;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");

   Current_Clock : Time := 0;

   function Clock return Time
   is (Current_Clock);

   function Sub_Second (Date : Time) return Second_Duration
   is (0.0);

   function "+" (Left : Time;     Right : Duration) return Time
   is (Left + Time (Right));

   function "+" (Left : Duration; Right : Time)     return Time
   is (Right + Time (Left));

   function "-" (Left : Time;     Right : Duration) return Time
   is (Left - Time (Right));

   function "-" (Left : Time;     Right : Time)     return Duration
   is (Duration (Time'(Left - Right)));

end Concorde.Calendar;