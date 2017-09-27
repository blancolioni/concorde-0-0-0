with Ada.Calendar;
with Ada.Strings.Fixed;

package body Concorde.Dates is

   protected Calendar is
      function Current_Date return Date_Type;
      procedure Tick (Simulation_Seconds : Duration);
   private
      Current   : Date_Type := 1.0;
   end Calendar;

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Date_Type) return Duration is
   begin
      return Duration (Date_Type'(Left - Right));
   end "-";

   --------------
   -- Calendar --
   --------------

   protected body Calendar is

      ------------------
      -- Current_Date --
      ------------------

      function Current_Date return Date_Type is
      begin
         return Current;
      end Current_Date;

      ----------
      -- Tick --
      ----------

      procedure Tick (Simulation_Seconds : Duration) is
      begin
         Current := Current
           + Date_Type (Simulation_Seconds)
           / Date_Type (Ada.Calendar.Day_Duration'Last);
      end Tick;

   end Calendar;

   ------------------
   -- Current_Date --
   ------------------

   function Current_Date return Date_Type is
   begin
      return Calendar.Current_Date;
   end Current_Date;

   ------------------
   -- Current_Date --
   ------------------

   function Current_Date_To_String return String is
   begin
      return To_String (Current_Date);
   end Current_Date_To_String;

   ---------------------
   -- Elapsed_Seconds --
   ---------------------

   function Elapsed_Seconds
     return Non_Negative_Real
   is
   begin
      return Non_Negative_Real (Current_Date)
        * 24.0 * 60.0 * 60.0;
   end Elapsed_Seconds;

   -------------
   -- Get_Day --
   -------------

   function Get_Day (Date : Date_Type) return Day_Index is
   begin
      return Day_Index (Date);
   end Get_Day;

   ----------
   -- Tick --
   ----------

   procedure Tick (Simulation_Seconds : Duration) is
   begin
      Calendar.Tick (Simulation_Seconds);
   end Tick;

   -----------------------------
   -- To_Date_And_Time_String --
   -----------------------------

   function To_Date_And_Time_String (Date : Date_Type) return String is
      Date_String : constant String := To_String (Date);
      Days        : constant Date_Type := Date - Date_Type'Truncation (Date);
      Hours       : constant Natural :=
                      Natural (Date_Type'Truncation (Days * 24.0));
      Hour_Img    : constant String := Natural'Image (Hours + 100);
      Minutes     : constant Natural :=
                      Natural (Days * 24.0 * 60.0) mod 60;
      Minutes_Img : constant String := Natural'Image (Minutes + 100);
   begin
      return Date_String & " " & Hour_Img (3 .. 4)
        & ":" & Minutes_Img (3 .. 4); --  & ":00";
   end To_Date_And_Time_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Date : Date_Type) return String is
      use Ada.Strings, Ada.Strings.Fixed;

      Date_Index : constant Positive := Positive (Get_Day (Date));
      Year       : constant Positive := 3001 + Date_Index / 360;
      Month      : constant Positive := Date_Index mod 360 / 30 + 1;
      Day        : constant Positive := Date_Index mod 30 + 1;

      function Image (X : Positive;
                      W : Natural)
                      return String;

      -----------
      -- Image --
      -----------

      function Image (X : Positive;
                      W : Natural)
                      return String
      is
         Img : constant String := Trim (Positive'Image (X), Left);
         Zs  : constant String (1 .. W) := (others => '0');
      begin
         if W <= Img'Length then
            return Img;
         else
            return Zs (1 .. W - Img'Length) & Img;
         end if;
      end Image;

   begin
      return Image (Year, 4) & "-" & Image (Month, 2) & "-" & Image (Day, 2);
   end To_String;

end Concorde.Dates;
