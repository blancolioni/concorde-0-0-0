with Ada.Strings.Fixed;

package body Concorde.Dates is

   protected Calendar is
      function Current_Date return Date_Type;
      procedure Tick;
   private
      Current : Date_Type := 1;
   end Calendar;

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

      procedure Tick is
      begin
         Current := Current + 1;
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

   ----------
   -- Tick --
   ----------

   procedure Tick is
   begin
      Calendar.Tick;
   end Tick;

   ---------------
   -- To_String --
   ---------------

   function To_String (Date : Date_Type) return String is
      use Ada.Strings, Ada.Strings.Fixed;

      Date_Index : constant Positive := Positive (Date);
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
