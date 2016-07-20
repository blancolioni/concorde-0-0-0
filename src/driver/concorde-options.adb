with Ada.Command_Line;
with Ada.Strings.Fixed;

with Tropos.Reader;
with Concorde.Paths;

package body Concorde.Options is

   Option_Config : Tropos.Configuration;
   Got_Config    : Boolean := False;

   function With_Underscores (S : String) return String;

   procedure Load_Options;

   function String_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : String := "")
      return String;

   function Integer_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : Integer := 0)
      return Integer;

   function Real_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : Real := 0.0)
      return Real;

   -------------------------
   -- Average_Connections --
   -------------------------

   function Average_Connections return Positive is
   begin
      return Integer_Value ("average_connections", 'x');
   end Average_Connections;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : Boolean := False)
      return Boolean
   is
      use Ada.Strings.Fixed;
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Argument : constant String :=
                         Ada.Command_Line.Argument (I);
         begin
            if Argument'Length > 2
              and then Argument (1 .. 2) = "--"
              and then Index (Argument, "=") = 0
              and then Argument (3 .. Argument'Last) = Long_Name
            then
               return True;
            elsif Argument'Length > 5
              and then Argument (1 .. 5) = "--no-"
              and then Index (Argument, "=") = 0
              and then Argument (6 .. Argument'Last) = Long_Name
            then
               return False;
            elsif Short_Name /= Character'Val (0)
              and then Argument (1) = '-'
              and then Argument'Length > 1
              and then Argument (2) /= '-'
            then
               for J in 2 .. Argument'Last loop
                  if Argument (J) = Short_Name then
                     return not Default;
                  end if;
               end loop;
            end if;
         end;
      end loop;

      Load_Options;

      if Option_Config.Contains (Long_Name) then
         return Option_Config.Get (Long_Name);
      elsif Option_Config.Contains (With_Underscores (Long_Name)) then
         return Option_Config.Get (With_Underscores (Long_Name));
      else
         return Default;
      end if;

   end Boolean_Value;

   ----------------------
   -- Check_Invariants --
   ----------------------

   function Check_Invariants return Boolean is
   begin
      return Boolean_Value ("check-invariants");
   end Check_Invariants;

   -------------
   -- Console --
   -------------

   function Console return Boolean is
   begin
      return Boolean_Value ("console", 'c');
   end Console;

   ----------------------
   -- Display_Language --
   ----------------------

   function Display_Language return String is
   begin
      return String_Value ("display-language", 'L', "english");
   end Display_Language;

   ------------------------------------
   -- Enable_Detailed_Battle_Logging --
   ------------------------------------

   function Enable_Detailed_Battle_Logging return Boolean is
   begin
      return Boolean_Value ("detailed-battle-logging", 'B');
   end Enable_Detailed_Battle_Logging;

   ---------------------------
   -- Enable_Empire_Logging --
   ---------------------------

   function Enable_Empire_Logging return Boolean is
   begin
      return Boolean_Value ("empire-logging", 'E');
   end Enable_Empire_Logging;

   ---------------------------
   -- Enable_Market_Logging --
   ---------------------------

   function Enable_Market_Logging return Boolean is
   begin
      return Boolean_Value ("market-logging");
   end Enable_Market_Logging;

   ------------------
   -- Galaxy_Shape --
   ------------------

   function Galaxy_Shape return String is
   begin
      return String_Value ("galaxy-shape", 'g', "spiral");
   end Galaxy_Shape;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : Integer := 0)
      return Integer
   is
   begin
      return Integer'Value (String_Value (Long_Name, Short_Name,
                            Integer'Image (Default)));
   exception
      when Constraint_Error =>
         return Default;
   end Integer_Value;

   ------------------
   -- Load_Options --
   ------------------

   procedure Load_Options is
   begin
      if not Got_Config then
         Option_Config :=
           Tropos.Reader.Read_Config
             (Concorde.Paths.Config_File ("options.txt"));
         Got_Config := True;
      end if;
   end Load_Options;

   -----------------------------
   -- Minimum_Size_For_Battle --
   -----------------------------

   function Minimum_Size_For_Battle return Natural is
   begin
      return Integer_Value ("minimum-battle-size", 'M', 10);
   end Minimum_Size_For_Battle;

   -----------------------
   -- Number_Of_Empires --
   -----------------------

   function Number_Of_Empires return Positive is
   begin
      return Integer_Value ("empire-count", 'e', 99);
   end Number_Of_Empires;

   -----------------------
   -- Number_Of_Systems --
   -----------------------

   function Number_Of_Systems return Positive is
   begin
      return Integer_Value ("system-count", 's', 500);
   end Number_Of_Systems;

   -----------------------
   -- Number_Of_Updates --
   -----------------------

   function Number_Of_Updates return Natural is
   begin
      return Integer_Value ("update-count", 'u', 500);
   end Number_Of_Updates;

   ---------------
   -- Randomise --
   ---------------

   function Randomise return Boolean is
   begin
      return Boolean_Value ("randomise", 'R');
   end Randomise;

   ----------------
   -- Real_Value --
   ----------------

   function Real_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : Real := 0.0)
      return Real
   is
   begin
      return Real'Value
        (String_Value
           (Long_Name, Short_Name,
            Real'Image (Default)));
   exception
      when Constraint_Error =>
         return Default;
   end Real_Value;

   --------------
   -- Scenario --
   --------------

   function Scenario return String is
   begin
      return String_Value ("scenario", 'S');
   end Scenario;

   ------------------------
   -- Show_Battle_Screen --
   ------------------------

   function Show_Battle_Screen return Boolean is
   begin
      return Boolean_Value ("battle-screen");
   end Show_Battle_Screen;

   ------------------------
   -- Show_Capital_Names --
   ------------------------

   function Show_Capital_Names return Boolean is
   begin
      return Boolean_Value ("show-capital-names", 'N', Default => True);
   end Show_Capital_Names;

   -----------------------
   -- Show_System_Names --
   -----------------------

   function Show_System_Names return Boolean is
   begin
      return Boolean_Value ("show-system-names", 'n', Default => False);
   end Show_System_Names;

   ------------------
   -- String_Value --
   ------------------

   function String_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : String := "")
      return String
   is
      use Ada.Strings.Fixed;
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Argument : constant String :=
                         Ada.Command_Line.Argument (I);
         begin
            if Argument'Length > 2
              and then Argument (1 .. 2) = "--"
              and then Index (Argument, "=") > 0
            then
               declare
                  Name : constant String :=
                           Argument (3 .. Index (Argument, "=") - 1);
               begin
                  if Name = Long_Name then
                     return Argument (Index (Argument, "=") + 1
                                      .. Argument'Last);
                  end if;
               end;
            elsif Short_Name /= Character'Val (0)
              and then Argument (1) = '-'
              and then Argument'Length = 2
              and then Argument (2) = Short_Name
              and then I < Ada.Command_Line.Argument_Count
            then
               return Ada.Command_Line.Argument (I + 1);
            end if;
         end;
      end loop;

      Load_Options;

      if Option_Config.Contains (Long_Name) then
         return Option_Config.Get (Long_Name);
      elsif Option_Config.Contains (With_Underscores (Long_Name)) then
         return Option_Config.Get (With_Underscores (Long_Name));
      else
         return Default;
      end if;

   end String_Value;

   ------------------------
   -- System_X_Deviation --
   ------------------------

   function System_X_Deviation return Real is
   begin
      return Real_Value ("system-x-deviation", Default => 0.0);
   end System_X_Deviation;

   ------------------------
   -- System_Y_Deviation --
   ------------------------

   function System_Y_Deviation return Real is
   begin
      return Real_Value ("system-y-deviation", Default => 0.0);
   end System_Y_Deviation;

   ------------------------
   -- System_Z_Deviation --
   ------------------------

   function System_Z_Deviation return Real is
   begin
      return Real_Value ("system-z-deviation", Default => 0.0);
   end System_Z_Deviation;

   -----------------
   -- Test_Battle --
   -----------------

   function Test_Battle return Boolean is
   begin
      return Boolean_Value ("test-battle", 'B', Default => False);
   end Test_Battle;

   ----------------------
   -- With_Underscores --
   ----------------------

   function With_Underscores (S : String) return String is
      Result : String := S;
   begin
      for I in Result'Range loop
         if Result (I) = '-' then
            Result (I) := '_';
         end if;
      end loop;
      return Result;
   end With_Underscores;

   ------------------
   -- Work_Threads --
   ------------------

   function Work_Threads return Positive is
   begin
      return Integer_Value ("work-threads", 't', 4);
   end Work_Threads;

   -------------------------------
   -- World_Continent_Smoothing --
   -------------------------------

   function World_Continent_Smoothing return Positive is
   begin
      return Integer_Value ("world-continent-smoothing", Default => 4);
   end World_Continent_Smoothing;

   -------------------------
   -- World_Detail_Factor --
   -------------------------

   function World_Detail_Factor return Positive is
   begin
      return Integer_Value ("world-detail-factor", Default => 20);
   end World_Detail_Factor;

   ------------------------------
   -- World_Fractal_Iterations --
   ------------------------------

   function World_Fractal_Iterations return Positive is
   begin
      return Integer_Value ("world-fractal-iterations", Default => 1000);
   end World_Fractal_Iterations;

   ----------------------------
   -- World_Height_Smoothing --
   ----------------------------

   function World_Height_Smoothing return Positive is
   begin
      return Integer_Value ("world-height-smoothing", Default => 4);
   end World_Height_Smoothing;

   -----------------------
   -- World_Sector_Size --
   -----------------------

   function World_Sector_Size return Positive is
   begin
      return Integer_Value ("world-sector-size", Default => 1400);
   end World_Sector_Size;

   --------------------
   -- Write_Accounts --
   --------------------

   function Write_Accounts return Boolean is
   begin
      return Boolean_Value ("write-accounts");
   end Write_Accounts;

end Concorde.Options;
