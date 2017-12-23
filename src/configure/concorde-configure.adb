with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Text_IO;

with Concorde.Names;

with Concorde.Components.Configure;
with Concorde.Ships.Designs;

with Concorde.Commodities.Configure;
with Concorde.Facilities.Configure;
with Concorde.People.Groups.Configure;

with Concorde.Atmosphere.Configure;
with Concorde.Features.Configure;
with Concorde.Terrain.Configure;

with Concorde.Options;
with Concorde.Scenarios;

with Concorde.Localisation;

with Concorde.Paths;

package body Concorde.Configure is

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   Config_Search_Path : String_Lists.List;
   Got_Search_Path : Boolean := False;

   procedure Check_Search_Paths;

   ------------------------
   -- Check_Search_Paths --
   ------------------------

   procedure Check_Search_Paths is
   begin
      if Got_Search_Path then
         return;
      end if;

      if Concorde.Options.Scenario /= "" then
         Config_Search_Path.Append
           (Concorde.Paths.Config_File
              ("scenarios/" & Concorde.Options.Scenario));
      end if;

      Config_Search_Path.Append
        (Concorde.Paths.Config_Path);
      Got_Search_Path := True;

   end Check_Search_Paths;

   --------------------
   -- Directory_Path --
   --------------------

   function Directory_Path
     (Directory  : String)
      return String
   is
   begin
      Check_Search_Paths;
      for Path of Config_Search_Path loop
         declare
            Directory_Path : constant String :=
                               Path & "/" & Directory;
         begin
            if Ada.Directories.Exists (Directory_Path) then
               Ada.Text_IO.Put_Line
                 ("reading: " & Directory_Path);
               return Directory_Path;
            end if;
         end;
      end loop;
      raise Constraint_Error
        with "config directory not found: " & Directory;

   end Directory_Path;

   ---------------
   -- File_Path --
   ---------------

   function File_Path
     (Directory : String;
      File_Name : String;
      Extension : String := "txt")
      return String
   is
   begin
      Check_Search_Paths;
      for Path of Config_Search_Path loop
         declare
            File_Path : constant String :=
                          Path & "/"
                          & Directory & "/"
                          & File_Name & "." & Extension;
         begin
            if Ada.Directories.Exists (File_Path) then
               Ada.Text_IO.Put_Line
                 ("reading: " & File_Path);
               return File_Path;
            end if;
         end;
      end loop;

      raise Constraint_Error with
        "config file not found: "
        & Directory & "/" & File_Name & "." & Extension;
   end File_Path;

   ------------------------
   -- Load_Configuration --
   ------------------------

   procedure Load_Configuration is
   begin
      Concorde.Localisation.Load_Localisation
        (Concorde.Options.Display_Language);

      Concorde.Names.Configure_Names;

      Concorde.Scenarios.Load_Scenario
        (Concorde.Options.Scenario);

      Concorde.Terrain.Configure.Configure_Terrain;
      Concorde.Features.Configure.Configure_Features;

      Concorde.Atmosphere.Configure.Configure_Atmosphere;

      Concorde.Commodities.Configure.Configure_Commodities;
      Concorde.People.Groups.Configure.Configure_Pop_Groups;

      Concorde.Components.Configure.Configure_Components;
      Concorde.Facilities.Configure.Configure_Facilities;
      Concorde.Ships.Designs.Configure_Designs;

      Concorde.Commodities.Configure.Calculate_Base_Prices;

   end Load_Configuration;

end Concorde.Configure;
