with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Text_IO;

with WL.Localisation;

with Tropos.Reader;

with Concorde.Names;

with Concorde.Ships.Components.Configure;
with Concorde.Ships.Designs.Configure;

with Concorde.Commodities.Configure;
--  with Concorde.Facilities.Configure;
with Concorde.Metrics.Configure;
with Concorde.Offices.Configure;
with Concorde.Powers.Configure;
with Concorde.Policies.Configure;

with Concorde.People.Careers.Configure;
with Concorde.People.Groups.Configure;
with Concorde.People.Skills.Configure;
with Concorde.People.Traits.Configure;

with Concorde.People.Individuals.Portraits;
with Concorde.People.Genetics;

--  with Concorde.Units.Configure;

with Concorde.Atmosphere.Configure;
with Concorde.Features.Configure;
with Concorde.Terrain.Configure;

with Concorde.Options;
with Concorde.Scenarios;

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

      procedure Read_Localisation
        (Item : Ada.Directories.Directory_Entry_Type);

      -----------------------
      -- Read_Localisation --
      -----------------------

      procedure Read_Localisation
        (Item : Ada.Directories.Directory_Entry_Type)
      is
      begin
         Ada.Text_IO.Put_Line
           ("Reading localisation from: " & Ada.Directories.Full_Name (Item));
         WL.Localisation.Read_Localisation
           (Ada.Directories.Full_Name (Item));
      end Read_Localisation;

   begin

      Ada.Directories.Search
        (Directory =>
           Concorde.Paths.Config_File
             ("localisation/" & Concorde.Options.Display_Language),
         Pattern   => "*.txt",
         Process   => Read_Localisation'Access);

      Concorde.Names.Configure_Names;

      Concorde.Scenarios.Load_Scenario
        (Concorde.Options.Scenario);

      Concorde.Terrain.Configure.Configure_Terrain;
      Concorde.Features.Configure.Configure_Features;

      Concorde.Atmosphere.Configure.Configure_Atmosphere;

      Concorde.Commodities.Configure.Configure_Commodities;

      Concorde.People.Groups.Configure.Configure_Pop_Groups;

      Concorde.People.Skills.Configure.Configure_Skills
        (Tropos.Reader.Read_Config
           (Path      => Directory_Path ("skills"),
            Extension => "txt"));

      Concorde.People.Careers.Configure.Configure_Careers
        (Tropos.Reader.Read_Config
           (Path      => Directory_Path ("careers"),
            Extension => "txt"));

      Concorde.Offices.Configure.Configure_Offices
        (Tropos.Reader.Read_Config
           (Path      => Directory_Path ("offices"),
            Extension => "txt"));

      Tropos.Reader.Read_Config
        (Path      => Directory_Path ("traits"),
         Extension => "txt",
         Configure =>
           Concorde.People.Traits.Configure.Configure_Trait'Access);

      Concorde.Metrics.Configure.Configure_Metrics;
      Concorde.Policies.Configure.Configure_Policies;

      Tropos.Reader.Read_Config
        (Path      => Directory_Path ("powers"),
         Extension => "txt",
         Configure =>
           Concorde.Powers.Configure.Configure_Power_Execution'Access);

--        Tropos.Reader.Read_Config
--          (Path      => Directory_Path ("units"),
--           Extension => "txt",
--           Configure =>
--             Concorde.Units.Configure.Configure_Unit'Access);

      Concorde.Ships.Components.Configure.Configure_Components;
--        Concorde.Facilities.Configure.Configure_Facilities;
      Concorde.Ships.Designs.Configure.Configure_Designs;

      Concorde.Commodities.Configure.Calculate_Base_Prices;

      Concorde.People.Genetics.Configure_Genes
        (Gene_Config =>
           Tropos.Reader.Read_Config
             (File_Path ("genetics", "genes", "txt")));

      Concorde.People.Individuals.Portraits.Configure_Portraits
        (Feature_Config =>
           Tropos.Reader.Read_Config
             (File_Path ("portraits", "portraits", "gfx")),
         Property_Config =>
           Tropos.Reader.Read_Config
             (File_Path ("portraits", "00_portrait_properties", "txt")),
         Sprite_Config  =>
           Tropos.Reader.Read_Config
             (File_Path ("portraits", "portrait_sprites", "gfx")));
   end Load_Configuration;

end Concorde.Configure;
