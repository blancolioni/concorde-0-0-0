package Concorde.Configure is

   procedure Load_Configuration;

   function File_Path
     (Directory : String;
      File_Name : String;
      Extension : String := "txt")
      return String;

   function Directory_Path
     (Directory  : String)
      return String;

end Concorde.Configure;
