package Concorde.Paths is

   Config_Path : constant String :=
     "C:\Users\Fraser\Documents\kiln\2-concorde\config";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end Concorde.Paths;
