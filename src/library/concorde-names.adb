with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with WL.Random;

with Concorde.Paths;

package body Concorde.Names is

   package Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   function Random_Name
     (Vector : Name_Vectors.Vector)
      return String;

   Female_First_Names : Name_Vectors.Vector;
   Male_First_Names   : Name_Vectors.Vector;
   Last_Names : Name_Vectors.Vector;

   ---------------------
   -- Configure_Names --
   ---------------------

   procedure Configure_Names is

      procedure Configure
        (File_Name : String;
         Vector    : in out Name_Vectors.Vector);

      ---------------
      -- Configure --
      ---------------

      procedure Configure
        (File_Name : String;
         Vector    : in out Name_Vectors.Vector)
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Open (File, In_File,
               Concorde.Paths.Config_File
                 ("names/" & File_Name));
         while not End_Of_File (File) loop
            declare
               Line : constant String := Get_Line (File);
               Space : constant Natural :=
                         Ada.Strings.Fixed.Index (Line, " ");
            begin
               if Space = 0 then
                  Vector.Append (Line);
               else
                  Vector.Append (Line (1 .. Space - 1));
               end if;
            end;
         end loop;
      end Configure;

   begin
      Configure ("female-first-names.txt", Female_First_Names);
      Configure ("male-first-names.txt", Male_First_Names);
      Configure ("last_names.txt", Last_Names);

   end Configure_Names;

   ------------------------------
   -- Random_Female_First_Name --
   ------------------------------

   function Random_Female_First_Name return String is
   begin
      return Random_Name (Female_First_Names);
   end Random_Female_First_Name;

   -----------------------
   -- Random_First_Name --
   -----------------------

   function Random_First_Name return String is
   begin
      if WL.Random.Random_Number (1, 2) = 1 then
         return Random_Name (Female_First_Names);
      else
         return Random_Name (Male_First_Names);
      end if;
   end Random_First_Name;

   ----------------------
   -- Random_Last_Name --
   ----------------------

   function Random_Last_Name return String is
   begin
      return Random_Name (Last_Names);
   end Random_Last_Name;

   function Random_Male_First_Name return String is
   begin
      return Random_Name (Male_First_Names);
   end Random_Male_First_Name;

   -----------------
   -- Random_Name --
   -----------------

   function Random_Name
     (Vector : Name_Vectors.Vector)
      return String
   is
      Index : constant Positive :=
                WL.Random.Random_Number (1, Vector.Last_Index);
   begin
      return Vector.Element (Index);
   end Random_Name;

end Concorde.Names;
