with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with Tropos.Reader;

with Concorde.Paths;

package body Concorde.Stars.Classification is

   type Star_Info_Record is
      record
         Classification : Stellar_Classification;
         Solar_Masses   : Non_Negative_Real;
      end record;

   package Star_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Star_Info_Record);

   Star_Table : Star_Info_Vectors.Vector;

   procedure Read_Star_Tables;
   Star_Tables_Read : Boolean := False;

   --------------------------------
   -- Get_Stellar_Classification --
   --------------------------------

   function Get_Stellar_Classification
     (Name : String)
      return Stellar_Classification
   is
      use Ada.Characters.Handling;
      Result : Stellar_Classification := (G, 2, Main_Sequence);
   begin
      if Name'Length = 0 then
         Ada.Text_IO.Put_Line ("warning: empty spectrum");
         return Result;
      end if;

      if To_Upper (Name (Name'First)) in
        'O' | 'B' | 'A' | 'F' | 'G' | 'K' | 'M' | 'L'
      then
         Result.Class :=
           Stellar_Class_Type'Value (Name (Name'First .. Name'First));
      else
         Ada.Text_IO.Put_Line
           ("warning: unknown spectral class: " & Name);
         return Result;
      end if;

      if Name'Length >= 2 then
         if Name (Name'First + 1) in '0' .. '9' then
            Result.Subclass :=
              Stellar_Subclass_Type'Value
                (Name (Name'First + 1 .. Name'First + 1));
         else
            Ada.Text_IO.Put_Line
              ("warning: unknown spectral subclass: " & Name);
            return Result;
         end if;
         if Name'Length = 2 then
            return Result;
         end if;

      end if;

      if Name'Length >= 3 then
         declare
            Size : constant String :=
                     To_Upper (Name (Name'First + 2 .. Name'Last));
         begin
            if Size = "I" then
               Result.Size := Supergiant;
            elsif Size = "III" then
               Result.Size := Giant;
            elsif Size = "V" then
               Result.Size := Main_Sequence;
            else
               Ada.Text_IO.Put_Line
                 ("warning: unknown size " & Size & " in name: " & Name);
            end if;
         end;
      end if;

      return Result;

   end Get_Stellar_Classification;

   ----------------------
   -- Read_Star_Tables --
   ----------------------

   procedure Read_Star_Tables is
      Main_Sequence_Config : Tropos.Configuration :=
                               Tropos.Reader.Read_CSV_Config
                                 (Concorde.Paths.Config_File
                                    ("star-systems/main-sequence.csv"),
                                  True, ',', False);
      Giant_Config         : Tropos.Configuration :=
                               Tropos.Reader.Read_CSV_Config
                                 (Concorde.Paths.Config_File
                                    ("star-systems/giant.csv"),
                                  True, ',', False);
      Supergiant_Config    : Tropos.Configuration :=
                               Tropos.Reader.Read_CSV_Config
                                 (Concorde.Paths.Config_File
                                    ("star-systems/supergiant.csv"),
                                  True, ',', False);
      pragma Unreferenced (Giant_Config, Supergiant_Config);
   begin
      for Config of Main_Sequence_Config loop
         declare
            Info : Star_Info_Record;
            Spectral_Type : constant String := Config.Get ("Spectral_Type");
            Mass          : constant Float := Config.Get ("Mass");
         begin
            Info.Classification.Class :=
              Stellar_Class_Type'Value (Spectral_Type (1 .. 1));
            Info.Classification.Subclass :=
              Stellar_Subclass_Type'Value (Spectral_Type (2 .. 2));
            Info.Classification.Size := Main_Sequence;
            Info.Solar_Masses := Real (Mass);
            Star_Table.Append (Info);
         end;
      end loop;
      Star_Tables_Read := True;
   end Read_Star_Tables;

   ------------------
   -- Solar_Masses --
   ------------------

   function Solar_Masses
     (Classification : Stellar_Classification)
      return Non_Negative_Real
   is
      Last : Star_Info_Record := ((G, 2, Main_Sequence), 1.0);
   begin
      if not Star_Tables_Read then
         Read_Star_Tables;
      end if;

      for Info of Star_Table loop
         if Info.Classification.Class = Classification.Class
           and then Info.Classification.Subclass <= Classification.Subclass
           and then Info.Classification.Size = Classification.Size
         then
            Last := Info;
         end if;
      end loop;
      return Last.Solar_Masses;
   end Solar_Masses;

end Concorde.Stars.Classification;
