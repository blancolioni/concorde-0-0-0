with Tropos.Reader;
with Concorde.Paths;

with Concorde.AI.Configure;

with Concorde.Empires.Create;

with Lui.Colours;

package body Concorde.Empires.Configure is

   --------------------
   -- Create_Empires --
   --------------------

   procedure Create_Empires (Count : Natural) is
      Empire_Config : constant Tropos.Configuration :=
                        Tropos.Reader.Read_Config
                          (Concorde.Paths.Config_File ("empires.txt"));
      Current       : Natural := 0;
   begin
      for Config of Empire_Config loop
         exit when Current >= Count;
         declare
            Name : constant String :=
                     Config.Get ("name", Config.Config_Name);
            Capital : constant String :=
                        Config.Get ("capital", Name);
            Colour  : constant Tropos.Configuration :=
                        Config.Child ("colour");
            Red     : constant Natural := Colour.Get (1);
            Green   : constant Natural := Colour.Get (2);
            Blue    : constant Natural := Colour.Get (3);
            AI_Name : constant String := Config.Get ("ai", "default");
            AI      : constant Concorde.AI.AI_Type :=
                        Concorde.AI.Configure.Get_AI (AI_Name);
         begin
            Concorde.Empires.Create.New_Empire
              (Name    => Name,
               Capital => Capital,
               Colour  =>
                 Lui.Colours.To_Colour
                   (Lui.Colours.Colour_Byte (Red),
                    Lui.Colours.Colour_Byte (Green),
                    Lui.Colours.Colour_Byte (Blue)),
               AI      => AI);
         end;

         Current := Current + 1;

      end loop;
   end Create_Empires;

end Concorde.Empires.Configure;
