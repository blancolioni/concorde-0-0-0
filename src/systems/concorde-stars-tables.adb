with Ada.Containers.Doubly_Linked_Lists;

with Tropos.Reader;
with Tropos.Writer;

with Concorde.Paths;

package body Concorde.Stars.Tables is

   type Star_Info_Record is
      record
         Solar_Masses : Non_Negative_Real;
         Class        : Stellar_Class_Type;
         Subclass     : Stellar_Subclass_Type;
         Surface_Temp : Non_Negative_Real;
         Colour       : Lui.Colours.Colour_Type;
         Radius       : Non_Negative_Real;
         Luminosity   : Non_Negative_Real;
      end record;

   package Star_Info_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Star_Info_Record);

   Main_Sequence_Table : Star_Info_Lists.List;

   procedure Read_Tables;

   ----------------------------
   -- Get_Main_Sequence_Info --
   ----------------------------

   procedure Get_Main_Sequence_Info
     (Solar_Masses : Non_Negative_Real;
      Class        : out Stellar_Class_Type;
      Subclass     : out Stellar_Subclass_Type;
      Radius       : out Non_Negative_Real;
      Luminosity   : out Non_Negative_Real;
      Colour       : out Lui.Colours.Colour_Type)
   is
      use Star_Info_Lists;
      Position : Cursor;
   begin

      if Main_Sequence_Table.Is_Empty then
         Read_Tables;
      end if;

      Position := Main_Sequence_Table.First;

      while Has_Element (Position) loop
         declare
            Info : constant Star_Info_Record := Element (Position);
         begin
            exit when Solar_Masses >= Info.Solar_Masses;
         end;
         Next (Position);
      end loop;

      if not Has_Element (Position) then
         Position := Main_Sequence_Table.Last;
      end if;

      declare
         Info : Star_Info_Record renames Element (Position);
      begin
         Class := Info.Class;
         Subclass := Info.Subclass;
         Radius := Info.Radius;
         Luminosity := Info.Luminosity;
         Colour := Info.Colour;
      end;

   end Get_Main_Sequence_Info;

   procedure Read_Tables is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_CSV_Config
                   (Concorde.Paths.Config_File
                      ("star-systems/star-classification.txt"),
                    Header_Line   => True,
                    Separator     => ';',
                    Extend_Header => False);
   begin
      Tropos.Writer.Write_Config (Config, "test.txt");
      for Info_Config of Config loop
         declare
            Class        : constant String := Info_Config.Get ("type");
            Surface_Temp : constant Real := Info_Config.Get ("surface-temp");
            Radius       : constant Real := Info_Config.Get ("radius");
            Mass         : constant Real := Info_Config.Get ("mass");
            Luminosity   : constant Real := Info_Config.Get ("luminosity");
            Red          : constant Natural := Info_Config.Get ("r");
            Green        : constant Natural := Info_Config.Get ("g");
            Blue         : constant Natural := Info_Config.Get ("b");
            Colour       : constant Lui.Colours.Colour_Type :=
                             Lui.Colours.To_Colour
                               (Lui.Colours.Colour_Byte (Red),
                                Lui.Colours.Colour_Byte (Green),
                                Lui.Colours.Colour_Byte (Blue));
            Info         : constant Star_Info_Record :=
                             (Solar_Masses => Mass,
                              Class        =>
                                Stellar_Class_Type'Value
                                  ((1 => Class (Class'First))),
                              Subclass     =>
                                Stellar_Subclass_Type'Value
                                  ((1 => Class (Class'First + 1))),
                              Surface_Temp => Surface_Temp,
                              Colour       => Colour,
                              Radius       => Radius,
                              Luminosity   => Luminosity);
         begin
            Main_Sequence_Table.Append (Info);
         end;
      end loop;
   end Read_Tables;

end Concorde.Stars.Tables;
