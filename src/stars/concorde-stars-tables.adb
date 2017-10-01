with Ada.Containers.Doubly_Linked_Lists;

with Tropos.Reader;
with Tropos.Writer;

with Xi;

with Concorde.Paths;

package body Concorde.Stars.Tables is

   type Star_Info_Record is
      record
         Solar_Masses : Non_Negative_Real;
         Class        : Stellar_Class_Type;
         Subclass     : Stellar_Subclass_Type;
         Surface_Temp : Non_Negative_Real;
         Colour       : Xi.Color.Xi_Color;
         Radius       : Non_Negative_Real;
         Luminosity   : Non_Negative_Real;
      end record;

   function Brighten
     (Color : Xi.Color.Xi_Color;
      Temperature : Xi.Xi_Non_Negative_Float)
      return Xi.Color.Xi_Color;

   package Star_Info_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Star_Info_Record);

   Main_Sequence_Table : Star_Info_Lists.List;

   procedure Read_Tables;

   protected Table_Reader is
      procedure Check_Tables;
   end Table_Reader;

   --------------
   -- Brighten --
   --------------

   function Brighten
     (Color       : Xi.Color.Xi_Color;
      Temperature : Xi.Xi_Non_Negative_Float)
      return Xi.Color.Xi_Color
   is
      use Xi;
      R : constant Xi_Float :=
            Color.Red * Temperature * (0.0534 / 255.0) - (43.0 / 255.0);
      G : constant Xi_Float :=
            Color.Green * Temperature * (0.0628 / 255.0) - (77.0 / 255.0);
      B : constant Xi_Float :=
            Color.Blue * Temperature * (0.0735 / 255.0) - (115.0 / 255.0);
   begin
      return (Clamp (R), Clamp (G), Clamp (B), Color.Alpha);
   end Brighten;

   ----------------------------
   -- Get_Main_Sequence_Info --
   ----------------------------

   procedure Get_Main_Sequence_Info
     (Solar_Masses : Non_Negative_Real;
      Class        : out Stellar_Class_Type;
      Subclass     : out Stellar_Subclass_Type;
      Radius       : out Non_Negative_Real;
      Luminosity   : out Non_Negative_Real;
      Color        : out Xi.Color.Xi_Color)
   is
      use Star_Info_Lists;
      Position : Cursor;
   begin

      Table_Reader.Check_Tables;

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
         Color := Info.Colour;
      end;

   end Get_Main_Sequence_Info;

   -----------------
   -- Read_Tables --
   -----------------

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

            use Xi;

            function Get (Name : String) return Real
            is (Real (Float'(Info_Config.Get (Name))));

            Class        : constant String := Info_Config.Get ("type");
            Surface_Temp : constant Real := Get ("surface-temp");
            Radius       : constant Real := Get ("radius");
            Mass         : constant Real := Get ("mass");
            Luminosity   : constant Real := Get ("luminosity");
            Red          : constant Natural := Info_Config.Get ("r");
            Green        : constant Natural := Info_Config.Get ("g");
            Blue         : constant Natural := Info_Config.Get ("b");
            Colour       : constant Xi.Color.Xi_Color :=
                             Xi.Color.Xi_Color'
                               (Red   => Xi_Float (Red) / 255.0,
                                Green => Xi_Float (Green) / 255.0,
                                Blue  => Xi_Float (Blue) / 255.0,
                                Alpha => 1.0);
            Info         : constant Star_Info_Record :=
                             (Solar_Masses => Mass,
                              Class        =>
                                Stellar_Class_Type'Value
                                  ((1 => Class (Class'First))),
                              Subclass     =>
                                Stellar_Subclass_Type'Value
                                  ((1 => Class (Class'First + 1))),
                              Surface_Temp => Surface_Temp,
                              Colour       =>
                                Brighten (Colour, Surface_Temp),
                              Radius       => Radius,
                              Luminosity   => Luminosity);
         begin
            Main_Sequence_Table.Append (Info);
         end;
      end loop;
   end Read_Tables;

   ------------------
   -- Table_Reader --
   ------------------

   protected body Table_Reader is

      ------------------
      -- Check_Tables --
      ------------------

      procedure Check_Tables is
      begin
         if Main_Sequence_Table.Is_Empty then
            Read_Tables;
         end if;
      end Check_Tables;

   end Table_Reader;
end Concorde.Stars.Tables;
