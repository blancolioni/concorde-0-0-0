with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Glib;

with Cairo;
with Cairo.Image_Surface;
with Cairo.Png;

with WL.String_Maps;

with Concorde.Paths;

package body Concorde.People.Individuals.Portraits is

   Portrait_Width  : constant := 152;
   Portrait_Height : constant := 152;

   type Sprite_Type is
      record
         Surface     : Cairo.Cairo_Surface;
         Frame_Count : Positive;
         Width       : Natural;
         Height      : Natural;
      end record;

   package Sprite_Maps is
     new WL.String_Maps (Sprite_Type);

   Sprite_Map : Sprite_Maps.Map;

   type Gene_Access is access constant Genetics.Gene_Type;

   type Portrait_Layer is
      record
         Sprite_Name : access constant String;
         Gene        : Gene_Access;
         Eye_Color   : Boolean := False;
         Property    : Natural := 0;
         Offset_X    : Natural := 0;
         Offset_Y    : Natural := 0;
      end record;

   package Portrait_Layer_Vectors is
     new Ada.Containers.Vectors (Positive, Portrait_Layer);

   type Portrait_Type is
      record
         Layers : Portrait_Layer_Vectors.Vector;
      end record;

   package Portrait_Maps is
     new WL.String_Maps (Portrait_Type);

   Portrait_Map : Portrait_Maps.Map;

   -------------------------
   -- Configure_Portraits --
   -------------------------

   procedure Configure_Portraits
     (Feature_Config  : Tropos.Configuration;
      Property_Config : Tropos.Configuration;
      Sprite_Config   : Tropos.Configuration)
   is
      pragma Unreferenced (Property_Config);
      Genes : constant array (0 .. 7) of Gene_Access :=
                (Genetics.Neck'Access,
                 Genetics.Chin'Access,
                 Genetics.Mouth'Access,
                 Genetics.Nose'Access,
                 Genetics.Cheeks'Access,
                 null,
                 Genetics.Eyes'Access,
                 Genetics.Ears'Access);
   begin
      for Config of Sprite_Config.Child ("spriteTypes") loop
         declare
            Name        : constant String :=
                            Config.Get ("name");
            Raw_Path    : constant String :=
                            Config.Get ("texturefile");
            Frame_Count : constant Positive :=
                            Config.Get ("noOfFrames");
            Base_File_Name : constant String :=
                               Ada.Directories.Base_Name (Raw_Path);
            Sub_Directory  : constant String :=
                               Ada.Directories.Base_Name
                                 (Ada.Directories.Containing_Directory
                                    (Raw_Path));
            Full_Path      : constant String :=
                               Concorde.Paths.Config_File
                                 ("images/characters/"
                                  & Sub_Directory
                                  & "/"
                                  & Base_File_Name
                                  & ".png");
         begin
            if Ada.Directories.Exists (Full_Path) then
               declare
                  use Cairo, Cairo.Image_Surface;
                  Surface        : constant Cairo_Surface :=
                                     Cairo.Png.Create_From_Png (Full_Path);
                  Surface_Width  : constant Glib.Gint :=
                                     Get_Width (Surface);
                  Surface_Height : constant Glib.Gint :=
                                     Get_Height (Surface);
                  Sprite         : constant Sprite_Type :=
                                     Sprite_Type'
                                       (Surface     => Surface,
                                        Frame_Count => Frame_Count,
                                        Width       =>
                                          Natural (Surface_Width)
                                        / Frame_Count,
                                        Height      =>
                                          Natural (Surface_Height));
               begin
--                    Ada.Text_IO.Put_Line
--                      ("sprite: " & Name & ": "
--                       & Sub_Directory & "/" & Base_File_Name & ".png"
--                       & Sprite.Width'Img & Sprite.Height'Img
--                       & Sprite.Frame_Count'Img
--                       & Stride'Img
--                       & " " & Format'Img);
                  Sprite_Map.Insert (Name, Sprite);
               end;
            else
               if False then
                  Ada.Text_IO.Put_Line
                    ("sprite: " & Name & ": "
                     & Sub_Directory & "/" & Base_File_Name & ".png"
                     & ": image file not found");
               end if;
            end if;
         end;
      end loop;

      for Config of Feature_Config.Child ("spriteTypes") loop
         declare
            Name : constant String := Config.Get ("name");
            Portrait : Portrait_Type;
         begin
            for Layer_Config of Config.Child ("layer") loop
               declare
                  use Ada.Strings.Fixed;
                  Layer_Definition : constant String :=
                                       Layer_Config.Config_Name;
                  Layer            : Portrait_Layer;
                  Start_Index      : Positive := Layer_Definition'First;
                  Finish_Index     : Natural  :=
                                       Index
                                         (Layer_Definition,
                                          ":", Start_Index);

                  function Finished return Boolean
                  is (Finish_Index = 0);

                  function Field return String
                  is (Layer_Definition (Start_Index .. Finish_Index - 1));

                  procedure Next_Field;

                  ----------------
                  -- Next_Field --
                  ----------------

                  procedure Next_Field is
                  begin
                     if Finish_Index > Layer_Definition'Length then
                        Finish_Index := 0;
                     else
                        Start_Index := Finish_Index + 1;
                        Finish_Index := Index (Layer_Definition, ":",
                                               Start_Index);
                        if Finish_Index = 0 then
                           Finish_Index := Layer_Definition'Length + 1;
                        end if;
                     end if;
                  end Next_Field;

               begin

                  if Field /= "" then
                     Layer.Sprite_Name :=
                       new String'(Field);

                     Next_Field;

                     if not Finished then
                        declare
                           Source : constant String := Field;
                        begin
                           if Source (Source'First) = 'p' then
                              --  property layer
                              Layer.Gene := null;
                              declare
                                 Index : constant Natural :=
                                           Natural'Value
                                             (Source (Source'First + 1
                                              .. Source'Last));
                              begin
                                 Layer.Property := Index;
                              end;
                           elsif Source (Source'First) = 'd' then
                              declare
                                 Index : constant Natural :=
                                           Natural'Value
                                             (Source (Source'First + 1
                                              .. Source'Last));
                              begin
                                 if Index in Genes'Range then
                                    Layer.Gene := Genes (Index);
                                 else
                                    Layer.Gene := null;
                                 end if;
                              end;

                              Layer.Offset_X := 0;
                              Layer.Offset_Y := 0;
                              Layer.Property := 0;

                              Next_Field;

                              if not Finished and then Field = "e" then
                                 Layer.Eye_Color := True;
                                 Next_Field;
                              end if;

                              if not Finished then
                                 declare
                                    Offset : constant String := Field;
                                 begin
                                    if Offset'Length > 0
                                      and then Offset (Offset'First) = 'o'
                                    then
                                       declare
                                          use Ada.Characters.Handling;
                                          Start : constant Positive :=
                                                    Offset'First + 1;
                                          Index : Positive := Start;
                                       begin
                                          while Index <= Offset'Last
                                            and then Is_Digit (Offset (Index))
                                          loop
                                             Index := Index + 1;
                                          end loop;
                                          if Index > Start then
                                             Layer.Offset_X :=
                                               Natural'Value
                                                 (Offset (Start .. Index - 1));
                                             if Index < Offset'Last
                                               and then Offset (Index) = 'x'
                                             then
                                                Layer.Offset_Y :=
                                                  Natural'Value
                                                    (Offset (Index + 1
                                                     .. Offset'Last));
                                             end if;
                                          end if;
                                       end;
                                    end if;
                                 end;
                              end if;

                              Ada.Text_IO.Put_Line
                                (Name & ": "
                                 & Layer.Sprite_Name.all
                                 & ": "
                                 & Layer.Offset_X'Img
                                 & Layer.Offset_Y'Img);

                           end if;
                        end;
                     end if;

                     Portrait.Layers.Append (Layer);
                  end if;
               end;
            end loop;

            Portrait_Map.Insert (Name, Portrait);

         end;
      end loop;
   end Configure_Portraits;

   -------------------
   -- Save_Portrait --
   -------------------

   procedure Save_Portrait
     (Individual : not null access constant Root_Individual_Type'Class;
      Path       : String)
   is
      Surface : constant Cairo.Cairo_Surface :=
                  Cairo.Image_Surface.Create
                    (Format => Cairo.Image_Surface.Cairo_Format_ARGB32,
                     Width  => Portrait_Width,
                     Height => Portrait_Height);
      Cr      : constant Cairo.Cairo_Context :=
                  Cairo.Create (Surface);

      procedure Add_Feature
        (Gene          : Genetics.Gene_Type;
         Base          : Cairo.Cairo_Surface;
         Dst_X, Dst_Y  : Natural;
         Width, Height : Positive;
         Count         : Positive)
        with Unreferenced;

      procedure Add_Layer
        (Sprite        : Sprite_Type;
         Dst_X, Dst_Y  : Natural;
         Frame_Index   : Positive);

      -----------------
      -- Add_Feature --
      -----------------

      procedure Add_Feature
        (Gene          : Genetics.Gene_Type;
         Base          : Cairo.Cairo_Surface;
         Dst_X, Dst_Y  : Natural;
         Width, Height : Positive;
         Count         : Positive)
      is
         use Glib;
         Value : constant Genetics.Gene_Expression :=
                   Genetics.Express (Individual.DNA, Gene);
         Index : constant Natural :=
                   (Natural (Value) - 1) mod Count;
         X     : constant Glib.Gdouble :=
                   Glib.Gdouble (Dst_X - Index * Width);
      begin
--           Cairo.Set_Source_Rgb (Cr, 0.0, 0.0, 1.0);
--           Cairo.Rectangle
--             (Cr,
--              Gdouble (Dst_X), Gdouble (Dst_Y),
--              Gdouble (Width), Gdouble (Height));
--           Cairo.Fill (Cr);
         Cairo.Set_Source_Surface
           (Cr      => Cr,
            Surface => Base,
            X       => X,
            Y       => Gdouble (Dst_Y));
         Cairo.Rectangle
           (Cr,
            Gdouble (Dst_X), Gdouble (Dst_Y),
            Gdouble (Width), Gdouble (Height));
         Cairo.Fill (Cr);
      end Add_Feature;

      ---------------
      -- Add_Layer --
      ---------------

      procedure Add_Layer
        (Sprite        : Sprite_Type;
         Dst_X, Dst_Y  : Natural;
         Frame_Index   : Positive)
      is
         use Glib;
         X : constant Gdouble :=
               Gdouble (Dst_X)
               - Gdouble ((Frame_Index - 1) mod Sprite.Frame_Count
                          * Sprite.Width - 1);
      begin
         Cairo.Set_Source_Surface
           (Cr      => Cr,
            Surface => Sprite.Surface,
            X       => X,
            Y       => Gdouble (Dst_Y));
         Cairo.Rectangle
           (Cr,
            Gdouble (Dst_X), Gdouble (Dst_Y),
            Gdouble (Sprite.Width), Gdouble (Sprite.Height));
         Cairo.Fill (Cr);
      end Add_Layer;

      Portrait_Name : constant String :=
                        (case Individual.Gender is
                            when Female =>
                               "PORTRAIT_westerngfx_female",
                            when Male   =>
                               "PORTRAIT_westerngfx_male",
                            when None   =>
                               "PORTRAIT_westerngfx_none");
   begin
      Cairo.Save (Cr);
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Clear);
      Cairo.Paint (Cr);
      Cairo.Restore (Cr);

      for Layer of
        Portrait_Map.Element (Portrait_Name).Layers
      loop
         if Layer.Gene /= null then
            declare
               Sprite : constant Sprite_Type :=
                          Sprite_Map.Element (Layer.Sprite_Name.all);
            begin
               Add_Layer
                 (Sprite,
                  Layer.Offset_X,
                  Portrait_Height - Layer.Offset_Y - Sprite.Height,
                  Positive
                    (Genetics.Express (Individual.DNA, Layer.Gene.all)));
            end;
         elsif Layer.Sprite_Name /= null
           and then Sprite_Map.Contains (Layer.Sprite_Name.all)
         then
            declare
               Sprite : constant Sprite_Type :=
                          Sprite_Map.Element (Layer.Sprite_Name.all);
            begin
               Add_Layer
                 (Sprite, 0, 0, 1);
            end;
         end if;
      end loop;

--        declare
--           Surfaces : constant Portrait_Surface_Record :=
--                        Portrait_Map.Element ("western_female") (Young);
--        begin
--
--           Cairo.Set_Source_Surface
--             (Cr      => Cr,
--              Surface => Surfaces.Base,
--              X       => 0.0,
--              Y       => 0.0);
--           Cairo.Paint (Cr);
--
--           Add_Feature (Genetics.Eyes, Surfaces.Eyes,
--                        Dst_X  => 50,
--                        Dst_Y  => 8,
--                        Width  => 68,
--                        Height => 64,
--                        Count  => 13);

      Cairo.Destroy (Cr);

      declare
         use all type Cairo.Cairo_Status;
         Status : constant Cairo.Cairo_Status :=
                    Cairo.Png.Write_To_Png (Surface, Path);
      begin
         pragma Assert (Status = Cairo_Status_Success);
      end;

      Cairo.Surface_Destroy (Surface);

   end Save_Portrait;

end Concorde.People.Individuals.Portraits;
