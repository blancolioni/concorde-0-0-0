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

with Concorde.Weighted_Random_Choices;

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

   package Modifier_Vectors is
     new Ada.Containers.Vectors (Positive, Tropos.Configuration, Tropos."=");

   type Property_Frame_Record is
      record
         Frame          : Positive;
         Initial_Factor : Natural;
         Modifiers      : Modifier_Vectors.Vector;
      end record;

   package Property_Frame_Vectors is
     new Ada.Containers.Vectors (Positive, Property_Frame_Record);

   type Property_Record is
      record
         Frames : Property_Frame_Vectors.Vector;
      end record;

   package Property_Vectors is
     new Ada.Containers.Vectors (Positive, Property_Record);

   Property_Vector : Property_Vectors.Vector;

   type Property_Modifier_Function is access
     function (Individual : not null access constant
                 Root_Individual_Type'Class;
               Config     : Tropos.Configuration)
               return Boolean;

   package Property_Modifier_Maps is
     new WL.String_Maps (Property_Modifier_Function);

   Property_Modifier_Map : Property_Modifier_Maps.Map;

   function Test
     (Individual : not null access constant Root_Individual_Type'Class;
      Modifier   : Tropos.Configuration)
      return Boolean;

   function Boolean_Test
     (Config     : Tropos.Configuration;
      Value      : Boolean)
      return Boolean
   is (Value or else Config.Value = "no");

   function Not_Operator
     (Individual : not null access constant Root_Individual_Type'Class;
      Config     : Tropos.Configuration)
      return Boolean
   is (not Test (Individual, Config));

   function Is_Alive
     (Individual : not null access constant Root_Individual_Type'Class;
      Config     : Tropos.Configuration)
      return Boolean
   is (Boolean_Test (Config, Individual.Alive));

   function Is_Female
     (Individual : not null access constant Root_Individual_Type'Class;
      Config     : Tropos.Configuration)
      return Boolean
   is (Boolean_Test (Config, Individual.Gender = Female));

   function Is_Male
     (Individual : not null access constant Root_Individual_Type'Class;
      Config     : Tropos.Configuration)
      return Boolean
   is (Boolean_Test (Config, Individual.Gender = Male));

   function Is_Prisoner
     (Individual : not null access constant Root_Individual_Type'Class;
      Config     : Tropos.Configuration)
      return Boolean
   is (Boolean_Test (Config, False));

   function Age_At_Least
     (Individual : not null access constant Root_Individual_Type'Class;
      Config     : Tropos.Configuration)
      return Boolean
   is (Config.Value <= 21);

   -------------------------
   -- Configure_Portraits --
   -------------------------

   procedure Configure_Portraits
     (Feature_Config  : Tropos.Configuration;
      Property_Config : Tropos.Configuration;
      Sprite_Config   : Tropos.Configuration)
   is
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

      Property_Modifier_Map.Insert ("not", Not_Operator'Access);
      Property_Modifier_Map.Insert ("is_alive", Is_Alive'Access);
      Property_Modifier_Map.Insert ("is_female", Is_Female'Access);
      Property_Modifier_Map.Insert ("is_male", Is_Male'Access);
      Property_Modifier_Map.Insert ("prisoner", Is_Prisoner'Access);
      Property_Modifier_Map.Insert ("age", Age_At_Least'Access);

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
                                 Layer.Property := Index + 1;
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

      for Config of Property_Config loop
         declare
            Frames : Property_Frame_Vectors.Vector;
         begin
            for Frame_Config of Config loop
               declare
                  Frame : Property_Frame_Record :=
                            Property_Frame_Record'
                              (Frame          => Frames.Last_Index + 1,
                               Initial_Factor => Frame_Config.Get ("factor"),
                               Modifiers      => <>);
               begin
                  for Mod_Config of Frame_Config.Children ("modifier") loop
                     Frame.Modifiers.Append (Mod_Config);
                  end loop;
                  Frames.Append (Frame);
               end;
            end loop;
            if Frames.Is_Empty then
               Frames.Append
                 (Property_Frame_Record'
                    (Frame          => 1,
                     Initial_Factor => 100,
                     Modifiers      => <>));
            end if;

            Property_Vector.Append ((Frames => Frames));
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
         elsif Layer.Property > 0
           and then Layer.Sprite_Name /= null
           and then Sprite_Map.Contains (Layer.Sprite_Name.all)
         then
            declare
               Sprite : constant Sprite_Type :=
                          Sprite_Map.Element (Layer.Sprite_Name.all);
               Property : constant Property_Record :=
                            Property_Vector.Element (Layer.Property);
               Frame    : Natural := 0;

               package Frame_Choices is
                 new Concorde.Weighted_Random_Choices (Positive);

               Choices  : Frame_Choices.Weighted_Choice_Set;

            begin
               for Prop_Frame of Property.Frames loop
                  declare
                     Factor : Float := Float (Prop_Frame.Initial_Factor);
                  begin
                     for Modifier of Prop_Frame.Modifiers loop
                        if Test (Individual, Modifier) then
                           Factor := Factor * Modifier.Get ("factor");
                        end if;
                        exit when Factor = 0.0;
                     end loop;
                     if Factor >= 100.0 then
                        Frame := Prop_Frame.Frame;
                        exit;
                     elsif Factor > 0.0 then
                        Choices.Insert
                          (Prop_Frame.Frame, Positive (Factor + 0.5));
                     end if;
                  end;
               end loop;

               if Frame = 0 and then not Choices.Is_Empty then
                  Frame := Choices.Choose;
               end if;

               if Frame > 0 then
                  Add_Layer
                    (Sprite, 0, 0, Frame);
               end if;
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

   ----------
   -- Test --
   ----------

   function Test
     (Individual : not null access constant Root_Individual_Type'Class;
      Modifier   : Tropos.Configuration)
      return Boolean
   is
   begin
      for Fn of Modifier loop
         if Fn.Config_Name /= "factor" then
            if Property_Modifier_Map.Contains (Fn.Config_Name) then
               if not Property_Modifier_Map.Element (Fn.Config_Name)
                 (Individual, Fn)
               then
                  return False;
               end if;
            else
               return False;
            end if;
         end if;
      end loop;

      return True;
   end Test;

end Concorde.People.Individuals.Portraits;
