with Ada.Directories;

with Tropos.Reader;

with Concorde.String_Maps;
with Concorde.Paths;

package body Concorde.Xi_UI.Assets is

   Have_Assets : Boolean := False;
   Asset_Config : Tropos.Configuration;

   procedure Check_Assets;

   package Texture_Asset_Map is
     new Concorde.String_Maps (Xi.Texture.Xi_Texture,
                               Xi.Texture."=");

   Texture_Assets : Texture_Asset_Map.Map;

   ------------------
   -- Check_Assets --
   ------------------

   procedure Check_Assets is
   begin
      if Have_Assets then
         return;
      end if;

      Asset_Config :=
        Tropos.Reader.Read_Config
          (Concorde.Paths.Config_File ("assets.txt"));
      Have_Assets := True;
   end Check_Assets;

   -------------
   -- Texture --
   -------------

   function Texture
     (Name : String)
      return Xi.Texture.Xi_Texture
   is
   begin
      Check_Assets;

      if Texture_Assets.Contains (Name) then
         return Texture_Assets (Name);
      elsif Asset_Config.Contains (Name) then
         declare
            Path : constant String :=
                     Concorde.Paths.Config_File
                       (Asset_Config.Get (Name));
         begin

            if Ada.Directories.Exists (Path) then
               declare
                  Texture : constant Xi.Texture.Xi_Texture :=
                              Xi.Texture.Create_From_Png
                                (Name, Path);
               begin
                  Texture_Assets.Insert (Name, Texture);
                  return Texture;
               end;
            end if;

         end;
      end if;

      declare
         Result : constant Xi.Texture.Xi_Texture :=
                    Texture ("default_texture");
      begin
         Texture_Assets.Insert (Name, Result);
         return Result;
      end;
   end Texture;

end Concorde.Xi_UI.Assets;
