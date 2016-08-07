with Xi.Shader.Load;

with Concorde.String_Maps;

package body Concorde.Xi_UI.Shaders is

   package Shader_Maps is
     new Concorde.String_Maps (Xi.Shader.Xi_Shader,
                               Xi.Shader."=");

   Loaded_Shaders : Shader_Maps.Map;

   ------------
   -- Shader --
   ------------

   function Shader (Name : String) return Xi.Shader.Xi_Shader is
   begin
      if not Loaded_Shaders.Contains (Name) then
         declare
            Shader : constant Xi.Shader.Xi_Shader :=
                       Xi.Shader.Load.Load
                         (Name, Name);
         begin
            Loaded_Shaders.Insert (Name, Shader);
         end;
      end if;
      return Loaded_Shaders.Element (Name);
   end Shader;

end Concorde.Xi_UI.Shaders;
