with Xi.Shader.Noise;
with Xi.Texture;

package Concorde.Xi_UI.Noise is

   function Create_Noise_Shader
     (Palette_Name : String;
      Initiator    : Integer)
      return Xi.Shader.Noise.Xi_Noise_Shader;

end Concorde.Xi_UI.Noise;
