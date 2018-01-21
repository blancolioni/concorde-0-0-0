with Cairo;

with Tropos;

package Concorde.People.Individuals.Portraits is

   procedure Configure_Portraits
     (Feature_Config  : Tropos.Configuration;
      Property_Config : Tropos.Configuration;
      Sprite_Config   : Tropos.Configuration);

   procedure Draw_Portrait
     (Context       : Cairo.Cairo_Context;
      Individual    : not null access constant Root_Individual_Type'Class;
      Width, Height : Positive);

   procedure Save_Portrait
     (Individual : not null access constant Root_Individual_Type'Class;
      Path       : String);

end Concorde.People.Individuals.Portraits;
