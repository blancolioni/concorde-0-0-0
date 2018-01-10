with Tropos;

package Concorde.People.Individuals.Portraits is

   procedure Configure_Portraits
     (Feature_Config : Tropos.Configuration;
      Sprite_Config  : Tropos.Configuration);

   procedure Save_Portrait
     (Individual : not null access constant Root_Individual_Type'Class;
      Path       : String);

end Concorde.People.Individuals.Portraits;
