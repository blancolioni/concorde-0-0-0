with WL.Random.Names;

package Concorde.Galaxy.Create is

   procedure Create_Galaxy
     (System_Count        : Natural;
      Average_Connections : Positive;
      Reset_Seed          : Boolean;
      Name_Generator      : WL.Random.Names.Name_Generator);

end Concorde.Galaxy.Create;
