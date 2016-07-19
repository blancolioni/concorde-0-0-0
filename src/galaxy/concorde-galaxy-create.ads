with WL.Random.Names;

package Concorde.Galaxy.Create is

   type Galaxy_Shape is (Cube, Sphere, Spiral);

   procedure Create_Galaxy
     (System_Count        : Natural;
      Shape               : Galaxy_Shape;
      Average_Connections : Positive;
      Reset_Seed          : Boolean;
      Name_Generator      : WL.Random.Names.Name_Generator);

end Concorde.Galaxy.Create;
