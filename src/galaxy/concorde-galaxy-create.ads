with WL.Random.Names;

package Concorde.Galaxy.Create is

   type Galaxy_Shape is (Cube, Sphere, Spiral);

   procedure Create_Galaxy
     (System_Count        : Natural;
      Shape               : Galaxy_Shape;
      DX, DY, DZ          : Real;
      Average_Connections : Positive;
      Name_Generator      : WL.Random.Names.Name_Generator);
   --  Create System_Count systems using the given shape to guide their
   --  positions.  If the given shape (e.g. a spiral) defines a linear
   --  or planer layout, then DX, DY, and DZ define the standard
   --  deviation of a normal distribution that is used to randomly
   --  place the system away from the selected location; otherwise
   --  these are ignored.
   --  If the galaxy is to be fully connected, Average_Connections is
   --  the target number of connections, on average, that each system
   --  has to other systems.  If Reset_Seed is true, the layout will
   --  be randomised.  Name_Generator is used to give each system
   --  a random name.

   procedure Create_Catalogue_Systems
     (Catalogue_Path : String);

end Concorde.Galaxy.Create;
