with Xi.Entity;

with Memor.Element_Vectors;

package body Concorde.Ships.Xi_Model is

   package Ship_Entity_Vectors is
     new Memor.Element_Vectors
       (Xi.Entity.Xi_Entity, null, Xi.Entity."=");

   ---------------------
   -- Transit_To_Ship --
   ---------------------

   procedure Transit_To_Ship
     (Ship   : Ship_Type;
      Model  : in out Concorde.Xi_UI.Root_Xi_Model'Class)
   is
   begin
      Concorde.Xi_UI.Transit_To_Object
        (Model, Ship.Orbiting);
   end Transit_To_Ship;

end Concorde.Ships.Xi_Model;
