private with Memor.Database;

with Xi.Matrices;

with Concorde.Objects;
with Concorde.Ships.Components;

package Concorde.Ships.Modules is

   type Root_Module_Type is
     new Concorde.Objects.Root_User_Named_Object_Type
     and Concorde.Objects.Massive_Object_Interface
   with private;

   overriding function Mass
     (Module : Root_Module_Type)
      return Non_Negative_Real;

   function Component
     (Module : Root_Module_Type'Class)
      return Concorde.Ships.Components.Component_Type;

   function Position
     (Module : Root_Module_Type'Class)
      return Xi.Matrices.Vector_3;

   function Orientation
     (Module : Root_Module_Type'Class)
      return Xi.Matrices.Matrix_4;

   function Thrust_Component
     (Module : Root_Module_Type'Class;
      Vector : Xi.Matrices.Vector_3)
      return Non_Negative_Real;

   type Module_Type is access constant Root_Module_Type'Class;

private

   type Root_Module_Type is
     new Concorde.Objects.Root_User_Named_Object_Type
     and Concorde.Objects.Massive_Object_Interface with
      record
         Component   : Concorde.Ships.Components.Component_Type;
         Position    : Xi.Matrices.Vector_3;
         Orientation : Xi.Matrices.Matrix_4;
      end record;

   overriding function Object_Database
     (Item : Root_Module_Type)
      return Memor.Memor_Database;

   overriding function Mass
     (Module : Root_Module_Type)
      return Non_Negative_Real
   is (Module.Component.Mass);

   function Component
     (Module : Root_Module_Type'Class)
      return Concorde.Ships.Components.Component_Type
   is (Module.Component);

   function Position
     (Module : Root_Module_Type'Class)
      return Xi.Matrices.Vector_3
   is (Module.Position);

   function Orientation
     (Module : Root_Module_Type'Class)
      return Xi.Matrices.Matrix_4
   is (Module.Orientation);

   package Db is
     new Memor.Database
       ("ship-module", Root_Module_Type, Module_Type);

   overriding function Object_Database
     (Item : Root_Module_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

end Concorde.Ships.Modules;
