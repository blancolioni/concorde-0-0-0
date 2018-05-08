private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;
private with WL.String_Maps;

private with Memor;
private with Memor.Database;

with Xi.Matrices;

with Concorde.Objects;
with Concorde.Commodities;
with Concorde.Vectors;

package Concorde.Ships.Components is

   type Root_Component_Type is
     new Concorde.Objects.Root_Localised_Object_Type
     and Concorde.Objects.Massive_Object_Interface
   with private;

   overriding function Mass
     (Component : Root_Component_Type)
      return Non_Negative_Real;

   function Cargo_Payload_Volume
     (Component : Root_Component_Type)
      return Non_Negative_Real;

   function Commodity_Payload_Volume
     (Component : Root_Component_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Non_Negative_Real;

   function Thrust_Component
     (Component   : Root_Component_Type'Class;
      Orientation : Xi.Matrices.Matrix_4;
      Vector      : Xi.Matrices.Vector_3)
      return Non_Negative_Real;

   function Maximum_Thrust
     (Component : Root_Component_Type'Class)
      return Non_Negative_Real;

   type Component_Type is access constant Root_Component_Type'Class;

private

   type Component_Shape is
     (Rectangular_Prism, Hexagonal_Prism, Cylinder,
      Cone, Sphere, Cube, Conical_Frustum);

   type Attachment is
      record
         Location     : Concorde.Vectors.Vector_3;
         Force_Limit  : Non_Negative_Real := 0.0;
         Torque_Limit : Non_Negative_Real := 0.0;
      end record;

   package Attachment_Maps is
     new WL.String_Maps (Attachment);

   type Component_Fuel_Record is
      record
         Fuel  : Concorde.Commodities.Commodity_Type;
         Ratio : Unit_Real;
      end record;

   package Component_Fuel_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Component_Fuel_Record);

   type Root_Component_Type is
     new Concorde.Objects.Root_Localised_Object_Type
     and Concorde.Objects.Massive_Object_Interface with
      record
         Throttled                 : Boolean := False;
         Pressurised               : Boolean := False;
         Cargo                     : Boolean := False;
         Tank                      : Boolean := False;
         Thruster                  : Boolean := False;
         Bounding_Box              : Bounding_Box_Type;
         Shape                     : Component_Shape;
         Mesh                      : Ada.Strings.Unbounded.Unbounded_String;
         Mass                      : Non_Negative_Real;
         Attachments               : Attachment_Maps.Map;
         Max_Linear_Acceleration   : Non_Negative_Real := 0.0;
         Max_Angular_Acceleration  : Non_Negative_Real := 0.0;
         Max_Operating_Temperature : Non_Negative_Real := 0.0;
         Failure_Temperature       : Non_Negative_Real := 0.0;
         Payload_Volume            : Non_Negative_Real := 0.0;
         Thrust_Minimum            : Non_Negative_Real := 0.0;
         Thrust_Maximum            : Non_Negative_Real := 0.0;
         Heat                      : Non_Negative_Real := 0.0;
         Start_Power               : Non_Negative_Real := 0.0;
         Start_Duration            : Duration := 0.0;
         Fuel                      : Component_Fuel_Lists.List;
         Propellant                : Component_Fuel_Lists.List;
         Fuel_Is_Propellant        : Boolean := False;
      end record;

   overriding function Object_Database
     (Item : Root_Component_Type)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       ("ship-component", Root_Component_Type, Component_Type);

   overriding function Object_Database
     (Item : Root_Component_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   overriding function Mass
     (Component : Root_Component_Type)
      return Non_Negative_Real
   is (Component.Mass);

   function Cargo_Payload_Volume
     (Component : Root_Component_Type)
      return Non_Negative_Real
   is (if Component.Cargo then Component.Payload_Volume else 0.0);

   function Commodity_Payload_Volume
     (Component : Root_Component_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Non_Negative_Real
   is (if Component.Cargo then Component.Payload_Volume else 0.0);

   function Maximum_Thrust
     (Component : Root_Component_Type'Class)
      return Non_Negative_Real
   is (Component.Thrust_Maximum);

   package Component_Vectors is
     new Ada.Containers.Vectors (Positive, Component_Type);

   Component_Vector : Component_Vectors.Vector;

end Concorde.Ships.Components;
