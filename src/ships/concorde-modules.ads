with Concorde.Components;
with Concorde.Objects;

package Concorde.Modules is

   type Root_Module_Type is
     new Concorde.Objects.Root_Object_Type
   with private;

   function Component
     (Module : Root_Module_Type'Class)
      return Concorde.Components.Component_Type;

   function Size
     (Module : Root_Module_Type'Class)
      return Natural;

   function Mass
     (Module : Root_Module_Type'Class)
      return Natural;

   function Damage
     (Module : Root_Module_Type'Class)
      return Unit_Real;

   procedure Hit
     (Module         : in out Root_Module_Type'Class);

   function Exploding
     (Module : Root_Module_Type'Class)
      return Boolean;

   function Charge
     (Module : Root_Module_Type'Class)
      return Unit_Real;

   function Stored_Energy
     (Module : Root_Module_Type'Class)
      return Natural;

   procedure Execute
     (Module : in out Root_Module_Type'Class);

   procedure Draw_Power
     (Module : in out Root_Module_Type'Class;
      Power  : Natural);

   procedure Initial_State
     (Module : in out Root_Module_Type'Class);

   type Module_Type is access all Root_Module_Type'Class;

   function New_Module
     (Component : Concorde.Components.Component_Type)
      return Module_Type;

   type Array_Of_Modules is array (Positive range <>) of Module_Type;

private

   type Root_Module_Type is
     new Concorde.Objects.Root_Object_Type with
      record
         Component : Concorde.Components.Component_Type;
         Mass      : Natural;
         HP        : Natural;
         Energy    : Natural;
         Heat      : Natural;
         Exploding : Boolean;
      end record;

end Concorde.Modules;
