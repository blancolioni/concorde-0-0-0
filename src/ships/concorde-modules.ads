with Concorde.Components;
with Concorde.Objects;

package Concorde.Modules is

   type Root_Module_Type is
     new Concorde.Objects.Root_Named_Object_Type
   with private;

   function Component
     (Module : Root_Module_Type'Class)
      return Concorde.Components.Component_Type;

   function Size
     (Module : Root_Module_Type'Class)
      return Size_Type;

   function Mass
     (Module : Root_Module_Type'Class)
      return Non_Negative_Real;

   function Damage
     (Module : Root_Module_Type'Class)
      return Unit_Real;

   function Effectiveness
     (Module : Root_Module_Type'Class)
      return Unit_Real;

   function Explosion_Chance
     (Module : Root_Module_Type'Class)
      return Unit_Real;

   procedure Hit
     (Module : in out Root_Module_Type'Class);

   function Exploding
     (Module : Root_Module_Type'Class)
      return Boolean;

   procedure Start_Explosion
     (Module : in out Root_Module_Type'Class);

   function Explosion_Timer
     (Module : Root_Module_Type'Class)
      return Integer;

   function Explosion_Size
     (Module : Root_Module_Type'Class)
      return Natural;

   function Charge
     (Module : Root_Module_Type'Class)
      return Unit_Real;

   function Stored_Energy
     (Module : Root_Module_Type'Class)
      return Non_Negative_Real;

   procedure Execute
     (Module : in out Root_Module_Type'Class);

   procedure Update_Damage
     (Module : in out Root_Module_Type'Class);

   procedure Draw_Power
     (Module : in out Root_Module_Type'Class;
      Power  : Non_Negative_Real);

   function Maximum_Power_Draw
     (Module : in out Root_Module_Type'Class)
      return Non_Negative_Real;

   procedure Initial_State
     (Module : in out Root_Module_Type'Class);

   type Module_Type is access all Root_Module_Type'Class;

   function New_Module
     (Name      : String;
      Component : Concorde.Components.Component_Type;
      Size      : Size_Type)
      return Module_Type;

   type Array_Of_Modules is array (Positive range <>) of Module_Type;

private

   type Root_Module_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Component         : Concorde.Components.Component_Type;
         Size              : Size_Type;
         Volume            : Positive;
         Max_Stored_Energy : Non_Negative_Real;
         Stored_Energy     : Non_Negative_Real := 0.0;
         Heat              : Non_Negative_Real;
         Max_Hits          : Natural;
         Hits              : Natural;
         Exploding         : Boolean;
         Explosion_Timer   : Integer;
         Explosion_Size    : Natural;
      end record;

end Concorde.Modules;