private with Memor;
private with Memor.Database;

with Concorde.Components;
with Concorde.Objects;

package Concorde.Modules is

   type Root_Module_Type is
     new Concorde.Objects.Root_Localised_Object_Type
   with private;

   function Component
     (Module : Root_Module_Type'Class)
      return Concorde.Components.Component_Type;

   function Size
     (Module : Root_Module_Type'Class)
      return Size_Type;

   function Volume
     (Module : Root_Module_Type'Class)
      return Positive;

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
     (Module : in out Root_Module_Type'Class;
      Damage : Positive);

   procedure Repair
     (Module : in out Root_Module_Type'Class;
      Points : in out Natural);

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

   function Maximum_Stored_Energy
     (Module : Root_Module_Type'Class)
      return Non_Negative_Real;

   function Maximum_Output
     (Module : Root_Module_Type'Class)
      return Non_Negative_Real;

   procedure Execute
     (Module : in out Root_Module_Type'Class);

   procedure Execute
     (Module : in out Root_Module_Type'Class;
      Charge : Non_Negative_Real)
     with Pre => Charge <= Module.Stored_Energy;

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

   function Is_Shield
     (Module : Root_Module_Type'Class)
      return Boolean
   is (Concorde.Components."=" (Module.Component.Class,
                                Concorde.Components.Shield_Generator));

   type Module_Type is access constant Root_Module_Type'Class;

   function New_Module
     (Name      : String;
      Component : Concorde.Components.Component_Type;
      Size      : Size_Type)
      return Module_Type;

   type Array_Of_Modules is array (Positive range <>) of Module_Type;

   type Updateable_Reference (Item : not null access Root_Module_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Module_Type'Class)
      return Updateable_Reference;

private

   type Root_Module_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Component         : Concorde.Components.Component_Type;
         Size              : Size_Type;
         Volume            : Positive;
         Max_Stored_Energy : Non_Negative_Real;
         Stored_Energy     : Non_Negative_Real := 0.0;
         Heat              : Non_Negative_Real := 0.0;
         Max_Hits          : Natural;
         Hits              : Natural := 0;
         Exploding         : Boolean := False;
         Explosion_Timer   : Integer := 0;
         Explosion_Size    : Natural := 0;
      end record;

   overriding function Object_Database
     (Module : Root_Module_Type)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       ("module", Root_Module_Type, Module_Type);

   type Updateable_Reference
     (Item : not null access Root_Module_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.Modules;
