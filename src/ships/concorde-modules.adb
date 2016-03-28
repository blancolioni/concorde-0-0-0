with Concorde.Random;

package body Concorde.Modules is

   ------------
   -- Charge --
   ------------

   function Charge
     (Module : Root_Module_Type'Class)
      return Unit_Real
   is
      Max_Energy : constant Natural := Module.Component.Max_Stored_Energy;
   begin
      if Max_Energy = 0 then
         return 0.0;
      else
         return Real (Module.Energy)
           / Real (Module.Component.Max_Stored_Energy);
      end if;
   end Charge;

   ---------------
   -- Component --
   ---------------

   function Component
     (Module : Root_Module_Type'Class)
      return Concorde.Components.Component_Type
   is
   begin
      return Module.Component;
   end Component;

   ------------
   -- Damage --
   ------------

   function Damage
     (Module : Root_Module_Type'Class)
      return Unit_Real
   is
   begin
      return 1.0 - Real (Module.HP) / Real (Module.Component.Max_Hits);
   end Damage;

   ----------------
   -- Draw_Power --
   ----------------

   procedure Draw_Power
     (Module : in out Root_Module_Type'Class;
      Power  : Natural)
   is
   begin
      if Module.Energy < Module.Component.Max_Stored_Energy then
         Module.Energy := Module.Energy + 1;
      end if;
      Module.Heat := Module.Heat + Power + 1;
   end Draw_Power;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Module : in out Root_Module_Type'Class)
   is
   begin
      Module.Heat := Module.Heat + Module.Energy;
      Module.Energy := 0;
   end Execute;

   ---------------
   -- Exploding --
   ---------------

   function Exploding
     (Module : Root_Module_Type'Class)
      return Boolean
   is
   begin
      return Module.Exploding;
   end Exploding;

   ---------
   -- Hit --
   ---------

   procedure Hit
     (Module         : in out Root_Module_Type'Class)
   is
   begin
      if Module.HP > 0 then
         Module.HP := Module.HP - 1;
      end if;

      declare
         Explode : constant Unit_Real :=
                     Module.Component.Explosion_Chance
                       (Module.HP);
      begin
         if Concorde.Random.Unit_Random < Explode then
            Module.Exploding := True;
         end if;
      end;
   end Hit;

   -------------------
   -- Initial_State --
   -------------------

   procedure Initial_State
     (Module : in out Root_Module_Type'Class)
   is
   begin
      Module.Energy := Module.Component.Max_Stored_Energy / 2;
      Module.Heat := 0;
      Module.Exploding := False;
   end Initial_State;

   ----------
   -- Mass --
   ----------

   function Mass
     (Module : Root_Module_Type'Class)
      return Natural
   is
   begin
      return Module.Mass;
   end Mass;

   ----------------
   -- New_Module --
   ----------------

   function New_Module
     (Component : Concorde.Components.Component_Type)
      return Module_Type
   is
   begin
      return new Root_Module_Type'
        (Concorde.Objects.Root_Object_Type with
           Component => Component,
         Mass      => Component.Mass,
         HP        => Component.Max_Hits,
         Energy    => 0,
         Heat      => 0,
         Exploding => False);
   end New_Module;

   ----------
   -- Size --
   ----------

   function Size
     (Module : Root_Module_Type'Class)
      return Natural
   is
   begin
      return Module.Component.Size;
   end Size;

   -------------------
   -- Stored_Energy --
   -------------------

   function Stored_Energy
     (Module : Root_Module_Type'Class)
      return Natural
   is
   begin
      return Module.Energy;
   end Stored_Energy;

end Concorde.Modules;
