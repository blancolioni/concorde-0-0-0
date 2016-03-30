package body Concorde.Modules is

   ------------
   -- Charge --
   ------------

   function Charge
     (Module : Root_Module_Type'Class)
      return Unit_Real
   is
   begin
      if Module.Max_Stored_Energy = 0.0 then
         return 0.0;
      else
         return Module.Stored_Energy / Module.Max_Stored_Energy;
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
      return Real (Module.Hits) / Real (Module.Max_Hits);
   end Damage;

   ----------------
   -- Draw_Power --
   ----------------

   procedure Draw_Power
     (Module : in out Root_Module_Type'Class;
      Power  : Non_Negative_Real)
   is
   begin
      Module.Stored_Energy :=
        Non_Negative_Real'Min (Module.Stored_Energy + Power,
                               Module.Max_Stored_Energy);
      Module.Heat := Module.Heat + Power;
   end Draw_Power;

   -------------------
   -- Effectiveness --
   -------------------

   function Effectiveness
     (Module : Root_Module_Type'Class)
      return Unit_Real
   is
   begin
      return 1.0 - Module.Damage ** 2;
   end Effectiveness;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Module : in out Root_Module_Type'Class)
   is
   begin
      Module.Heat := Module.Heat + Module.Stored_Energy;
      Module.Stored_Energy := 0.0;
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

   ----------------------
   -- Explosion_Chance --
   ----------------------

   function Explosion_Chance
     (Module : Root_Module_Type'Class)
      return Unit_Real
   is
   begin
      return (1.0 - Module.Component.Explosion_Chance) ** Module.Hits;
   end Explosion_Chance;

   --------------------
   -- Explosion_Size --
   --------------------

   function Explosion_Size
     (Module : Root_Module_Type'Class)
      return Natural
   is
   begin
      return Module.Explosion_Size;
   end Explosion_Size;

   ---------------------
   -- Explosion_Timer --
   ---------------------

   function Explosion_Timer
     (Module : Root_Module_Type'Class)
      return Integer
   is
   begin
      return Module.Explosion_Timer;
   end Explosion_Timer;

   ---------
   -- Hit --
   ---------

   procedure Hit
     (Module         : in out Root_Module_Type'Class)
   is
   begin
      if Module.Hits < Module.Volume then
         Module.Hits := Module.Hits + 1;
      end if;
   end Hit;

   -------------------
   -- Initial_State --
   -------------------

   procedure Initial_State
     (Module : in out Root_Module_Type'Class)
   is
   begin
      Module.Max_Stored_Energy :=
        Module.Component.Maximum_Stored_Energy (Module.Volume);
      Module.Stored_Energy := 0.0;
      Module.Heat := 0.0;
      Module.Exploding := False;
      Module.Hits := 0;
   end Initial_State;

   ----------
   -- Mass --
   ----------

   function Mass
     (Module : Root_Module_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Module.Component.Mass (Module.Volume);
   end Mass;

   ------------------------
   -- Maximum_Power_Draw --
   ------------------------

   function Maximum_Power_Draw
     (Module : in out Root_Module_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Module.Component.Maximum_Power_Draw (Module.Volume);
   end Maximum_Power_Draw;

   ----------------
   -- New_Module --
   ----------------

   function New_Module
     (Name      : String;
      Component : Concorde.Components.Component_Type;
      Size      : Size_Type)
      return Module_Type
   is
      Volume : constant Positive := Size.X * Size.Y * Size.Z;
      Module : constant Module_Type :=
                 new Root_Module_Type'
                   (Concorde.Objects.Root_Named_Object_Type with
                    Component         => Component,
                    Size              => Size,
                    Volume            => Volume,
                    Max_Stored_Energy =>
                      Component.Maximum_Stored_Energy (Volume),
                    Stored_Energy     => 0.0,
                    Heat              => 0.0,
                    Hits              => 0,
                    Max_Hits          => Volume * 10,
                    Exploding         => False,
                    Explosion_Timer   => 0,
                    Explosion_Size    => 0);
   begin
      Module.Set_Name (Name);
      return Module;
   end New_Module;

   ----------
   -- Size --
   ----------

   function Size
     (Module : Root_Module_Type'Class)
      return Size_Type
   is
   begin
      return Module.Size;
   end Size;

   ---------------------
   -- Start_Explosion --
   ---------------------

   procedure Start_Explosion
     (Module : in out Root_Module_Type'Class)
   is
   begin
      Module.Exploding := True;
      Module.Explosion_Timer :=
        Module.Max_Hits - Module.Hits;
      Module.Explosion_Size := Module.Volume * 3;
   end Start_Explosion;

   -------------------
   -- Stored_Energy --
   -------------------

   function Stored_Energy
     (Module : Root_Module_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Module.Stored_Energy;
   end Stored_Energy;

   -------------------
   -- Update_Damage --
   -------------------

   procedure Update_Damage
     (Module : in out Root_Module_Type'Class)
   is
   begin
      if Module.Exploding then
         Module.Explosion_Timer := Module.Explosion_Timer - 1;
         if Module.Explosion_Timer = 0 then
            Module.Hits := Module.Max_Hits;
         end if;
      end if;
   end Update_Damage;

end Concorde.Modules;