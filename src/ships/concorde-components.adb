package body Concorde.Components is

   -----------
   -- Class --
   -----------

   function Class
     (Component : Root_Component_Type'Class)
      return Component_Class
   is
   begin
      return Component.Class;
   end Class;

   -------------------------
   -- Configure_Component --
   -------------------------

   procedure Configure_Component
     (Component : in out Root_Component_Type'Class;
      Config    : Tropos.Configuration)
   is
   begin
      Component.Mass := Config.Get ("mass");
      Component.Size := Config.Get ("size");
      Component.Power_Draw := Config.Get ("power");
      Component.Energy_Store := Config.Get ("energy");
      Component.Heat_Production := Config.Get ("heat");
      Component.Max_Hits := Config.Get ("hp");

      declare
         Explode : constant Tropos.Configuration :=
                     Config.Child ("explode");
      begin
         Component.Explode :=
           new Explode_Chance (1 .. Component.Max_Hits);
         for I in Component.Explode'Range loop
            Component.Explode (I) := Explode.Get (I) / 100.0;
         end loop;
      end;

   end Configure_Component;

   ----------------------
   -- Explosion_Chance --
   ----------------------

   function Explosion_Chance
     (Component    : Root_Component_Type'Class;
      Remaining_HP : Natural)
      return Unit_Real
   is
   begin
      return Component.Explode (Remaining_HP + 1);
   end Explosion_Chance;

   ----------
   -- Mass --
   ----------

   function Mass
     (Component : Root_Component_Type'Class)
      return Natural
   is
   begin
      return Component.Mass;
   end Mass;

   --------------
   -- Max_Hits --
   --------------

   function Max_Hits
     (Component : Root_Component_Type'Class)
      return Natural
   is
   begin
      return Component.Max_Hits;
   end Max_Hits;

   --------------------
   -- Max_Power_Draw --
   --------------------

   function Max_Power_Draw
     (Component : Root_Component_Type'Class)
      return Natural
   is
   begin
      return Component.Power_Draw;
   end Max_Power_Draw;

   -----------------------
   -- Max_Stored_Energy --
   -----------------------

   function Max_Stored_Energy
     (Component : Root_Component_Type'Class)
      return Natural
   is
   begin
      return Component.Energy_Store;
   end Max_Stored_Energy;

   ----------
   -- Size --
   ----------

   function Size
     (Component : Root_Component_Type'Class)
      return Natural
   is
   begin
      return Component.Size;
   end Size;

end Concorde.Components;
