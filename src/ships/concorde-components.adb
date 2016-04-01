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

   ----------------------
   -- Effective_Damage --
   ----------------------

   function Effective_Damage
     (Component     : Root_Component_Type'Class;
      Power         : Non_Negative_Real;
      Effectiveness : Unit_Real;
      At_Range      : Non_Negative_Real)
      return Natural
   is
      Damage : Non_Negative_Real := Power * Effectiveness;
   begin
      if At_Range > Component.Nominal_Half_Range then
         Damage := Damage * Component.Nominal_Half_Range / At_Range;
      end if;
      return Natural (Damage);
   end Effective_Damage;

   ----------------------
   -- Explosion_Chance --
   ----------------------

   function Explosion_Chance
     (Component    : Root_Component_Type'Class)
      return Unit_Real
   is
   begin
      return Component.Explosion_Chance;
   end Explosion_Chance;

   ----------
   -- Mass --
   ----------

   function Mass
     (Component : Root_Component_Type'Class;
      Volume    : Positive)
      return Non_Negative_Real
   is
   begin
      return Real (Volume) * Component.Density;
   end Mass;

   ------------------------
   -- Maximum_Power_Draw --
   ------------------------

   function Maximum_Power_Draw
     (Component : Root_Component_Type'Class;
      Volume    : Positive)
      return Non_Negative_Real
   is
   begin
      return Component.Nominal_Power_Draw
        * Real (Volume);
   end Maximum_Power_Draw;

   ---------------------------
   -- Maximum_Stored_Energy --
   ---------------------------

   function Maximum_Stored_Energy
     (Component : Root_Component_Type'Class;
      Volume    : Positive)
      return Non_Negative_Real
   is
   begin
      return Component.Nominal_Charge
        * Non_Negative_Real (Volume)
        * Component.Charge_Size_Power ** Volume;
   end Maximum_Stored_Energy;

end Concorde.Components;
