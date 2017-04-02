with Concorde.Components.Db;

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

   ------------
   -- Colour --
   ------------

   function Colour
     (Component : Root_Component_Type'Class)
      return Lui.Colours.Colour_Type
   is
   begin
      return Component.Colour;
   end Colour;

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

   --------------------
   -- Maximum_Output --
   --------------------

   function Maximum_Output
     (Component : Root_Component_Type'Class;
      Volume    : Positive)
      return Non_Negative_Real
   is
   begin
      return Component.Nominal_Max_Output
        * Non_Negative_Real (Volume)
        * Component.Output_Size_Power ** Volume;
   end Maximum_Output;

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

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Component : Root_Component_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Component);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------
   -- Shape --
   -----------

   function Shape
     (Component : Root_Component_Type'Class)
      return Component_Shape
   is
   begin
      return Component.Shape;
   end Shape;

   -------------------
   -- Throttle_Step --
   -------------------

   function Throttle_Step
     (Component : Root_Component_Type'Class)
      return Unit_Real
   is
   begin
      return Component.Throttle_Step;
   end Throttle_Step;

end Concorde.Components;
