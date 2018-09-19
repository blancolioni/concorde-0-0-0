with Concorde.Money;
with WL.String_Maps;

with Concorde.Ships.Components;

package body Concorde.Ships.Designs is

   --------------------
   -- Cargo_Capacity --
   --------------------

   function Cargo_Capacity
     (Design : Root_Design_Type'Class)
      return Concorde.Quantities.Quantity_Type
   is
      Volume : Non_Negative_Real := 0.0;
   begin
      for Installed of Design.Installed_Modules loop
         Volume := Volume + Installed.Module.Component.Cargo_Payload_Volume;
      end loop;
      return Concorde.Quantities.To_Quantity (Volume);
   end Cargo_Capacity;

   ---------------------------
   -- Get_Fuel_Requirements --
   ---------------------------

   procedure Get_Fuel_Requirements
     (Design       : Root_Design_Type'Class;
      Requirements : in out Concorde.Commodities.Stock_Interface'Class)
   is
      package Fuel_Info_Maps is
        new WL.String_Maps (Non_Negative_Real);

      Cryo_Fuel : Fuel_Info_Maps.Map;
      Normal_Fuel : Fuel_Info_Maps.Map;
      Total_Cryo : Non_Negative_Real := 0.0;
      Total_Normal : Non_Negative_Real := 0.0;

      Number_Of_Tanks      : Natural := 0;
      Number_Of_Cryo_Tanks : Natural := 0;
      Volume_Of_Tanks      : Non_Negative_Real := 0.0;
      Volume_Of_Cryo_Tanks : Non_Negative_Real := 0.0;

      procedure Update_Fuel_Ratios
        (Commodity : Concorde.Commodities.Commodity_Type;
         Ratio     : Non_Negative_Real);

      ------------------------
      -- Update_Fuel_Ratios --
      ------------------------

      procedure Update_Fuel_Ratios
        (Commodity : Concorde.Commodities.Commodity_Type;
         Ratio     : Non_Negative_Real)
      is
      begin
         if Commodity.Is_Set (Concorde.Commodities.Cryogenic) then
            if Cryo_Fuel.Contains (Commodity.Identifier) then
               Cryo_Fuel.Replace
                 (Commodity.Identifier,
                  Cryo_Fuel.Element (Commodity.Identifier) + Ratio);
            else
               Cryo_Fuel.Insert (Commodity.Identifier, Ratio);
            end if;
            Total_Cryo := Total_Cryo + Ratio;
         else
            if Normal_Fuel.Contains (Commodity.Identifier) then
               Normal_Fuel.Replace
                 (Commodity.Identifier,
                  Normal_Fuel.Element (Commodity.Identifier) + Ratio);
            else
               Normal_Fuel.Insert (Commodity.Identifier, Ratio);
            end if;
            Total_Normal := Total_Normal + Ratio;
         end if;
      end Update_Fuel_Ratios;

   begin

      for Module of Design.Installed_Modules loop
         declare
            Component : constant Concorde.Ships.Components.Component_Type :=
                          Module.Module.Component;
         begin
            Component.Scan_Propellant
              (Update_Fuel_Ratios'Access);
            if Component.Is_Tank then
               if Component.Is_Cryogenic then
                  Number_Of_Cryo_Tanks := Number_Of_Cryo_Tanks + 1;
                  Volume_Of_Cryo_Tanks := Volume_Of_Cryo_Tanks
                    + Component.Propellant_Payload_Volume;
               else
                  Number_Of_Tanks := Number_Of_Tanks + 1;
                  Volume_Of_Tanks := Volume_Of_Tanks
                    + Component.Propellant_Payload_Volume;
               end if;
            end if;
         end;
      end loop;

      Requirements.Clear_Stock;

      for Position in Cryo_Fuel.Iterate loop
         declare
            Volume : constant Non_Negative_Real :=
                       Fuel_Info_Maps.Element (Position)
                       * Volume_Of_Cryo_Tanks / Total_Cryo;
         begin
            Requirements.Add_Quantity
              (Item     =>
                 Concorde.Commodities.Get
                   (Fuel_Info_Maps.Key (Position)),
               Quantity => Concorde.Quantities.To_Quantity (Volume),
               Value    => Concorde.Money.To_Money (Volume));
         end;
      end loop;
   end Get_Fuel_Requirements;

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Item : Root_Design_Type)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Identifier);
   end Identifier;

end Concorde.Ships.Designs;
