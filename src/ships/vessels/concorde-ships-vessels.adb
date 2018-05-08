with Concorde.Ships.Modules;

package body Concorde.Ships.Vessels is

   ------------------------
   -- Available_Capacity --
   ------------------------

   overriding function Available_Capacity
     (Ship      : Root_Vessel_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return WL.Quantities.Quantity_Type
   is
      Volume : Non_Negative_Real := 0.0;
   begin
      for I in 1 .. Ship.Modules.Last_Index loop
         Volume := Volume
           + Ship.Design.Get_Module (I).Component.Commodity_Payload_Volume
           (Commodity);
      end loop;
      return WL.Quantities.To_Quantity (Float (Volume));
   end Available_Capacity;

   ------------------
   -- Current_Mass --
   ------------------

   overriding function Current_Mass
     (Ship : Root_Vessel_Type)
      return Non_Negative_Real
   is
   begin
      return Mass : Non_Negative_Real := 0.0 do
         for I in 1 .. Ship.Modules.Last_Index loop
            Mass := Mass + Ship.Design.Get_Module (I).Mass
              + Ship.Modules.Element (I).Stock.Total_Mass;
         end loop;
      end return;
   end Current_Mass;

   --------------------
   -- Maximum_Thrust --
   --------------------

   overriding function Maximum_Thrust
     (Ship : Root_Vessel_Type)
      return Non_Negative_Real
   is
   begin
      return Thrust : Non_Negative_Real := 0.0 do
         for I in 1 .. Ship.Modules.Last_Index loop
            declare
               Module : constant Concorde.Ships.Modules.Module_Type :=
                          Ship.Design.Get_Module (I);
            begin
               Thrust := Thrust
                 + Module.Thrust_Component ((0.0, 0.0, 1.0))
                 * Module.Component.Maximum_Thrust;
            end;
         end loop;
      end return;
   end Maximum_Thrust;

   -------------------
   -- Update_Vessel --
   -------------------

   function Update_Vessel
     (Item : not null access constant Root_Vessel_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update_Vessel;

   ------------------------
   -- Variable_Reference --
   ------------------------

   overriding function Variable_Reference
     (Vessel : not null access constant Root_Vessel_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is
   begin
      return Vessel.Update_Vessel.Item;
   end Variable_Reference;

end Concorde.Ships.Vessels;
