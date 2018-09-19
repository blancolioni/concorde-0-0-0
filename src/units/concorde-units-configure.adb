with Concorde.Quantities;

with Concorde.Commodities.Configure;

package body Concorde.Units.Configure is

   --------------------
   -- Configure_Unit --
   --------------------

   procedure Configure_Unit
     (Config : Tropos.Configuration)
   is
      procedure Create (Unit : in out Root_Unit_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Unit : in out Root_Unit_Type'Class) is
         Construct : Concorde.Commodities.Root_Stock_Type;
         Maintain  : Concorde.Commodities.Root_Stock_Type;
      begin
         Unit.Set_Local_Tag (Config.Config_Name);
         Unit.Movement := Movement_Category'Value (Config.Get ("type"));
         Unit.Speed := Config.Get ("maximum_speed");
         Unit.Recon := Config.Get ("reconnaissance");
         Unit.Cargo := Config.Get ("cargo", 0);
         Unit.Can_Be_Cargo := Config.Get ("can-be-cargo", False);
         Unit.Combat := Config.Get ("combat", False);
         Unit.Priority := Config.Get ("priority", 0);
         Unit.Image_Resource :=
           Ada.Strings.Unbounded.To_Unbounded_String (Config.Get ("icon", ""));

         Construct.Create_Stock
           (Concorde.Quantities.To_Quantity (100_000.0), False);
         Maintain.Create_Stock
           (Concorde.Quantities.To_Quantity (1_000.0), False);

         Concorde.Commodities.Configure.Configure_Stock
           (Config.Child ("build_cost"), Construct, 1000.0);
         Concorde.Commodities.Configure.Configure_Stock
           (Config.Child ("supply_cost"), Maintain, 1000.0);

         Unit.Construct.Create (Construct, Maintain);

      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Unit;

end Concorde.Units.Configure;
