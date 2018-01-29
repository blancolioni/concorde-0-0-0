with WL.String_Maps;

with Concorde.Commodities;

with Concorde.People.Individuals;
with Concorde.Worlds;

with Concorde.Laws.Bureaucracy;
with Concorde.Laws.Tax_Laws;

with Concorde.Powers.Configure;

package body Concorde.Laws.Configure is

   type Configure_Function is access
     function (Context : Law_Context;
               Config  : Tropos.Configuration)
               return Law_Type;

   package Configure_Function_Maps is
     new WL.String_Maps (Configure_Function);

   Configure_Map : Configure_Function_Maps.Map;

   procedure Create_Configure_Function_Map;

   function Configure_Power_Delegation
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type;

   function Configure_Import_Tariff
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type;

   function Configure_Sales_Tax
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type;

   -----------------------------
   -- Configure_Import_Tariff --
   -----------------------------

   function Configure_Import_Tariff
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type
   is
   begin
      if Config.Contains ("commodity") then
         return Tax_Laws.Import_Tariff
           (Context   => Context,
            Commodity => Commodities.Get (Config.Get ("commodity")),
            Rate      => Unit_Real (Float'(Config.Get ("rate"))));
      else
         return Tax_Laws.Import_Tariff
           (Context   => Context,
            Rate      => Unit_Real (Float'(Config.Value)));
      end if;
   end Configure_Import_Tariff;

   -------------------
   -- Configure_Law --
   -------------------

   function Configure_Law
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type
   is
   begin
      if Configure_Map.Is_Empty then
         Create_Configure_Function_Map;
      end if;

      if Configure_Map.Contains (Config.Config_Name) then
         return Configure_Map.Element (Config.Config_Name)
           (Context, Config);
      else
         raise Constraint_Error with
           "unknown law id: " & Config.Config_Name;
      end if;
   end Configure_Law;

   --------------------------------
   -- Configure_Power_Delegation --
   --------------------------------

   function Configure_Power_Delegation
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type
   is
   begin
      return Concorde.Laws.Bureaucracy.Delegate_Power
        (Context => Context,
         Power   =>
           Concorde.Powers.Configure.Configure_Power (Config.Child (1)));
   end Configure_Power_Delegation;

   -------------------------
   -- Configure_Sales_Tax --
   -------------------------

   function Configure_Sales_Tax
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type
   is
   begin
      if Config.Contains ("commodity") then
         return Tax_Laws.Sales_Tax
           (Context   => Context,
            Commodity => Commodities.Get (Config.Get ("commodity")),
            Rate      => Unit_Real (Float'(Config.Get ("rate"))));
      else
         return Tax_Laws.Sales_Tax
           (Context   => Context,
            Rate      => Unit_Real (Float'(Config.Value)));
      end if;
   end Configure_Sales_Tax;

   -----------------------------------
   -- Create_Configure_Function_Map --
   -----------------------------------

   procedure Create_Configure_Function_Map is
   begin
      Configure_Map.Insert
        ("delegate_power", Configure_Power_Delegation'Access);
      Configure_Map.Insert
        ("import_tariff", Configure_Import_Tariff'Access);
      Configure_Map.Insert
        ("sales_tax", Configure_Sales_Tax'Access);
   end Create_Configure_Function_Map;

   ------------------------
   -- Individual_Context --
   ------------------------

   function Individual_Context
     (Individual : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
      return Law_Context
   is
   begin
      return Law_Context'
        (Legislator => Concorde.Objects.Object_Type (Individual.Faction),
         Target     => Concorde.Objects.Object_Type (Individual));
   end Individual_Context;

   --------------------
   -- Vassal_Context --
   --------------------

   function Vassal_Context
     (Ruler  : not null access constant
        Concorde.Factions.Root_Faction_Type'Class;
      Vassal : not null access constant
        Concorde.Factions.Root_Faction_Type'Class)
      return Law_Context
   is
   begin
      return Law_Context'
        (Legislator => Concorde.Objects.Object_Type (Ruler),
         Target     => Concorde.Objects.Object_Type (Vassal));
   end Vassal_Context;

   -------------------
   -- World_Context --
   -------------------

   function World_Context
     (World : not null access constant
        Concorde.Worlds.Root_World_Type'Class)
      return Law_Context
   is
   begin
      return Law_Context'
        (Legislator => Concorde.Objects.Object_Type (World.Owner),
         Target     => Concorde.Objects.Object_Type (World));
   end World_Context;

end Concorde.Laws.Configure;
