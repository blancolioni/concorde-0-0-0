with Ada.Exceptions;

with WL.String_Maps;

with Concorde.Commodities;
with Concorde.Money;

with Concorde.Factions;
with Concorde.Ministries;
with Concorde.People.Communities;
with Concorde.People.Groups;
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

   type Simple_Configure_Function is access
     function (Context : Law_Context;
               Value   : String)
               return Law_Type;

   type Configure_Record is
      record
         General_Handler : Configure_Function;
         Simple_Handler  : Simple_Configure_Function;
      end record;

   package Configure_Function_Maps is
     new WL.String_Maps (Configure_Record);

   Configure_Map : Configure_Function_Maps.Map;

   procedure Create_Configure_Function_Map;

   function Configure_Create_Ministry
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type;

   function Configure_Import_Tariff
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type;

   function Configure_Power_Delegation
     (Context : Law_Context;
      Value   : String)
      return Law_Type;

   function Configure_Power_Creation
     (Context : Law_Context;
      Value   : String)
      return Law_Type;

   function Configure_Sales_Tax
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type;

   -----------------------
   -- Community_Context --
   -----------------------

   function Community_Context
     (Community : not null access constant
        Concorde.People.Communities.Root_Community_Type'Class)
      return Law_Context
   is
   begin
      return Law_Context'
        (Legislator => Concorde.Objects.Object_Type (Community.Owner),
         Target     => Community);
   end Community_Context;

   -------------------------------
   -- Configure_Create_Ministry --
   -------------------------------

   function Configure_Create_Ministry
     (Context : Law_Context;
      Config  : Tropos.Configuration)
      return Law_Type
   is
      Powers : Concorde.Powers.Power_Set;
      Budget : Concorde.Ministries.Ministry_Budget;
      Budget_Config : constant Tropos.Configuration :=
                        Config.Child ("budget");
   begin
      if Budget_Config.Child_Count = 1
        and then Budget_Config.Child (1).Child_Count = 0
      then
         Concorde.Ministries.Add_Budget_Item
           (Budget,
            Concorde.Money.To_Money
              (Real (Float'(Budget_Config.Value))));
      else
         for Item_Config of Budget_Config loop
            if Item_Config.Child_Count = 1
              and then Item_Config.Child (1).Child_Count = 0
            then
               Concorde.Ministries.Add_Budget_Item
                 (Budget,
                  Concorde.Money.To_Money
                    (Real (Float'(Item_Config.Value))));
            elsif Item_Config.Contains ("pop_group") then
               Concorde.Ministries.Add_Budget_Item
                 (Budget,
                  Concorde.People.Groups.Get
                    (Item_Config.Get ("pop_group")),
                  Concorde.Money.To_Price
                    (Real (Float'(Item_Config.Get ("scale")))));
            else
               raise Constraint_Error with
                 "bad budget item";
            end if;
         end loop;
      end if;

      Concorde.Powers.Configure.Configure_Power_Set
        (Config.Child ("powers"), Powers);

      return Concorde.Laws.Bureaucracy.Create_Ministry
        (Context  => Context,
         Name     => Config.Get ("name", "Ministry"),
         Budget   => Budget,
         Powers   => Powers);

   exception
      when E : others =>
         raise Constraint_Error with
           "error while configuring ministry law "
           & Config.Config_Name
           & ": " & Ada.Exceptions.Exception_Message (E);

   end Configure_Create_Ministry;

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
         declare
            Rec : constant Configure_Record :=
                    Configure_Map.Element (Config.Config_Name);
         begin
            if Rec.Simple_Handler /= null
              and then Config.Child_Count = 1
            then
               return Rec.Simple_Handler (Context, Config.Value);
            elsif Rec.General_Handler /= null then
               return Rec.General_Handler (Context, Config);
            else
               raise Constraint_Error with "do not understand config for "
                 & Config.Config_Name;
            end if;
         end;
      else
         raise Constraint_Error with
           "unknown law id: " & Config.Config_Name;
      end if;
   end Configure_Law;

   ------------------------------
   -- Configure_Power_Creation --
   ------------------------------

   function Configure_Power_Creation
     (Context : Law_Context;
      Value   : String)
      return Law_Type
   is
   begin
      return Concorde.Laws.Bureaucracy.Create_Power
        (Context => Context,
         Power   => Concorde.Powers.Configure.Get_Power (Value));
   end Configure_Power_Creation;

   --------------------------------
   -- Configure_Power_Delegation --
   --------------------------------

   function Configure_Power_Delegation
     (Context : Law_Context;
      Value   : String)
      return Law_Type
   is
   begin
      return Concorde.Laws.Bureaucracy.Delegate_Power
        (Context => Context,
         Power   => Concorde.Powers.Configure.Get_Power (Value));
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

      procedure Configure
        (Name    : String;
         General : Configure_Function;
         Simple  : Simple_Configure_Function);

      procedure Configure
        (Name : String;
         Config : Simple_Configure_Function);

      procedure Configure
        (Name   : String;
         Config : Configure_Function);

      ---------------
      -- Configure --
      ---------------

      procedure Configure
        (Name    : String;
         General : Configure_Function;
         Simple  : Simple_Configure_Function)
      is
      begin
         Configure_Map.Insert
           (Name, (General, Simple));
      end Configure;

      ---------------
      -- Configure --
      ---------------

      procedure Configure
        (Name   : String;
         Config : Simple_Configure_Function)
      is
      begin
         Configure (Name, null, Config);
      end Configure;

      ---------------
      -- Configure --
      ---------------

      procedure Configure
        (Name   : String;
         Config : Configure_Function)
      is
      begin
         Configure (Name, Config, null);
      end Configure;

   begin
      Configure ("create_ministry", Configure_Create_Ministry'Access);
      Configure ("delegate_power", Configure_Power_Delegation'Access);
      Configure ("create_power", Configure_Power_Creation'Access);
      Configure ("import_tariff", Configure_Import_Tariff'Access);
      Configure ("sales_tax", Configure_Sales_Tax'Access);
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
         Target     => Individual);
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
         Target     => Vassal);
   end Vassal_Context;

end Concorde.Laws.Configure;
