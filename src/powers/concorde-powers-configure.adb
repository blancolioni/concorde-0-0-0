with WL.String_Maps;

with Concorde.Trades;

with Concorde.Powers.Armies;
with Concorde.Powers.Ministries;
with Concorde.Powers.Ships;
with Concorde.Powers.Taxation;

with Concorde.Powers.Execution;

with Concorde.People.Groups;

package body Concorde.Powers.Configure is

   type Configure_Function is access
     function (Config : Tropos.Configuration) return Power_Type;

   package Configure_Maps is new WL.String_Maps (Configure_Function);

   Configure_Map : Configure_Maps.Map;

   package Simple_Maps is new WL.String_Maps (Power_Type);

   Simple_Map : Simple_Maps.Map;

   procedure Create_Map;

   function Configure_Pay_Power
     (Config : Tropos.Configuration)
      return Power_Type;

   -------------------------
   -- Configure_Pay_Power --
   -------------------------

   function Configure_Pay_Power
     (Config : Tropos.Configuration)
      return Power_Type
   is
   begin
      return Concorde.Powers.Ministries.Pay
        (Concorde.People.Groups.Get (Config.Value));
   exception
      when others =>
         raise Constraint_Error with
           "no such group " & Config.Value
           & " in configuration for pay power";
   end Configure_Pay_Power;

   ---------------------
   -- Configure_Power --
   ---------------------

   function Configure_Power
     (Config : Tropos.Configuration)
      return Power_Type
   is
   begin
      if Simple_Map.Is_Empty then
         Create_Map;
      end if;

      if Simple_Map.Contains (Config.Config_Name) then
         return Simple_Map.Element (Config.Config_Name);
      elsif Configure_Map.Contains (Config.Config_Name) then
         declare
            Fn : constant Configure_Function :=
                    Configure_Map.Element (Config.Config_Name);
         begin
            return Fn (Config);
         end;
      else
         raise Constraint_Error with
           "no such power: " & Config.Config_Name;
      end if;
   end Configure_Power;

   -------------------------------
   -- Configure_Power_Execution --
   -------------------------------

   procedure Configure_Power_Execution
     (Config : Tropos.Configuration)
   is
   begin
      Concorde.Powers.Execution.Configure_Power_Execution (Config);
   end Configure_Power_Execution;

   -------------------------
   -- Configure_Power_Set --
   -------------------------

   procedure Configure_Power_Set
     (Config : Tropos.Configuration;
      Set    : in out Power_Set)
   is
   begin
      for Power_Config of Config loop
         Set.Add_Power
           (Configure_Power (Power_Config));
      end loop;
   end Configure_Power_Set;

   ----------------
   -- Create_Map --
   ----------------

   procedure Create_Map is
   begin
      Simple_Map.Insert
        ("set_sales_tax",
         Concorde.Powers.Taxation.Set_Tax_Rate (Concorde.Trades.Sales));
      Simple_Map.Insert
        ("collect_sales_tax",
         Concorde.Powers.Taxation.Collect_Tax (Concorde.Trades.Sales));
      Simple_Map.Insert
        ("set_import_tariff",
         Concorde.Powers.Taxation.Set_Tax_Rate (Concorde.Trades.Import));
      Simple_Map.Insert
        ("collect_import_tariff",
         Concorde.Powers.Taxation.Collect_Tax (Concorde.Trades.Import));
      Simple_Map.Insert
        ("set_export_tariff",
         Concorde.Powers.Taxation.Set_Tax_Rate (Concorde.Trades.Export));
      Simple_Map.Insert
        ("collect_export_tariff",
         Concorde.Powers.Taxation.Collect_Tax (Concorde.Trades.Export));
      Simple_Map.Insert
        ("appoint_minister", Concorde.Powers.Ministries.Appoint_Minister);
      Simple_Map.Insert
        ("appoint_general", Concorde.Powers.Armies.Appoint_General);
      Simple_Map.Insert
        ("appoint_trader_captain",
         Concorde.Powers.Ships.Appoint_Trader_Captain);
      Simple_Map.Insert
        ("law_enforcement", Concorde.Powers.Ministries.Law_Enforcement);
      Configure_Map.Insert
        ("pay", Configure_Pay_Power'Access);

   end Create_Map;

   ---------------
   -- Get_Power --
   ---------------

   function Get_Power (Name : String) return Power_Type is
   begin
      return Configure_Power
        (Tropos.New_Config (Name));
   end Get_Power;

end Concorde.Powers.Configure;
