with WL.String_Maps;

package body Concorde.Powers.Configure is

   type Simple_Configure_Function is access
     function (Name : String) return Power_Type;

   type Configuration_Record is
      record
         Simple : Simple_Configure_Function;
      end record;

   package Configure_Maps is new WL.String_Maps (Configuration_Record);

   Configure_Map : Configure_Maps.Map;

   package Simple_Maps is new WL.String_Maps (Power_Type);

   Simple_Map : Simple_Maps.Map;

   procedure Create_Map;

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
            Rec : constant Configuration_Record :=
                    Configure_Map.Element (Config.Config_Name);
         begin
            return Rec.Simple (Config.Config_Name);
         end;
      else
         raise Constraint_Error with
           "no such power: " & Config.Config_Name;
      end if;
   end Configure_Power;

   -------------------------
   -- Configure_Power_Set --
   -------------------------

   procedure Configure_Power_Set
     (Config : Tropos.Configuration;
      Set    : in out Power_Set)
   is
   begin
      for Power_Config of Config loop
         Set.Insert
           (Configure_Power (Power_Config));
      end loop;
   end Configure_Power_Set;

   ----------------
   -- Create_Map --
   ----------------

   procedure Create_Map is
   begin
      Simple_Map.Insert
        ("set_sales_tax", (Set_Tax_Rate, Concorde.Trades.Sales));
      Simple_Map.Insert
        ("collect_sales_tax", (Collect_Tax, Concorde.Trades.Sales));
      Simple_Map.Insert
        ("set_import_tariff", (Set_Tax_Rate, Concorde.Trades.Import));
      Simple_Map.Insert
        ("collect_import_tariff", (Collect_Tax, Concorde.Trades.Import));
      Simple_Map.Insert
        ("set_export_tariff", (Set_Tax_Rate, Concorde.Trades.Export));
      Simple_Map.Insert
        ("collect_export_tariff", (Collect_Tax, Concorde.Trades.Export));
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
