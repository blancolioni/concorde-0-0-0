with Concorde.Ministries;

package body Concorde.Powers is

   ---------------
   -- Add_Power --
   ---------------

   overriding procedure Add_Power
     (Container : in out Power_Set;
      Power     : Power_Type)
   is
   begin
      if not Container.Has_Power (Power) then
         Container.Set.Append (Power);
      end if;
   end Add_Power;

   ------------------
   -- Check_Powers --
   ------------------

   overriding function Check_Powers
     (Container : Power_Set;
      Test      : not null access
        function (Power : Power_Type) return Boolean)
      return Boolean
   is
   begin
      for Power of Container.Set loop
         if not Test (Power) then
            return False;
         end if;
      end loop;
      return True;
   end Check_Powers;

   ----------------------
   -- Class_Identifier --
   ----------------------

   function Class_Identifier (Power : Power_Type) return String is
   begin
      case Power.Class is
         when Set_Tax_Rate =>
            return "set_tax_rate";
         when Collect_Tax =>
            case Power.Tax_Category is
               when Concorde.Trades.Sales =>
                  return "collect_sales_tax";
               when Concorde.Trades.Import =>
                  return "collect_import_tariffs";
               when Concorde.Trades.Export =>
                  return "collect_export_tariffs";
            end case;
         when Appoint_Minister =>
            return "appoint_minister";
         when Direct_Minister =>
            return "direct_minister";
         when Law_Enforcement =>
            return "law_enforcement";
         when Appoint_General =>
            return "appoint_general";
         when Command_Army =>
            return "command_army";
      end case;
   end Class_Identifier;

   --------------
   -- Contains --
   --------------

   overriding function Has_Power
     (Container : Power_Set;
      Power     : Power_Type)
      return Boolean
   is
   begin
      return Power_Lists.Has_Element (Container.Set.Find (Power));
   end Has_Power;

   ----------------
   -- Identifier --
   ----------------

   function Identifier (Power : Power_Type) return String is
   begin
      case Power.Class is
         when Set_Tax_Rate =>
            return Class_Identifier (Power);
         when Collect_Tax =>
            return Class_Identifier (Power);
         when Appoint_Minister =>
            return Class_Identifier (Power);
         when Direct_Minister =>
            return Class_Identifier (Power) & "_" & Power.Ministry.Identifier;
         when Law_Enforcement =>
            return Class_Identifier (Power);
         when Appoint_General =>
            return Class_Identifier (Power);
         when Command_Army =>
            return Class_Identifier (Power);
      end case;
   end Identifier;

   ------------
   -- Remove --
   ------------

   overriding procedure Remove_Power
     (Container : in out Power_Set;
      Power     : Power_Type)
   is
      use Power_Lists;
      Position : Cursor := Container.Set.Find (Power);
   begin
      if Has_Element (Position) then
         Container.Set.Delete (Position);
      end if;
   end Remove_Power;

   -----------------
   -- Scan_Powers --
   -----------------

   overriding procedure Scan_Powers
     (Container : Power_Set;
      Process   : not null access
        procedure (Power : Power_Type))
   is
   begin
      for Power of Container.Set loop
         Process (Power);
      end loop;
   end Scan_Powers;

   ----------
   -- Show --
   ----------

   function Show (Power : Power_Type) return String is
   begin
      case Power.Class is
         when Set_Tax_Rate =>
            case Power.Tax_Category is
               when Concorde.Trades.Sales =>
                  return "set sales tax rate";
               when Concorde.Trades.Import =>
                  return "set import tariffs";
               when Concorde.Trades.Export =>
                  return "set export tariffs";
            end case;
         when Collect_Tax =>
            case Power.Tax_Category is
               when Concorde.Trades.Sales =>
                  return "collect sales tax";
               when Concorde.Trades.Import =>
                  return "collect import tariffs";
               when Concorde.Trades.Export =>
                  return "collect export tariffs";
            end case;
         when Appoint_Minister =>
            return "appoint minister";
         when Direct_Minister =>
            return "direct minister of " & Power.Ministry.Name;
         when Law_Enforcement =>
            return "law enforcement";
         when Appoint_General =>
            return "appoint general";
         when Command_Army =>
            return "command army";
      end case;
   end Show;

end Concorde.Powers;
