private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Concorde.Trades;

with Concorde.Markets;

limited with Concorde.Ministries;
--  limited with Concorde.Ships;

package Concorde.Powers is

   type Power_Type (<>) is private;

   function Show (Power : Power_Type) return String;

   function Set_Tax_Rate
     (Category : Concorde.Trades.Market_Tax_Category)
      return Power_Type;

   function Collect_Tax
     (Market   : Concorde.Markets.Market_Type;
      Category : Concorde.Trades.Market_Tax_Category)
      return Power_Type;

   function Appoint_Minister return Power_Type;

   function Appoint_General return Power_Type;

   function Appoint_Trader_Captain return Power_Type;

   function Direct_Minister
     (Ministry : not null access constant
        Concorde.Ministries.Root_Ministry_Type'Class)
     return Power_Type;

   function Law_Enforcement return Power_Type;

   function Command_Army return Power_Type;

--     function Command_Trade_Ship
--       (Ship : not null access Concorde.Ships.Root_Vessel_Type'Class)
--        return Power_Type;
--
--     function Command_Naval_Ship
--       (Ship : not null access Concorde.Ships.Root_Vessel_Type'Class)
--        return Power_Type;

   type Powered_Interface is limited interface;

   function Has_Power
     (Container : Powered_Interface;
      Power     : Power_Type)
      return Boolean
      is abstract;

   procedure Add_Power
     (Container : in out Powered_Interface;
      Power     : Power_Type)
   is abstract;

   procedure Remove_Power
     (Container : in out Powered_Interface;
      Power     : Power_Type)
   is abstract;

   procedure Scan_Powers
     (Container : Powered_Interface;
      Process   : not null access
        procedure (Power : Power_Type))
   is abstract;

   function Check_Powers
     (Container : Powered_Interface;
      Test      : not null access
        function (Power : Power_Type) return Boolean)
      return Boolean
      is abstract;

   type Power_Set is new Powered_Interface with private;

   function No_Powers return Power_Set;

   overriding function Has_Power
     (Container : Power_Set;
      Power     : Power_Type)
      return Boolean;

   overriding procedure Add_Power
     (Container : in out Power_Set;
      Power     : Power_Type);

   overriding procedure Remove_Power
     (Container : in out Power_Set;
      Power     : Power_Type);

   overriding function Check_Powers
     (Container : Power_Set;
      Test      : not null access
        function (Power : Power_Type) return Boolean)
      return Boolean;

   overriding procedure Scan_Powers
     (Container : Power_Set;
      Process   : not null access
        procedure (Power : Power_Type));

private

   type Ministry_Access is access constant
     Concorde.Ministries.Root_Ministry_Type'Class;

--     type Ship_Access is access constant
--       Concorde.Ships.Root_Vessel_Type'Class;

   type Power_Class is (Set_Tax_Rate, Collect_Tax,
                        Appoint_Minister, Direct_Minister,
                        Law_Enforcement,
                        Appoint_General, Command_Army,
                        Appoint_Captain,
                        Captain_Trade_Ship, Captain_Naval_Ship);

   type Ship_Captain_Type is (Naval_Captain, Trader_Captain);

   type Power_Type (Class : Power_Class) is
      record
         case Class is
            when Set_Tax_Rate | Collect_Tax =>
               Tax_Category : Concorde.Trades.Market_Tax_Category;
            when Appoint_Minister =>
               null;
            when Direct_Minister =>
               Ministry     : Ministry_Access;
            when Law_Enforcement =>
               null;
            when Appoint_General =>
               null;
            when Command_Army =>
               null;
            when Appoint_Captain =>
               Captain_Type : Ship_Captain_Type;
            when Captain_Trade_Ship =>
               null;
--                 Trade_Ship   : Ship_Access;
            when Captain_Naval_Ship =>
               null;
--                 Naval_Ship   : Ship_Access;
         end case;
      end record;

   function Identifier (Power : Power_Type) return String;

   function Class_Identifier (Power : Power_Type) return String;

   function Set_Tax_Rate
     (Category : Concorde.Trades.Market_Tax_Category)
      return Power_Type
   is (Set_Tax_Rate, Category);

   function Collect_Tax
     (Market   : Concorde.Markets.Market_Type;
      Category : Concorde.Trades.Market_Tax_Category)
      return Power_Type
   is (Collect_Tax, Category);

   function Appoint_Minister return Power_Type
   is (Class => Appoint_Minister);

   function Appoint_General return Power_Type
   is (Class => Appoint_General);

   function Command_Army return Power_Type
   is (Class => Command_Army);

   function Direct_Minister
     (Ministry : not null access constant
        Concorde.Ministries.Root_Ministry_Type'Class)
      return Power_Type
   is (Direct_Minister, Ministry_Access (Ministry));

   function Law_Enforcement return Power_Type
   is (Class => Law_Enforcement);

--     function Command_Trade_Ship
--       (Ship : not null access Concorde.Ships.Root_Vessel_Type'Class)
--        return Power_Type
--     is (Command_Trade_Ship, Ship_Access (Ship));
--
--     function Command_Naval_Ship
--       (Ship : not null access Concorde.Ships.Root_Vessel_Type'Class)
--        return Power_Type
--     is (Command_Naval_Ship, Ship_Access (Ship));

   package Power_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Power_Type);

   type Power_Set is new Powered_Interface with
      record
         Set : Power_Lists.List;
      end record;

   function No_Powers return Power_Set is (Set => <>);

end Concorde.Powers;
