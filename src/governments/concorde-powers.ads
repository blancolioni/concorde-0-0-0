private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Concorde.Trades;

with Concorde.Markets;

limited with Concorde.Ministries;

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

   function Direct_Minister
     (Ministry : not null access constant
        Concorde.Ministries.Root_Ministry_Type'Class)
     return Power_Type;

   function Law_Enforcement return Power_Type;

   type Powered_Interface is limited interface;

   function Contains
     (Container : Powered_Interface;
      Power     : Power_Type)
      return Boolean
      is abstract;

   procedure Insert
     (Container : in out Powered_Interface;
      Power     : Power_Type)
   is abstract;

   procedure Remove
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

   overriding function Contains
     (Container : Power_Set;
      Power     : Power_Type)
      return Boolean;

   overriding procedure Insert
     (Container : in out Power_Set;
      Power     : Power_Type);

   overriding procedure Remove
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

   type Power_Class is (Set_Tax_Rate, Collect_Tax,
                        Appoint_Minister, Direct_Minister,
                        Law_Enforcement);

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

   function Direct_Minister
     (Ministry : not null access constant
        Concorde.Ministries.Root_Ministry_Type'Class)
      return Power_Type
   is (Direct_Minister, Ministry_Access (Ministry));

   function Law_Enforcement return Power_Type
   is (Class => Law_Enforcement);

   package Power_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Power_Type);

   type Power_Set is new Powered_Interface with
      record
         Set : Power_Lists.List;
      end record;

   function No_Powers return Power_Set is (Set => <>);

end Concorde.Powers;
