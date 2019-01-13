private with WL.String_Maps;

private with Memor.Database;
private with Memor.Element_Vectors;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Network.Nodes;

with Concorde.Calendar;
with Concorde.Objects;
with Concorde.Locations;

with Concorde.Agents;
with Concorde.Commodities;
with Concorde.Factions;
with Concorde.Government;
with Concorde.Industries;
with Concorde.Laws;
with Concorde.Markets;
with Concorde.Trades;
with Concorde.People.Groups;
with Concorde.People.Individuals;
with Concorde.People.Pops;
with Concorde.Ships;

limited with Concorde.Worlds;

private with Concorde.Corporations.Lists;
private with Concorde.Industries.Lists;
private with Concorde.People.Individuals.Lists;
private with Concorde.People.Pops.Lists;
private with Concorde.Ships.Lists;

package Concorde.People.Communities is

   type Land_Use is
     (Agricultural, Commercial, Industrial, Mining, Undeveloped, Urban);

   type Root_Community_Type is
     new Concorde.Objects.Root_User_Named_Object_Type
     and Concorde.Government.Governed_Interface
     and Concorde.Laws.Law_Target_Interface
     and Concorde.Network.Network_State_Interface
     and Concorde.Markets.Market_Interface
     and Concorde.Locations.Located_Interface with private;

   type Community_Type is access constant Root_Community_Type'Class;

   function World
     (Community : Root_Community_Type'Class)
      return not null access constant Concorde.Worlds.Root_World_Type'Class;

   function Owner
     (Community : Root_Community_Type'Class)
      return access constant Concorde.Factions.Root_Faction_Type'Class;

   function Owned_By
     (Community : Root_Community_Type'Class;
      Faction : Concorde.Factions.Root_Faction_Type'Class)
      return Boolean;

   procedure Set_Owner
     (Community : in out Root_Community_Type'Class;
      Faction : not null access constant
        Concorde.Factions.Root_Faction_Type'Class);

   function Market
     (Community : Root_Community_Type'Class)
      return Concorde.Markets.Market_Type;

   procedure Scan_Exports
     (Community : Root_Community_Type'Class;
      Process   : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Type;
                   Quantity  : Concorde.Quantities.Quantity_Type;
                   Price     : Concorde.Money.Price_Type));

   procedure Scan_Imports
     (Community : Root_Community_Type'Class;
      Process   : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Type;
                   Quantity  : Concorde.Quantities.Quantity_Type;
                   Price     : Concorde.Money.Price_Type));

   procedure Export
     (Community : in out Root_Community_Type'Class;
      Exporter  : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   procedure Import
     (Community : in out Root_Community_Type'Class;
      Importer  : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   procedure Buy_Export
     (Community : in out Root_Community_Type'Class;
      Buyer     : in out Concorde.Agents.Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type);

   procedure Sell_Import
     (Community : in out Root_Community_Type'Class;
      Seller    : in out Concorde.Agents.Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type);

   procedure Add_Pop
     (Community : in out Root_Community_Type'Class;
      Pop       : not null access constant
        Concorde.People.Pops.Root_Pop_Type'Class);

   procedure Scan_Pops
     (Community : Root_Community_Type'Class;
      Process : not null access
        procedure (Pop : Concorde.People.Pops.Pop_Type));

   function Get_Pop_By_Group
     (Community : Root_Community_Type'Class;
      Group     : not null access constant
        Concorde.People.Groups.Root_Pop_Group'Class)
      return Concorde.People.Pops.Pop_Type;

   function Total_Population
     (Community : Root_Community_Type'Class)
      return Concorde.Quantities.Quantity_Type;

   function Group_Population
     (Community : Root_Community_Type'Class;
      Group     : Concorde.People.Groups.Pop_Group)
      return Concorde.Quantities.Quantity_Type;

   procedure Add_Individual
     (Community  : in out Root_Community_Type'Class;
      Individual : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class);

   procedure Remove_Individual
     (Community  : in out Root_Community_Type'Class;
      Individual : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class);

   procedure Scan_Individuals
     (Community : Root_Community_Type'Class;
      Process : not null access
        procedure (Individual : Concorde.People.Individuals.Individual_Type));

   procedure Update_Market
     (Community : in out Root_Community_Type'Class);

   procedure Add_Ship
     (Community : in out Root_Community_Type'Class;
      Ship      : not null access constant
        Concorde.Ships.Root_Ship_Type'Class);

   procedure Remove_Ship
     (Community : in out Root_Community_Type'Class;
      Ship      : not null access constant
        Concorde.Ships.Root_Ship_Type'Class);

   procedure Scan_Ships
     (Community : Root_Community_Type'Class;
      Process   : not null access
        procedure (Ship : Concorde.Ships.Ship_Type));

   function Exists (Name : String) return Boolean;
   function Get (Name : String) return Community_Type;

   procedure Scan (Process : not null access
                     procedure (Community : Community_Type));

   type Updateable_Reference (Item : not null access Root_Community_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Community_Type'Class)
      return Updateable_Reference;

private

   type Land_Use_Record is
      record
         Relative : Unit_Real;
         Absolute : Non_Negative_Real;
      end record;

   type Land_Use_Array is array (Land_Use) of Land_Use_Record;

   type Local_Commodity_Record is
     new Concorde.Network.Expression_Object_Interface with
      record
         Base_Price : Concorde.Money.Price_Type;
         Price      : Concorde.Money.Price_Type;
         Quantity   : Concorde.Quantities.Quantity_Type;
         Supply     : Concorde.Quantities.Quantity_Type;
         Demand     : Concorde.Quantities.Quantity_Type;
         Imported   : Concorde.Quantities.Quantity_Type;
         Exported   : Concorde.Quantities.Quantity_Type;
      end record;

   type Local_Commodity is access all Local_Commodity_Record'Class;
   type Local_Commodity_Access is access all Local_Commodity_Record;

   overriding function Get_Field_Value
     (Local : Local_Commodity_Record;
      Name  : String)
      return Concorde.Network.Expression_Value;

   overriding function Get_Value
     (Local : Local_Commodity_Record)
      return Concorde.Network.Expression_Value;

   overriding function Has_Field
     (Local : Local_Commodity_Record;
      Name  : String)
      return Boolean;

   package Local_Commodity_Vectors is
     new Memor.Element_Vectors
       (Concorde.Commodities.Root_Commodity_Type,
        Local_Commodity_Access, null);

   type Import_Export_Record is
      record
         Quantity  : Concorde.Quantities.Quantity_Type;
         Price     : Concorde.Money.Price_Type;
      end record;

   package Import_Export_Vectors is
     new Memor.Element_Vectors
       (Concorde.Commodities.Root_Commodity_Type,
        Import_Export_Record,
        (Concorde.Quantities.Zero, Concorde.Money.Zero));

   type Import_Export_Agent is
      record
         Agent : access constant
           Concorde.Agents.Root_Agent_Type'Class;
         Items : Import_Export_Vectors.Vector;
      end record;

   package Import_Export_Maps is
     new WL.String_Maps (Import_Export_Agent);

   type Root_Community_Type is
     new Concorde.Objects.Root_User_Named_Object_Type
     and Concorde.Government.Governed_Interface
     and Concorde.Laws.Law_Target_Interface
     and Concorde.Network.Network_State_Interface
     and Concorde.Markets.Market_Interface
     and Concorde.Locations.Located_Interface with
      record
         World             : access constant
           Concorde.Worlds.Root_World_Type'Class;
         Owner             : Concorde.Factions.Faction_Type;
         Network           : Concorde.Network.Nodes.Node_State_Map;
         Importers         : Import_Export_Maps.Map;
         Exporters         : Import_Export_Maps.Map;
         Imports           : Import_Export_Vectors.Vector;
         Exports           : Import_Export_Vectors.Vector;
         Pops              : Concorde.People.Pops.Lists.List;
         Individuals       : Concorde.People.Individuals.Lists.List;
         Corporations      : Concorde.Corporations.Lists.List;
         Industries        : Concorde.Industries.Lists.List;
         Ships             : Concorde.Ships.Lists.List;
         Market            : Concorde.Markets.Market_Type;
         Government        : Concorde.Government.Government_Type;
         Occupation        : Unit_Real := 0.0;
         Land_Use          : Land_Use_Array := (others => (0.0, 0.0));
         Local_Commodities : Local_Commodity_Vectors.Vector;
      end record;

   overriding function Object_Database
     (Item : Root_Community_Type)
      return Memor.Memor_Database;

   overriding function Current_Location
     (Community : Root_Community_Type)
      return Concorde.Locations.Object_Location;

   overriding function Government
     (Community : Root_Community_Type)
      return Concorde.Government.Government_Type
   is (Community.Government);

   overriding procedure Set_Location
     (Community : in out Root_Community_Type;
      Location  : Concorde.Locations.Object_Location);

   overriding function Location_At
     (Community : Root_Community_Type;
      Time      : Concorde.Calendar.Time)
      return Concorde.Locations.Object_Location;

   overriding function Node
     (Community : Root_Community_Type;
      Name  : String)
      return Concorde.Network.Node_State_Access;

   overriding procedure Add_Node
     (Community : in out Root_Community_Type;
      Node      : Concorde.Network.Node_State_Access);

   overriding procedure Scan_State_Nodes
     (Community : Root_Community_Type;
      Process   : not null access
        procedure (Node_State : Concorde.Network.Node_State_Access));

   overriding function Evaluate_Constraint
     (From             : Root_Community_Type;
      Class_Name       : String;
      Constraint_Name  : String;
      Constraint_Value : String)
      return Concorde.Network.Array_Of_Values;

   overriding function Get_Field_Value
     (Community : Root_Community_Type;
      Name  : String)
      return Concorde.Network.Expression_Value;

   overriding function Get_Value
     (Community : Root_Community_Type)
      return Concorde.Network.Expression_Value;

   overriding function Has_Field
     (Community : Root_Community_Type;
      Name  : String)
      return Boolean;

   overriding function Current_Quantity
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Base_Price
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type;

   overriding function Current_Price
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type;

   overriding function Current_Demand
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Current_Supply
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Current_Imports
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Current_Exports
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Tax_Rate
     (Community : Root_Community_Type;
      Category : Concorde.Trades.Market_Tax_Category;
      Item     : Concorde.Commodities.Commodity_Type)
      return Unit_Real;

   overriding procedure Tax_Receipt
     (Community     : in out Root_Community_Type;
      Category      : Concorde.Trades.Market_Tax_Category;
      Commodity     : Concorde.Commodities.Commodity_Type;
      Tax           : Concorde.Money.Money_Type);

   overriding procedure Update_Commodity
     (Community     : in out Root_Community_Type;
      Item          : Concorde.Commodities.Commodity_Type;
      Demand        : Concorde.Quantities.Quantity_Type;
      Supply        : Concorde.Quantities.Quantity_Type;
      Quantity      : Concorde.Quantities.Quantity_Type;
      Base_Price    : Concorde.Money.Price_Type;
      Current_Price : Concorde.Money.Price_Type);

   overriding procedure Scan_Agents
     (Community : Root_Community_Type;
      Process : not null access
        procedure (Agent : not null access constant
                     Concorde.Agents.Root_Agent_Type'Class));

   function World
     (Community : Root_Community_Type'Class)
      return access constant Concorde.Worlds.Root_World_Type'Class
   is (Community.World);

   function Owner
     (Community : Root_Community_Type'Class)
      return access constant Concorde.Factions.Root_Faction_Type'Class
   is (Community.Owner);

   function Owned_By
     (Community : Root_Community_Type'Class;
      Faction   : Concorde.Factions.Root_Faction_Type'Class)
      return Boolean
   is (Concorde.Factions."/=" (Community.Owner, null)
       and then Community.Owner.Identifier = Faction.Identifier);

   function Market
     (Community : Root_Community_Type'Class)
      return Concorde.Markets.Market_Type
   is (Community.Market);

   function World
     (Community : Root_Community_Type'Class)
      return not null access constant Concorde.Worlds.Root_World_Type'Class
   is (Community.World);

   package Db is
     new Memor.Database
       ("community", Root_Community_Type, Community_Type);

   type Updateable_Reference
     (Item : not null access Root_Community_Type'Class) is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

   function Exists (Name : String) return Boolean
   is (Db.Exists (Name));

   function Get (Name : String) return Community_Type
   is (Db.Get (Name));

end Concorde.People.Communities;
