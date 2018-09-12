private with Memor.Database;

with Concorde.Network.Nodes;

with Concorde.Calendar;
with Concorde.Objects;
with Concorde.Locations;

with Concorde.Factions;
with Concorde.Government;
with Concorde.Laws;
with Concorde.Markets;
with Concorde.Worlds;

with Concorde.People.Individuals.Lists;
with Concorde.People.Pops.Lists;

package Concorde.People.Communities is

   type Root_Community_Type is
     new Concorde.Objects.Root_User_Named_Object_Type
     and Concorde.Government.Governed_Interface
     and Concorde.Laws.Law_Target_Interface
     and Concorde.Network.Network_State_Interface
     and Concorde.Locations.Located_Interface with private;

   type Community_Type is access constant Root_Community_Type'Class;

   function World
     (Community : Root_Community_Type'Class)
      return Concorde.Worlds.World_Type;

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

   procedure Add_Pop
     (Community : in out Root_Community_Type'Class;
      Pop       : not null access constant
        Concorde.People.Pops.Root_Pop_Type'Class);

   procedure Scan_Pops
     (Community : Root_Community_Type'Class;
      Process : not null access
        procedure (Pop : Concorde.People.Pops.Pop_Type));

   procedure Scan_Individuals
     (Community : Root_Community_Type'Class;
      Process : not null access
        procedure (Individual : Concorde.People.Individuals.Individual_Type));

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

   type Root_Community_Type is
     new Concorde.Objects.Root_User_Named_Object_Type
     and Concorde.Government.Governed_Interface
     and Concorde.Laws.Law_Target_Interface
     and Concorde.Network.Network_State_Interface
     and Concorde.Locations.Located_Interface with
      record
         World       : Concorde.Worlds.World_Type;
         Owner       : Concorde.Factions.Faction_Type;
         Network     : Concorde.Network.Nodes.Node_State_Map;
         Pops        : Concorde.People.Pops.Lists.List;
         Individuals : Concorde.People.Individuals.Lists.List;
         Market      : Concorde.Markets.Market_Type;
         Government  : Concorde.Government.Government_Type;
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

   overriding procedure Update
     (Community : in out Root_Community_Type);

   overriding function Evaluate_Constraint
     (From             : Root_Community_Type;
      Class_Name       : String;
      Constraint_Name  : String;
      Constraint_Value : String)
      return Concorde.Network.Array_Of_Values;

   function World
     (Community : Root_Community_Type'Class)
      return Concorde.Worlds.World_Type
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
