with Concorde.Real_Images;

with Concorde.Worlds;

with Concorde.People.Communities.Fields;

with Concorde.Ministries;

package body Concorde.People.Communities is

   procedure Scan_Import_Export_Vector
     (Vector    : Import_Export_Vectors.Vector;
      Process   : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Type;
                   Quantity  : Concorde.Quantities.Quantity_Type;
                   Price     : Concorde.Money.Price_Type));

   procedure Import_Export
     (Vector    : in out Import_Export_Vectors.Vector;
      Map       : in out Import_Export_Maps.Map;
      Agent     : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   --------------------
   -- Add_Individual --
   --------------------

   procedure Add_Individual
     (Community  : in out Root_Community_Type'Class;
      Individual : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
   is
   begin
      Community.Individuals.Append
        (Concorde.People.Individuals.Individual_Type (Individual));
   end Add_Individual;

   --------------
   -- Add_Node --
   --------------

   overriding procedure Add_Node
     (Community : in out Root_Community_Type;
      Node      : Concorde.Network.Node_State_Access)
   is
   begin
      Community.Network.Add_Node (Node);
   end Add_Node;

   -------------
   -- Add_Pop --
   -------------

   procedure Add_Pop
     (Community : in out Root_Community_Type'Class;
      Pop       : not null access constant
        Concorde.People.Pops.Root_Pop_Type'Class)
   is
   begin
      Community.Pops.Append (Concorde.People.Pops.Pop_Type (Pop));
   end Add_Pop;

   --------------
   -- Add_Ship --
   --------------

   procedure Add_Ship
     (Community : in out Root_Community_Type'Class;
      Ship      : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
   is
   begin
      Community.Ships.Append (Concorde.Ships.Ship_Type (Ship));
   end Add_Ship;

   ----------------
   -- Base_Price --
   ----------------

   overriding function Base_Price
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
   begin
      return Community.Local_Commodities.Element (Item).Base_Price;
   end Base_Price;

   ----------------
   -- Buy_Export --
   ----------------

   procedure Buy_Export
     (Community : in out Root_Community_Type'Class;
      Buyer     : in out Concorde.Agents.Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Quantities;
      Remaining : Quantity_Type := Quantity;
      Total     : Quantity_Type := Zero;
   begin
      Buyer.Log ("buying " & Show (Quantity) & " " & Commodity.Identifier);
      for Exporter of Community.Exporters loop
         declare
            Rec : Import_Export_Record :=
                    Exporter.Items.Element (Commodity);
            Bought : constant Quantity_Type :=
                       Min (Remaining, Rec.Quantity);
         begin
            if Bought > Zero then
               Rec.Quantity := Rec.Quantity - Bought;
               Total := Total + Bought;
               Remaining := Remaining - Bought;
               Buyer.Require_Cash
                 (Concorde.Money.Total (Rec.Price, Bought));
               Buyer.On_Commodity_Buy (Commodity, Bought, Rec.Price);
               Exporter.Agent.Log
                 ("exporting " & Show (Bought)
                  & " " & Commodity.Identifier
                  & " @ " & Concorde.Money.Show (Rec.Price));
               Exporter.Agent.Variable_Reference.On_Commodity_Sell
                 (Commodity, Bought, Rec.Price);
               Exporter.Items.Replace_Element (Commodity, Rec);
            end if;
         end;
         declare
            Rec : constant Local_Commodity_Access :=
                    Community.Local_Commodities.Element (Commodity);
         begin
            Rec.Exported := Rec.Exported + Quantity;
         end;

      end loop;

      declare
         Export : Import_Export_Record :=
                    Community.Exports.Element (Commodity);
      begin
         Export.Quantity := Export.Quantity - Total;
         Community.Exports.Replace_Element
           (Commodity, Export);
      end;

   end Buy_Export;

   --------------------
   -- Current_Demand --
   --------------------

   overriding function Current_Demand
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Community.Local_Commodities.Element (Item).Demand;
   end Current_Demand;

   ---------------------
   -- Current_Exports --
   ---------------------

   overriding function Current_Exports
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Community.Local_Commodities.Element (Item).Exported;
   end Current_Exports;

   ---------------------
   -- Current_Imports --
   ---------------------

   overriding function Current_Imports
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Community.Local_Commodities.Element (Item).Imported;
   end Current_Imports;

   ----------------------
   -- Current_Location --
   ----------------------

   overriding function Current_Location
     (Community : Root_Community_Type)
      return Concorde.Locations.Object_Location
   is
   begin
      return Concorde.Locations.World_Surface
        (Community.World, 1);
   end Current_Location;

   -------------------
   -- Current_Price --
   -------------------

   overriding function Current_Price
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
   begin
      return Community.Local_Commodities.Element (Item).Price;
   end Current_Price;

   ----------------------
   -- Current_Quantity --
   ----------------------

   overriding function Current_Quantity
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Community.Local_Commodities.Element (Item).Quantity;
   end Current_Quantity;

   --------------------
   -- Current_Supply --
   --------------------

   overriding function Current_Supply
     (Community : Root_Community_Type;
      Item      : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Community.Local_Commodities.Element (Item).Supply;
   end Current_Supply;

   -------------------------
   -- Evaluate_Constraint --
   -------------------------

   overriding function Evaluate_Constraint
     (From             : Root_Community_Type;
      Class_Name       : String;
      Constraint_Name  : String;
      Constraint_Value : String)
      return Concorde.Network.Array_Of_Values
   is

      type Constraint_Type is (All_Constraint, Group_Constraint);

      function Eval_Pop_Constraint return Concorde.Network.Array_Of_Values;

      -------------------------
      -- Eval_Pop_Constraint --
      -------------------------

      function Eval_Pop_Constraint return Concorde.Network.Array_Of_Values is
         Result : Concorde.Network.Array_Of_Values
           (1 .. Natural (From.Pops.Length));
         Count  : Natural := 0;
         Constraint : constant Constraint_Type :=
                        (if Constraint_Name = "group"
                         then Group_Constraint
                         elsif Constraint_Name = "all"
                         then All_Constraint
                         else raise Constraint_Error with
                           "no such pop constraint: " & Constraint_Name);
         Group      : constant Concorde.People.Groups.Pop_Group :=
                        (case Constraint is
                            when All_Constraint =>
                              null,
                            when Group_Constraint =>
                              Concorde.People.Groups.Get (Constraint_Value));
      begin
         for Pop of From.Pops loop
            declare
               use type Concorde.People.Groups.Pop_Group;
               Include : constant Boolean :=
                           (case Constraint is
                               when All_Constraint => True,
                               when Group_Constraint =>
                                 Pop.Group = Group);
            begin
               if Include then
                  Count := Count + 1;
                  Result (Count) :=
                    Concorde.Network.To_Expression_Value (Pop);
               end if;
            end;
         end loop;
         return Result (1 .. Count);
      end Eval_Pop_Constraint;

   begin
      if Class_Name = "pops" then
         return Eval_Pop_Constraint;
      else
         return From.Network.Evaluate_Constraint
           (Class_Name, Constraint_Name, Constraint_Value);
      end if;
   end Evaluate_Constraint;

   ------------
   -- Export --
   ------------

   procedure Export
     (Community : in out Root_Community_Type'Class;
      Exporter  : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
   begin
      Exporter.Log
        ("exporting "
         & Concorde.Quantities.Show (Quantity)
         & " "
         & Commodity.Identifier
         & " @ "
         & Concorde.Money.Show (Price));
      Import_Export
        (Vector    => Community.Exports,
         Map       => Community.Exporters,
         Agent     => Exporter,
         Commodity => Commodity,
         Quantity  => Quantity,
         Price     => Price);
   end Export;

   ---------------------
   -- Get_Field_Value --
   ---------------------

   overriding function Get_Field_Value
     (Community : Root_Community_Type;
      Name      : String)
      return Concorde.Network.Expression_Value
   is
   begin
      return Concorde.People.Communities.Fields.Get_Field (Community, Name);
   end Get_Field_Value;

   ---------------------
   -- Get_Field_Value --
   ---------------------

   overriding function Get_Field_Value
     (Local : Local_Commodity_Record;
      Name  : String)
      return Concorde.Network.Expression_Value
   is
   begin
      if Name = "price" then
         return Concorde.Network.To_Expression_Value
           (Real (Concorde.Money.To_Real (Local.Price)));
      else
         raise Program_Error;
      end if;
   end Get_Field_Value;

   ----------------------
   -- Get_Pop_By_Group --
   ----------------------

   function Get_Pop_By_Group
     (Community : Root_Community_Type'Class;
      Group     : not null access constant
        Concorde.People.Groups.Root_Pop_Group'Class)
      return Concorde.People.Pops.Pop_Type
   is
      use Concorde.People.Groups;
   begin
      for Pop of Community.Pops loop
         if Pop.Group = Pop_Group (Group) then
            return Pop;
         end if;
      end loop;
      return null;
   end Get_Pop_By_Group;

   ---------------
   -- Get_Value --
   ---------------

   overriding function Get_Value
     (Community : Root_Community_Type)
      return Concorde.Network.Expression_Value
   is
      pragma Unreferenced (Community);
   begin
      return Concorde.Network.To_Expression_Value (0.0);
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   overriding function Get_Value
     (Local : Local_Commodity_Record)
      return Concorde.Network.Expression_Value
   is
   begin
      return Concorde.Network.To_Expression_Value
        (Non_Negative_Real
           (Concorde.Quantities.To_Real (Local.Quantity)));
   end Get_Value;

   ----------------------
   -- Group_Population --
   ----------------------

   function Group_Population
     (Community : Root_Community_Type'Class;
      Group     : Concorde.People.Groups.Pop_Group)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
      use type Concorde.People.Groups.Pop_Group;
   begin
      return Total : Quantity_Type := Zero do
         for Pop of Community.Pops loop
            if Pop.Group = Group then
               Total := Total + Pop.Size_Quantity;
            end if;
         end loop;
      end return;
   end Group_Population;

   ---------------
   -- Has_Field --
   ---------------

   overriding function Has_Field
     (Community : Root_Community_Type;
      Name      : String)
      return Boolean
   is
      pragma Unreferenced (Community);
   begin
      return Concorde.People.Communities.Fields.Have_Field (Name);
   end Has_Field;

   ---------------
   -- Has_Field --
   ---------------

   overriding function Has_Field
     (Local : Local_Commodity_Record;
      Name  : String)
      return Boolean
   is
      pragma Unreferenced (Local);
   begin
      return Name = "price";
   end Has_Field;

   ------------
   -- Import --
   ------------

   procedure Import
     (Community : in out Root_Community_Type'Class;
      Importer  : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
   begin
      Import_Export
        (Vector    => Community.Imports,
         Map       => Community.Importers,
         Agent     => Importer,
         Commodity => Commodity,
         Quantity  => Quantity,
         Price     => Price);

      Community.Log
        (Importer.Identifier
         & " importing "
         & Concorde.Quantities.Show (Quantity)
         & " "
         & Commodity.Identifier
         & " @ "
         & Concorde.Money.Show (Price)
         & "; total imports now "
         & Concorde.Quantities.Show
           (Community.Imports.Element (Commodity).Quantity));

   end Import;

   -------------------
   -- Import_Export --
   -------------------

   procedure Import_Export
     (Vector    : in out Import_Export_Vectors.Vector;
      Map       : in out Import_Export_Maps.Map;
      Agent     : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
      All_Items_Rec : Import_Export_Record :=
                        Vector.Element (Commodity);
   begin

      if not Map.Contains (Agent.Identifier) then
         Map.Insert (Agent.Identifier,
                     Import_Export_Agent'
                       (Agent => Agent,
                        Items => <>));
      end if;

      declare
         use Concorde.Quantities;
         Item_Rec : Import_Export_Record :=
                      Map.Element (Agent.Identifier).Items.Element (Commodity);
      begin
         Item_Rec.Price := Price;
         All_Items_Rec.Price := Price;
         All_Items_Rec.Quantity :=
           All_Items_Rec.Quantity + Quantity - Item_Rec.Quantity;
         Item_Rec.Quantity := Quantity;
         Map (Agent.Identifier).Items.Replace_Element
           (Commodity, Item_Rec);
         Vector.Replace_Element (Commodity, All_Items_Rec);
      end;

   end Import_Export;

   -----------------
   -- Location_At --
   -----------------

   overriding function Location_At
     (Community : Root_Community_Type;
      Time      : Concorde.Calendar.Time)
      return Concorde.Locations.Object_Location
   is
      pragma Unreferenced (Time);
   begin
      return Root_Community_Type'Class (Community).Current_Location;
   end Location_At;

   ----------
   -- Node --
   ----------

   overriding function Node
     (Community : Root_Community_Type;
      Name      : String)
      return Concorde.Network.Node_State_Access
   is
   begin
      return Community.Network.Node (Name);
   end Node;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Community_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------------------
   -- Remove_Individual --
   -----------------------

   procedure Remove_Individual
     (Community  : in out Root_Community_Type'Class;
      Individual : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
   is
      Position : Concorde.People.Individuals.Lists.Cursor :=
                   Community.Individuals.Find
                     (Concorde.People.Individuals.Individual_Type
                        (Individual));
   begin
      pragma Assert (Concorde.People.Individuals.Lists.Has_Element (Position));
      Community.Individuals.Delete (Position);
   end Remove_Individual;

   -----------------
   -- Remove_Ship --
   -----------------

   procedure Remove_Ship
     (Community : in out Root_Community_Type'Class;
      Ship      : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
   is
      Position : Concorde.Ships.Lists.Cursor :=
                   Community.Ships.Find
                     (Concorde.Ships.Ship_Type (Ship));
   begin
      pragma Assert (Concorde.Ships.Lists.Has_Element (Position));
      Community.Ships.Delete (Position);
   end Remove_Ship;

   ----------
   -- Scan --
   ----------

   procedure Scan (Process : not null access
                     procedure (Community : Community_Type))
   is
   begin
      Db.Scan (Process);
   end Scan;

   -----------------
   -- Scan_Agents --
   -----------------

   overriding procedure Scan_Agents
     (Community : Root_Community_Type;
      Process   : not null access
        procedure (Agent : not null access constant
                     Concorde.Agents.Root_Agent_Type'Class))
   is
      procedure Process_Ministry
        (Ministry : Concorde.Ministries.Ministry_Type);

      ----------------------
      -- Process_Ministry --
      ----------------------

      procedure Process_Ministry
        (Ministry : Concorde.Ministries.Ministry_Type)
      is
      begin
         Process (Ministry);
      end Process_Ministry;

   begin
      Process (Community.Owner);
      Community.Owner.Scan_Ministries (Process_Ministry'Access);
      Process (Community.Government);
      for Individual of Community.Individuals loop
         Process (Individual);
      end loop;
      for Pop of Community.Pops loop
         Process (Pop);
      end loop;
      for Industry of Community.Industries loop
         Process (Industry);
      end loop;
      for Corporation of Community.Corporations loop
         Process (Corporation);
      end loop;
      if False then
         for Ship of Community.Ships loop
            Process (Ship);
         end loop;
      end if;
   end Scan_Agents;

   ------------------
   -- Scan_Exports --
   ------------------

   procedure Scan_Exports
     (Community : Root_Community_Type'Class;
      Process   : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Type;
                   Quantity  : Concorde.Quantities.Quantity_Type;
                   Price     : Concorde.Money.Price_Type))
   is
   begin
      Scan_Import_Export_Vector (Community.Exports, Process);
   end Scan_Exports;

   -------------------------------
   -- Scan_Import_Export_Vector --
   -------------------------------

   procedure Scan_Import_Export_Vector
     (Vector    : Import_Export_Vectors.Vector;
      Process   : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Type;
                   Quantity  : Concorde.Quantities.Quantity_Type;
                   Price     : Concorde.Money.Price_Type))
   is

      procedure Local_Process
        (Commodity : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Rec       : Import_Export_Record);

      -------------------
      -- Local_Process --
      -------------------

      procedure Local_Process
        (Commodity : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Rec       : Import_Export_Record)
      is
      begin
         Process (Concorde.Commodities.Commodity_Type (Commodity),
                  Rec.Quantity, Rec.Price);
      end Local_Process;

   begin
      Vector.Scan (Local_Process'Access);
   end Scan_Import_Export_Vector;

   ------------------
   -- Scan_Imports --
   ------------------

   procedure Scan_Imports
     (Community : Root_Community_Type'Class;
      Process   : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Type;
                   Quantity  : Concorde.Quantities.Quantity_Type;
                   Price     : Concorde.Money.Price_Type))
   is
   begin
      Scan_Import_Export_Vector (Community.Imports, Process);
   end Scan_Imports;

   ----------------------
   -- Scan_Individuals --
   ----------------------

   procedure Scan_Individuals
     (Community : Root_Community_Type'Class;
      Process   : not null access
        procedure (Individual : Concorde.People.Individuals.Individual_Type))
   is
   begin
      for Individual of Community.Individuals loop
         Process (Individual);
      end loop;
   end Scan_Individuals;

   ---------------
   -- Scan_Pops --
   ---------------

   procedure Scan_Pops
     (Community : Root_Community_Type'Class;
      Process   : not null access
        procedure (Pop : Concorde.People.Pops.Pop_Type))
   is
   begin
      for Pop of Community.Pops loop
         Process (Pop);
      end loop;
   end Scan_Pops;

   ----------------
   -- Scan_Ships --
   ----------------

   procedure Scan_Ships
     (Community : Root_Community_Type'Class;
      Process   : not null access
        procedure (Ship : Concorde.Ships.Ship_Type))
   is
   begin
      for Ship of Community.Ships loop
         Process (Ship);
      end loop;
   end Scan_Ships;

   ----------------------
   -- Scan_State_Nodes --
   ----------------------

   overriding procedure Scan_State_Nodes
     (Community : Root_Community_Type;
      Process   : not null access
        procedure (Node_State : Concorde.Network.Node_State_Access))
   is
      procedure Internal_Process
        (Node_State : Concorde.Network.Node_State_Access);

      ----------------------
      -- Internal_Process --
      ----------------------

      procedure Internal_Process
        (Node_State : Concorde.Network.Node_State_Access)
      is
      begin
         Process (Node_State);
      end Internal_Process;

   begin
      Community.Network.Scan_State_Nodes (Internal_Process'Access);
   end Scan_State_Nodes;

   -----------------
   -- Sell_Import --
   -----------------

   procedure Sell_Import
     (Community : in out Root_Community_Type'Class;
      Seller    : in out Concorde.Agents.Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Quantities;
      Remaining : Quantity_Type := Quantity;
      Total     : Quantity_Type := Zero;
   begin
      for Importer of Community.Importers loop
         declare
            Rec  : Import_Export_Record :=
                     Importer.Items.Element (Commodity);
            Sold : constant Quantity_Type :=
                     Min (Remaining, Rec.Quantity);
         begin
            if Sold > Zero then
               Rec.Quantity := Rec.Quantity - Sold;
               Total := Total + Sold;
               Remaining := Remaining - Sold;
               Importer.Agent.Variable_Reference.Require_Cash
                 (Concorde.Money.Total (Rec.Price, Sold));
               Importer.Agent.Variable_Reference.On_Commodity_Buy
                 (Commodity, Sold, Rec.Price);
               Seller.On_Commodity_Sell
                 (Commodity, Sold, Rec.Price);
               Importer.Items.Replace_Element (Commodity, Rec);
            end if;
         end;

         declare
            Rec : constant Local_Commodity_Access :=
                    Community.Local_Commodities.Element (Commodity);
         begin
            Rec.Imported := Rec.Imported + Quantity;
         end;

      end loop;

      declare
         Import : Import_Export_Record :=
                    Community.Imports.Element (Commodity);
      begin
         Community.Log
           (Commodity.Identifier
            & ": total sold: " & Show (Total)
            & "; total wanted: " & Show (Import.Quantity));

         pragma Assert (Total <= Import.Quantity);
         Import.Quantity := Import.Quantity - Total;
         Community.Imports.Replace_Element
           (Commodity, Import);
      end;

   end Sell_Import;

   ------------------
   -- Set_Location --
   ------------------

   overriding procedure Set_Location
     (Community : in out Root_Community_Type;
      Location  : Concorde.Locations.Object_Location)
   is
      pragma Unreferenced (Community, Location);
   begin
      null;
   end Set_Location;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (Community : in out Root_Community_Type'Class;
      Faction   : not null access constant
        Concorde.Factions.Root_Faction_Type'Class)
   is
   begin
      Community.Owner := Concorde.Factions.Faction_Type (Faction);
   end Set_Owner;

   --------------
   -- Tax_Rate --
   --------------

   overriding function Tax_Rate
     (Community : Root_Community_Type;
      Category  : Concorde.Trades.Market_Tax_Category;
      Item      : Concorde.Commodities.Commodity_Type)
      return Unit_Real
   is
   begin
      return Community.Government.Tax_Rate (Category, Item);
   end Tax_Rate;

   -----------------
   -- Tax_Receipt --
   -----------------

   overriding procedure Tax_Receipt
     (Community     : in out Root_Community_Type;
      Category      : Concorde.Trades.Market_Tax_Category;
      Commodity     : Concorde.Commodities.Commodity_Type;
      Tax           : Concorde.Money.Money_Type)
   is
      use Concorde.Trades;
      use all type Concorde.Government.Revenue_Source;
      Name : constant String :=
               (case Category is
                   when Import    => "import tariff",
                   when Export    => "export tariff",
                   when Sales     => "sales tax",
                   when Purchases => "purchases tax");
      Revenue : constant Concorde.Government.Revenue_Source :=
                  (case Category is
                      when Import    => Import_Tariff,
                      when Export    => Export_Tariff,
                      when Sales     => Sales_Tax,
                      when Purchases => Sales_Tax);
   begin

      Community.Log
        (Name & " "
         & Concorde.Real_Images.Approximate_Image
           (100.0 * Community.Tax_Rate (Category, Commodity))
         & "%"
         & " on " & Commodity.Identifier
         & " earns "
         & Concorde.Money.Show (Tax));

      Community.Government.Update.Tax_Receipt
        (Revenue => Revenue,
         Receipt => Tax);

   end Tax_Receipt;

   ----------------------
   -- Total_Population --
   ----------------------

   function Total_Population
     (Community : Root_Community_Type'Class)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
   begin
      return Total : Quantity_Type := Zero do
         for Pop of Community.Pops loop
            Total := Total + Pop.Size_Quantity;
         end loop;
      end return;
   end Total_Population;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Community_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

   ----------------------
   -- Update_Commodity --
   ----------------------

   overriding procedure Update_Commodity
     (Community     : in out Root_Community_Type;
      Item          : Concorde.Commodities.Commodity_Type;
      Demand        : Concorde.Quantities.Quantity_Type;
      Supply        : Concorde.Quantities.Quantity_Type;
      Quantity      : Concorde.Quantities.Quantity_Type;
      Base_Price    : Concorde.Money.Price_Type;
      Current_Price : Concorde.Money.Price_Type)
   is
      use Concorde.Quantities;
      Rec : Local_Commodity_Record renames
              Community.Local_Commodities.Element (Item).all;
      New_Supply : constant Quantity_Type :=
                     (if Rec.Supply = Zero
                      then Scale (Supply, 4.0)
                      else Scale (Rec.Supply, 3.0) + Supply);
      New_Demand : constant Quantity_Type :=
                     (if Rec.Demand = Zero
                      then Scale (Demand, 4.0)
                      else Scale (Rec.Demand, 3.0) + Demand);
   begin
      Rec :=
        Local_Commodity_Record'
          (Price      => Current_Price,
           Base_Price => Base_Price,
           Quantity   => Quantity,
           Supply     => Scale (New_Supply, 0.25),
           Demand     => Scale (New_Demand, 0.25),
           Imported   => Rec.Imported,
           Exported   => Rec.Exported);
   end Update_Commodity;

   -------------------
   -- Update_Market --
   -------------------

   procedure Update_Market
     (Community : in out Root_Community_Type'Class)
   is
   begin
      for Commodity of Concorde.Commodities.All_Commodities loop
         declare
            use Concorde.Money, Concorde.Quantities;
            Demand : constant Quantity_Type :=
                       Community.Market.Current_Demand (Commodity);
            Supply : constant Quantity_Type :=
                       Community.Market.Current_Supply (Commodity);
            Available : constant Quantity_Type :=
                          Min (Supply, Demand);
            Base_Price : constant Price_Type :=
                           Community.Market.Current_Price (Commodity);
            Current_Price : constant Price_Type := Base_Price;
         begin
            Community.Log
              (Commodity.Name
               & ": supply "
               & Show (Supply)
               & "; demand "
               & Show (Demand)
               & "; price "
               & Show (Current_Price));

            Community.Update_Commodity
              (Item          => Commodity,
               Demand        => Demand,
               Supply        => Supply,
               Quantity      => Available,
               Base_Price    => Base_Price,
               Current_Price => Current_Price);
         end;
      end loop;
   end Update_Market;

   -------------------------
   -- Update_Local_Market --
   -------------------------

--     procedure Update_Local_Market
--       (Community : in out Root_Community_Type'Class)
--     is
--        use Concorde.Money;
--        use Concorde.Quantities;
--        Food_Production : constant Non_Negative_Real :=
--                            Concorde.Network.To_Real_Value
--                              (Community.Node
--                                 ("agriculture-industry")
--                               .Get_Field_Value ("production"));
--        Food_Demand     : Non_Negative_Real := 0.0;
--        Food_Supply     : constant Non_Negative_Real := Food_Production;
--        Local_Food      : constant Local_Commodity :=
--                            Community.Local_Commodities.Element
--                              (Concorde.Commodities.Get ("food");
--        Food_Available  : Non_Negative_Real :=
--                        Non_Negative_Real (To_Real (Local_Food.Available));
--    Food_Price      : constant Real := Real (To_Real (Local_Food.Price));
--     begin
--        for Pop of Community.Pops loop
--           declare
--              Wanted        : constant Real := Real (Pop.Size);
--              Wanted_Budget : constant Real := Food_Price * Wanted;
--              Food_Budget   : constant Non_Negative_Real :=
--                                Real'Min (Pop.Current_Income_Total / 5.0,
--                                          Wanted_Budget);
--           begin
--          Food_Demand := Food_Demand + Wanted * Food_Budget / Wanted_Budget;
--           end;
--        end loop;
--
--        Food_Available := Food_Available + Food_Supply;
--        Local_Food.Available := To_Quantity (Float (Food_Available));
--        Local_Food.Demand := To_Quantity (Float (Food_Demand));
--        Local_Food.Supply := To_Quantity (Float (Food_Supply));
--
--        Community.Log
--          ("food: supply " & Show (Local_Food.Supply)
--           & "; demand " & Show (Local_Food.Demand)
--           & "; available " & Show (Local_Food.Available)
--           & "; price " & Show (Local_Food.Price));
--
--        declare
--           Factor : constant Real :=
--                      (if Food_Demand <= Food_Available
--                       then 1.0 else Food_Available / Food_Demand);
--        begin
--           Local_Food.Available :=
--             Scale (Local_Food.Available, Float (Factor));
--        end;
--
--     end Update_Local_Market;

end Concorde.People.Communities;
