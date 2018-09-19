with WL.Heaps;

with Concorde.Contracts;
with Concorde.Government;
with Concorde.People.Communities;
with Concorde.People.Groups;
with Concorde.Worlds;

package body Concorde.Installations is

   type Consumption_Record is
      record
         Commodity : Concorde.Commodities.Commodity_Type;
         Cost_Per  : WL.Money.Money_Type;
         Input_Per : WL.Quantities.Quantity_Type;
         Have      : WL.Quantities.Quantity_Type;
      end record;

   package Consumption_Queues is
     new WL.Heaps (WL.Money.Money_Type, Consumption_Record,
                   WL.Money.">")
   with Unreferenced;

   procedure New_Port_Contracts
     (Port      : not null access constant Root_Installation_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type)
     with Pre => Port.Is_Port, Unreferenced;

   ---------------
   -- Community --
   ---------------

   function Community
     (Installation : Root_Installation_Type'Class)
      return access constant
     Concorde.People.Communities.Root_Community_Type'Class
   is
   begin
      return Concorde.People.Communities.Community_Type
        (Concorde.Locations.Primary (Installation.Current_Location));
   end Community;

   ------------------
   -- Execute_Hire --
   ------------------

   overriding procedure Execute_Hire
     (Employer  : not null access constant Root_Installation_Type;
      Employee  : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Wage      : WL.Money.Price_Type)
   is
      pragma Unreferenced (Commodity);
      New_Employee : constant Employee_Record :=
                       Employee_Record'
                         (Pop            =>
                            Concorde.People.Pops.Pop_Type
                            (Employee),
                          Size           => Quantity,
                          Wage           => Wage,
                          Contract_Days  => 30,
                          Days_Remaining => 30);
   begin
      Employer.Update.Employees.Append (New_Employee);
   end Execute_Hire;

   ------------------------
   -- Execute_Production --
   ------------------------

   overriding procedure Execute_Production
     (Installation : in out Root_Installation_Type)
   is null;

   --------------
   -- Facility --
   --------------

   function Facility
     (Installation : Root_Installation_Type'Class)
      return Concorde.Facilities.Facility_Type
   is
   begin
      return Installation.Facility;
   end Facility;

   -------------------
   -- Is_Colony_Hub --
   -------------------

   function Is_Colony_Hub
     (Installation : Root_Installation_Type'Class)
      return Boolean
   is
   begin
      case Installation.Facility.Class is
         when Concorde.Facilities.Colony_Hub =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Colony_Hub;

   -------------
   -- Is_Port --
   -------------

   function Is_Port
     (Installation : Root_Installation_Type'Class)
      return Boolean
   is
   begin
      case Installation.Facility.Class is
         when Concorde.Facilities.Port =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Port;

   ------------------------
   -- New_Port_Contracts --
   ------------------------

   procedure New_Port_Contracts
     (Port      : not null access constant Root_Installation_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type)
   is
      use type Concorde.Calendar.Time;
      use WL.Quantities;
      Individual_Contract_Size : constant Quantity_Type :=
                                   To_Quantity (10_000.0);
      Remaining                : Quantity_Type := Quantity;
      Expires                  : constant Concorde.Calendar.Time :=
                                   Concorde.Calendar.Clock
                                     + Concorde.Calendar.Days (7);
   begin
      if Port.Contracted_To_Buy (Commodity) > Zero then
         Port.Update.Delete_Pending_Offers (Commodity);
      end if;

      while Remaining > Zero loop
         declare
            use WL.Money;
            Quantity : constant Quantity_Type :=
                         Min (Remaining, Individual_Contract_Size);
            Value    : constant Money_Type :=
                         Total (Price, Quantity);
         begin
            exit when Value > Port.Limit_Cash;

            declare
               Contract : constant Concorde.Contracts.Contract_Type :=
                            Concorde.Contracts.New_Buy_Contract
                              (Location  => Port.Current_Location,
                               Buyer     => Port,
                               Commodity => Commodity,
                               Quantity  => Quantity,
                               Price     => Price,
                               Expires   => Expires);
            begin
               Port.Update.Add_Contract (Contract);
               Remaining := Remaining - Contract.Quantity;
               Port.Log ("new contract: " & Contract.Show);
            end;
         end;
      end loop;
   end New_Port_Contracts;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Installation_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------
   -- Owner --
   -----------

   function Owner
     (Installation : Root_Installation_Type'Class)
      return access constant Concorde.Agents.Root_Agent_Type'Class
   is
   begin
      return Installation.Owner;
   end Owner;

   --------------------
   -- Remove_Manager --
   --------------------

   procedure Remove_Manager
     (Installation : in out Root_Installation_Type'Class)
   is
   begin
      Installation.Manager := null;
   end Remove_Manager;

   ----------------------------
   -- Set_Artisan_Production --
   ----------------------------

   procedure Set_Artisan_Production
     (Installation : in out Root_Installation_Type'Class;
      Facility     : Concorde.Facilities.Facility_Type)
   is
   begin
      Installation.Facility := Facility;
   end Set_Artisan_Production;

   -----------------
   -- Set_Manager --
   -----------------

   procedure Set_Manager
     (Installation : in out Root_Installation_Type'Class;
      Manager      : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
   is
   begin
      Installation.Manager := Manager;
   end Set_Manager;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Installation_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

   ------------------
   -- Update_Agent --
   ------------------

   overriding procedure Update_Agent
     (Installation   : not null access constant Root_Installation_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class))
   is
   begin
      Perform_Update (Installation.Update);
   end Update_Agent;

   -----------
   -- World --
   -----------

   function World
     (Installation : Root_Installation_Type'Class)
      return access constant Concorde.Worlds.Root_World_Type'Class
   is
   begin
      return Concorde.Worlds.World_Type
        (Concorde.Locations.Primary (Installation.Current_Location));
   end World;

end Concorde.Installations;
