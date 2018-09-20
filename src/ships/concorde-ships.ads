private with Ada.Strings.Unbounded;

with Concorde.Money;
with Concorde.Quantities;

with Memor;

with Concorde.Agents;
with Concorde.Calendar;
with Concorde.Commodities;
with Concorde.Locations;
with Concorde.Objects;
with Concorde.Ownership;
with Concorde.Trades;

limited with Concorde.Systems;

package Concorde.Ships is

   type Ship_Classification is (Civilian, Military);

   type Root_Ship_Type is abstract new Concorde.Agents.Root_Agent_Type
     and Concorde.Objects.User_Named_Object_Interface
     and Concorde.Locations.Located_Interface
     and Concorde.Ownership.Owned_Interface
     and Memor.Identifier_Record_Type
   with private;

   overriding function Name
     (Ship : Root_Ship_Type)
      return String;

   overriding function Short_Name
     (Ship : Root_Ship_Type)
      return String;

   overriding function Owner
     (Ship : Root_Ship_Type)
      return access constant Concorde.Ownership.Owner_Interface'Class;

   overriding procedure Set_Name
     (Ship : in out Root_Ship_Type;
      Name : String);

   overriding procedure On_Commodity_Buy
     (Ship      : in out Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   overriding procedure On_Commodity_Sell
     (Ship      : in out Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   function Alive (Ship : Root_Ship_Type) return Boolean
                   is abstract;

   function Classification
     (Ship : Root_Ship_Type)
      return Ship_Classification
      is abstract;

   function Maximum_Thrust
     (Ship : Root_Ship_Type)
      return Non_Negative_Real
      is abstract;

   function Current_Mass
     (Ship : Root_Ship_Type)
      return Non_Negative_Real
      is abstract;

   function Cargo_Capacity
     (Ship      : Root_Ship_Type)
      return Concorde.Quantities.Quantity_Type
      is abstract;

   function Available_Capacity
     (Ship      : Root_Ship_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Quantities.Quantity_Type
      is abstract;

   function Update
     (Ship : not null access constant Root_Ship_Type)
      return access Root_Ship_Type'Class
      is abstract;

   function Destination
     (Ship : Root_Ship_Type'Class)
      return Concorde.Locations.Object_Location;

   procedure Set_Destination
     (Ship         : in out Root_Ship_Type'Class;
      Destination  : not null access constant
        Concorde.Systems.Star_System_Object_Interface'Class;
      Start_Time   : Concorde.Calendar.Time;
      Journey_Time : Duration);

   procedure Set_Destination
     (Ship         : in out Root_Ship_Type'Class;
      Destination  : Concorde.Locations.Object_Location;
      Start_Time   : Concorde.Calendar.Time;
      Journey_Time : Duration);

   procedure Set_Jump_Destination
     (Ship         : in out Root_Ship_Type'Class;
      System       : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Start_Time   : Concorde.Calendar.Time;
      Journey_Time : Duration);

   procedure Clear_Destination
     (Ship   : in out Root_Ship_Type'Class);

   procedure Add_Wanted
     (Ship       : in out Root_Ship_Type'Class;
      Commodity  : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Quantity   : Concorde.Quantities.Quantity_Type;
      Sale_Price : Concorde.Money.Price_Type);

   procedure Clear_Wanted
     (Ship      : in out Root_Ship_Type'Class);

   function Has_Wanted
     (Ship : Root_Ship_Type'Class)
      return Boolean;

   function Has_Offers
     (Ship : Root_Ship_Type'Class)
      return Boolean;

   type Ship_Type is access constant Root_Ship_Type'Class;

private

   type Bounding_Box_Type is
      record
         X1, X2 : Real;
         Y1, Y2 : Real;
         Z1, Z2 : Real;
      end record;

   type Root_Ship_Type is abstract new Concorde.Agents.Root_Agent_Type
     and Concorde.Objects.User_Named_Object_Interface
     and Concorde.Locations.Located_Interface
     and Concorde.Ownership.Owned_Interface
     and Memor.Identifier_Record_Type with
      record
         Moving           : Boolean := False;
         Jumping          : Boolean := False;
         Identity         : String (1 .. 6);
         Ship_Name        : Ada.Strings.Unbounded.Unbounded_String;
         Owner            : access constant
           Concorde.Ownership.Owner_Interface'Class;
         Destination      : Concorde.Locations.Object_Location;
         Jump_Destination : access constant
           Concorde.Systems.Root_Star_System_Type'Class;
         Start_Time       : Concorde.Calendar.Time;
         Arrival_Time     : Concorde.Calendar.Time;
         Buying           : Concorde.Commodities.Root_Stock_Type;
         Selling          : Concorde.Commodities.Root_Stock_Type;
      end record;

   overriding function Daily_Budget
     (Ship      : Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Unit_Real;

   overriding function Daily_Needs
     (Ship      : Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Non_Negative_Real;

   overriding function Daily_Supply
     (Ship      : Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Non_Negative_Real;

   overriding function Offer_Strategy
     (Ship      : Root_Ship_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Trades.Offer_Price_Strategy
   is (case Commodity.Class is when others => Concorde.Trades.Average_Price);

   overriding function Name
     (Ship : Root_Ship_Type)
      return String
   is (Ship.Identifier & " "
       & Ada.Strings.Unbounded.To_String (Ship.Ship_Name));

   overriding function Short_Name
     (Ship : Root_Ship_Type)
      return String
   is (Root_Ship_Type'Class (Ship).Name);

   overriding function Owner
     (Ship : Root_Ship_Type)
      return access constant Concorde.Ownership.Owner_Interface'Class
   is (Ship.Owner);

   function Destination
     (Ship : Root_Ship_Type'Class)
      return Concorde.Locations.Object_Location
   is (Ship.Destination);

end Concorde.Ships;
