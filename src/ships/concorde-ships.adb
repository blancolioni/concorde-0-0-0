with Concorde.Systems;

package body Concorde.Ships is

   ----------------
   -- Add_Wanted --
   ----------------

   procedure Add_Wanted
     (Ship       : in out Root_Ship_Type'Class;
      Commodity  : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Quantity   : Concorde.Quantities.Quantity_Type;
      Sale_Price : Concorde.Money.Price_Type)
   is
      use Concorde.Money;
   begin
      Ship.Buying.Add_Quantity
        (Item     => Concorde.Commodities.Commodity_Type (Commodity),
         Quantity => Quantity,
         Value    => Total (Sale_Price, Quantity));
   end Add_Wanted;

   -----------------------
   -- Clear_Destination --
   -----------------------

   procedure Clear_Destination
     (Ship   : in out Root_Ship_Type'Class)
   is
   begin
      Ship.Moving := False;
   end Clear_Destination;

   ------------------
   -- Clear_Wanted --
   ------------------

   procedure Clear_Wanted
     (Ship      : in out Root_Ship_Type'Class)
   is
   begin
      Ship.Selling.Clear_Stock;
      declare
         procedure Update (Commodity : Concorde.Commodities.Commodity_Type);

         ------------
         -- Update --
         ------------

         procedure Update (Commodity : Concorde.Commodities.Commodity_Type) is
         begin
            Ship.Selling.Add_Quantity
              (Commodity,
               Ship.Get_Quantity (Commodity),
               Ship.Get_Value (Commodity));
         end Update;

      begin
         Ship.Buying.Scan_Stock (Update'Access);
      end;

      Ship.Buying.Clear_Stock;

   end Clear_Wanted;

   ------------------
   -- Daily_Budget --
   ------------------

   overriding function Daily_Budget
     (Ship      : Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Money_Type
   is
      pragma Unreferenced (Commodity);
   begin
      return Ship.Cash;
   end Daily_Budget;

   -----------------
   -- Daily_Needs --
   -----------------

   overriding function Daily_Needs
     (Ship      : Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
      Available : constant Quantity_Type :=
                    Ship.Available_Capacity
                      + Ship.Get_Quantity (Commodity);
      Wanted    : constant Quantity_Type :=
                    Ship.Buying.Get_Quantity (Commodity);
   begin
      if Wanted > Zero then
         Ship.Log
           (Commodity.Identifier
            & ": want " & Image (Wanted)
            & "; have " & Image (Ship.Get_Quantity (Commodity))
            & "; available " & Image (Available));
      end if;
      return Min (Available, Wanted);
   end Daily_Needs;

   ------------------
   -- Daily_Supply --
   ------------------

   overriding function Daily_Supply
     (Ship      : Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
      Have : constant Quantity_Type := Ship.Selling.Get_Quantity (Commodity);
      Want : constant Quantity_Type := Ship.Buying.Get_Quantity (Commodity);
   begin
      if Have > Want then
         Ship.Log ("supplying " & Show (Have - Want)
                   & " " & Commodity.Identifier);
         return Have - Want;
      else
         return Zero;
      end if;
   end Daily_Supply;

   ----------------
   -- Has_Offers --
   ----------------

   function Has_Offers
     (Ship : Root_Ship_Type'Class)
      return Boolean
   is
      use Concorde.Quantities;
   begin
      return Ship.Selling.Total_Quantity > Zero;
   end Has_Offers;

   ----------------
   -- Has_Wanted --
   ----------------

   function Has_Wanted
     (Ship : Root_Ship_Type'Class)
      return Boolean
   is
      use Concorde.Quantities;
   begin
      return Ship.Buying.Total_Quantity > Zero;
   end Has_Wanted;

   ----------------------
   -- On_Commodity_Buy --
   ----------------------

   overriding procedure On_Commodity_Buy
     (Ship      : in out Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
   begin
      Concorde.Agents.Root_Agent_Type (Ship)
        .On_Commodity_Buy (Commodity, Quantity, Price);
   end On_Commodity_Buy;

   -----------------------
   -- On_Commodity_Sell --
   -----------------------

   overriding procedure On_Commodity_Sell
     (Ship      : in out Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
      use Concorde.Money;
   begin
      Concorde.Agents.Root_Agent_Type (Ship)
        .On_Commodity_Sell (Commodity, Quantity, Price);
      Ship.Selling.Remove_Quantity
        (Item     => Commodity,
         Quantity => Quantity,
         Earn     => Total (Price, Quantity));
   end On_Commodity_Sell;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship         : in out Root_Ship_Type'Class;
      Destination  : Concorde.Locations.Object_Location;
      Start_Time   : Concorde.Calendar.Time;
      Journey_Time : Duration)
   is
      use Concorde.Calendar;
   begin
      Ship.Destination := Destination;
      Ship.Moving := True;
      Ship.Jumping := False;
      Ship.Start_Time := Start_Time;
      Ship.Arrival_Time := Start_Time + Journey_Time;
      if Ship.Has_Market then
         Ship.Leave_Market;
      end if;
   end Set_Destination;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship         : in out Root_Ship_Type'Class;
      Destination  : not null access constant
        Concorde.Systems.Star_System_Object_Interface'Class;
      Start_Time   : Concorde.Calendar.Time;
      Journey_Time : Duration)
   is
   begin
      Ship.Set_Destination
        (Concorde.Locations.Geosynchronous_Orbit (Destination),
         Start_Time, Journey_Time);
   end Set_Destination;

   --------------------------
   -- Set_Jump_Destination --
   --------------------------

   procedure Set_Jump_Destination
     (Ship         : in out Root_Ship_Type'Class;
      System       : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Start_Time   : Concorde.Calendar.Time;
      Journey_Time : Duration)
   is
      use Concorde.Calendar;
   begin
      Ship.Destination :=
        Concorde.Locations.System_Transfer_Orbit
          (System, Ship.Current_System);
      Ship.Moving := True;
      Ship.Jumping := True;
      Ship.Start_Time := Start_Time;
      Ship.Arrival_Time := Start_Time + Journey_Time;
   end Set_Jump_Destination;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Ship : in out Root_Ship_Type;
      Name : String)
   is
   begin
      Ship.Ship_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

end Concorde.Ships;
