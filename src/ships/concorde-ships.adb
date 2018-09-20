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
      Ship.Wanted.Add_Quantity
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
      Ship.Wanted.Clear_Stock;
   end Clear_Wanted;

   ------------------
   -- Daily_Budget --
   ------------------

   overriding function Daily_Budget
     (Ship      : Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Unit_Real
   is
      pragma Unreferenced (Ship, Commodity);
   begin
      return 1.0;
   end Daily_Budget;

   -----------------
   -- Daily_Needs --
   -----------------

   overriding function Daily_Needs
     (Ship      : Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Non_Negative_Real
   is
   begin
      return Concorde.Quantities.To_Real
        (Ship.Wanted.Get_Quantity (Commodity));
   end Daily_Needs;

   ------------------
   -- Daily_Supply --
   ------------------

   overriding function Daily_Supply
     (Ship      : Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Non_Negative_Real
   is
      use Concorde.Quantities;
      Have : constant Quantity_Type := Ship.Get_Quantity (Commodity);
      Want : constant Quantity_Type := Ship.Wanted.Get_Quantity (Commodity);
   begin
      if Have > Want then
         return To_Real (Want - Have);
      else
         return 0.0;
      end if;
   end Daily_Supply;

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
