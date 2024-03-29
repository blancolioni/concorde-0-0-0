package body Concorde.Contracts is

   ---------------------
   -- Accept_Contract --
   ---------------------

   procedure Accept_Contract
     (Contractor : not null access constant Contractor_Interface'Class;
      Contract   : Contract_Type)
   is
   begin
      Contract.Update.Accepted := True;
      Contract.Update.Accepted_By := Contractor_Type (Contractor);
      Contract.Update.Accepted_Time := Concorde.Calendar.Clock;
      Contract.Offered_By.On_Contract_Accepted (Contract);
      Contractor.On_Accepted_Contract (Contract);
   end Accept_Contract;

   ---------------------
   -- Cancel_Contract --
   ---------------------

   procedure Cancel_Contract
     (Contract : not null access constant Root_Contract_Type'Class)
   is
   begin
      Contract.Update.Active := False;
      Contract.Update.Canceled := True;
      Contract.Update.Completed_Time := Concorde.Calendar.Clock;
   end Cancel_Contract;

   -----------------------
   -- Complete_Contract --
   -----------------------

   procedure Complete_Contract
     (Contract : not null access constant Root_Contract_Type'Class)
   is
   begin
      Contract.Update.Active := False;
      Contract.Update.Completed_Time := Concorde.Calendar.Clock;
      Contract.Offered_By.On_Contract_Fulfilled
        (Contract_Type (Contract));
      Contract.Accepted_By.On_Contract_Fulfilled
        (Contract_Type (Contract));
   end Complete_Contract;

   ----------------------
   -- New_Buy_Contract --
   ----------------------

   function New_Buy_Contract
     (Location  : Concorde.Locations.Object_Location;
      Buyer     : not null access constant Contractor_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type;
      Expires   : Concorde.Calendar.Time)
      return Contract_Type
   is

      procedure Create (Contract : in out Root_Contract_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Contract : in out Root_Contract_Type'Class) is
      begin
         Contract.Class := Buy_Goods;
         Contract.Location := Location;
         Contract.Offered_By := Contractor_Type (Buyer);
         Contract.Accepted_By := null;
         Contract.Commodity := Commodity;
         Contract.Quantity := Quantity;
         Contract.Price := Price;
         Contract.Expiry_Time := Expires;
         Contract.Issue_Time := Concorde.Calendar.Clock;
         Contract.Active := True;
         Contract.Accepted := False;
         Contract.Canceled := False;
--           Buyer.Log
--             ("offers to buy " & Concorde.Quantities.Show (Quantity)
--              & " " & Commodity.Name & " @ " & Concorde.Money.Show (Price));
      end Create;

   begin
      return Db.Create (Create'Access);
   end New_Buy_Contract;

   --------------------
   -- Scan_Contracts --
   --------------------

   procedure Scan_Available_Contracts
     (Check : not null access
        procedure (Contract : Contract_Type))
   is
      function Is_Available (Contract : Contract_Type) return Boolean
      is (Contract.Active and then not Contract.Accepted);

   begin
      Db.Scan (Is_Available'Access, Check);
   end Scan_Available_Contracts;

   ----------
   -- Show --
   ----------

   function Show
     (Contract : Root_Contract_Type'Class)
      return String
   is
   begin
      case Contract.Class is
         when Buy_Goods =>
            return "ship "
              & Concorde.Quantities.Show (Contract.Quantity)
              & " "
              & Contract.Commodity.Name
              & " to "
              & Concorde.Locations.Primary (Contract.Location).Identifier
              & " for "
              & Concorde.Money.Show
              (Concorde.Money.Total (Contract.Price, Contract.Quantity));
      end case;
   end Show;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Contract_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Concorde.Contracts;
