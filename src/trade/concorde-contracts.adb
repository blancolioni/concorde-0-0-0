package body Concorde.Contracts is

   ---------------------
   -- Accept_Contract --
   ---------------------

   procedure Accept_Contract
     (Contractor : not null access constant Contractor_Interface'Class;
      Contract   : Contract_Type)
   is
   begin
      Contract.Update.Accepted_By := Contractor_Type (Contractor);
      Contract.Update.Accepted := Concorde.Calendar.Clock;
   end Accept_Contract;

   ---------------------
   -- Cancel_Contract --
   ---------------------

   procedure Cancel_Contract
     (Contract : Contract_Type)
   is
   begin
      Contract.Update.Active := False;
      Contract.Update.Canceled := True;
      Contract.Update.Completed := Concorde.Calendar.Clock;
   end Cancel_Contract;

   -----------------------
   -- Complete_Contract --
   -----------------------

   procedure Complete_Contract
     (Contract : Contract_Type)
   is
   begin
      Contract.Update.Active := False;
      Contract.Update.Completed := Concorde.Calendar.Clock;
   end Complete_Contract;

   ----------------------
   -- New_Buy_Contract --
   ----------------------

   function New_Buy_Contract
     (Location  : Concorde.Locations.Object_Location;
      Buyer     : not null access constant Contractor_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type;
      Expires   : Concorde.Calendar.Time)
      return Contract_Type
   is

      procedure Create (Contract : in out Root_Contract_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Contract : in out Root_Contract_Type'Class) is
      begin
         Contract.Location := Location;
         Contract.Offered_By := Contractor_Type (Buyer);
         Contract.Accepted_By := null;
         Contract.Commodity := Commodity;
         Contract.Quantity := Quantity;
         Contract.Price := Price;
         Contract.Expires := Expires;
         Contract.Issued := Concorde.Calendar.Clock;
         Contract.Active := True;
         Contract.Canceled := False;
--           Buyer.Log
--             ("offers to buy " & WL.Quantities.Show (Quantity)
--              & " " & Commodity.Name & " @ " & WL.Money.Show (Price));
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
   begin
      Db.Scan (Check);
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
              & WL.Quantities.Show (Contract.Quantity)
              & " "
              & Contract.Commodity.Name
              & " to "
              & Concorde.Locations.Primary (Contract.Location).Identifier
              & " for "
              & WL.Money.Show
              (WL.Money.Total (Contract.Price, Contract.Quantity));
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
