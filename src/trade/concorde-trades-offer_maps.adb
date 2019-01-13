package body Concorde.Trades.Offer_Maps is

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Map       : in out Offer_Map'Class;
      Reference : Offer_Reference)
   is
   begin
      Map.Map.Delete (WL.Guids.Guid (Reference));
   end Delete;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Map       : in out Offer_Map'Class;
      Reference : Offer_Reference;
      Element   : Element_Type)
   is
   begin
      Map.Map.Insert (WL.Guids.Guid (Reference), Element);
   end Insert;

end Concorde.Trades.Offer_Maps;
