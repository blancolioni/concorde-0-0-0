private with WL.Guids.Maps;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Concorde.Trades.Offer_Maps is

   type Offer_Map is tagged private;

   function Contains
     (Map       : Offer_Map'Class;
      Reference : Offer_Reference)
      return Boolean;

   function Element
     (Map       : Offer_Map'Class;
      Reference : Offer_Reference)
      return Element_Type;

   procedure Insert
     (Map       : in out Offer_Map'Class;
      Reference : Offer_Reference;
      Element   : Element_Type);

   procedure Delete
     (Map       : in out Offer_Map'Class;
      Reference : Offer_Reference);

private

   package Element_Maps is
     new WL.Guids.Maps (Element_Type, "=");

   type Offer_Map is tagged
      record
         Map : Element_Maps.Map;
      end record;

   function Contains
     (Map       : Offer_Map'Class;
      Reference : Offer_Reference)
      return Boolean
   is (Map.Map.Contains (WL.Guids.Guid (Reference)));

   function Element
     (Map       : Offer_Map'Class;
      Reference : Offer_Reference)
      return Element_Type
   is (Map.Map.Element (WL.Guids.Guid (Reference)));

end Concorde.Trades.Offer_Maps;
