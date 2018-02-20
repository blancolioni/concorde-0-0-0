with Concorde.Commodities;

package Concorde.Construction is

   type Constructed_Interface is limited interface;

   function Construction_Stock
     (Constructed : Constructed_Interface)
      return Concorde.Commodities.Stock_Interface'Class
      is abstract;

   function Maintenance_Stock
     (Constructed : Constructed_Interface)
      return Concorde.Commodities.Stock_Interface'Class
      is abstract;

   type Constructed_Record is
     new Constructed_Interface with private;

   overriding function Construction_Stock
     (Constructed : Constructed_Record)
      return Concorde.Commodities.Stock_Interface'Class;

   overriding function Maintenance_Stock
     (Constructed : Constructed_Record)
      return Concorde.Commodities.Stock_Interface'Class;

   procedure Create
     (Rec       : in out Constructed_Record'Class;
      Construct : Concorde.Commodities.Stock_Interface'Class;
      Maintain  : Concorde.Commodities.Stock_Interface'Class);

private

   type Constructed_Record is
     new Constructed_Interface with
      record
         Construct : Concorde.Commodities.Root_Stock_Type;
         Maintain  : Concorde.Commodities.Root_Stock_Type;
      end record;

   overriding function Construction_Stock
     (Constructed : Constructed_Record)
      return Concorde.Commodities.Stock_Interface'Class
   is (Constructed.Construct);

   overriding function Maintenance_Stock
     (Constructed : Constructed_Record)
      return Concorde.Commodities.Stock_Interface'Class
   is (Constructed.Maintain);

end Concorde.Construction;
