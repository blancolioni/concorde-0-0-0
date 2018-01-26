with Concorde.Government;
with Concorde.Trades;
with Concorde.Worlds;

package body Concorde.Laws.Tax_Laws is

   type Commodity_Tax_Law is
     abstract new Root_Tax_Law_Type with
      record
         Government : Concorde.Government.Government_Type;
         Commodity  : Concorde.Commodities.Commodity_Type;
      end record;

   type Sales_Tax_Law is
     new Commodity_Tax_Law with null record;

   overriding procedure Enact (Law : in out Sales_Tax_Law) is null;
   overriding procedure Repeal (Law : in out Sales_Tax_Law) is null;
   overriding function Show (Law : Sales_Tax_Law) return String
   is ("sales tax");

   type Import_Tariff_Law is
     new Commodity_Tax_Law with null record;

   overriding procedure Enact (Law : in out Import_Tariff_Law) is null;
   overriding procedure Repeal (Law : in out Import_Tariff_Law) is null;
   overriding function Show (Law : Import_Tariff_Law) return String
   is ("import tariff");

   function Context_Government
     (Context : Law_Context)
      return Concorde.Government.Government_Type;

   ------------------------
   -- Context_Government --
   ------------------------

   function Context_Government
     (Context : Law_Context)
      return Concorde.Government.Government_Type
   is
      function Gov
        (Item : not null access constant
           Concorde.Objects.Root_Object_Type'Class)
         return Concorde.Government.Government_Type;

      ---------
      -- Gov --
      ---------

      function Gov
        (Item : not null access constant
           Concorde.Objects.Root_Object_Type'Class)
         return Concorde.Government.Government_Type
      is
         use Concorde.Government;
         use Concorde.Worlds;
      begin
         if Item.all in Root_Government_Type'Class then
            return Government_Type (Item);
         elsif Item.all in Root_World_Type'Class then
            return Root_World_Type'Class (Item.all).Government;
         else
            raise Constraint_Error with
              "cannot find a government for object " & Item.Identifier;
         end if;
      end Gov;

   begin
      return Gov (Context.Target);
   end Context_Government;

   -------------------
   -- Import_Tariff --
   -------------------

   function Import_Tariff
     (Context   : Law_Context;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Rate      : Unit_Real)
      return Law_Type
   is
      Government : constant Concorde.Government.Government_Type :=
                     Context_Government (Context);

   begin
      return new Import_Tariff_Law'
        (Level         => 2,
         Context       => Context,
         Previous_Rate =>
           Government.Tax_Rate (Concorde.Trades.Sales, Commodity),
         New_Rate      => Rate,
         Commodity     => Concorde.Commodities.Commodity_Type (Commodity),
         Government    => Government);
   end Import_Tariff;

   -------------------
   -- Import_Tariff --
   -------------------

   function Import_Tariff
     (Context   : Law_Context;
      Rate      : Unit_Real)
      return Law_Type
   is
      Government : constant Concorde.Government.Government_Type :=
                     Context_Government (Context);

   begin
      return new Import_Tariff_Law'
        (Level         => 2,
         Context       => Context,
         Previous_Rate => 0.0,
         New_Rate      => Rate,
         Commodity     => null,
         Government    => Government);
   end Import_Tariff;

   ---------------
   -- Sales_Tax --
   ---------------

   function Sales_Tax
     (Context   : Law_Context;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Rate      : Unit_Real)
      return Law_Type
   is
      Government : constant Concorde.Government.Government_Type :=
                     Context_Government (Context);

   begin
      return new Sales_Tax_Law'
        (Level         => 2,
         Context       => Context,
         Previous_Rate =>
           Government.Tax_Rate (Concorde.Trades.Sales, Commodity),
         New_Rate      => Rate,
         Commodity     => Concorde.Commodities.Commodity_Type (Commodity),
         Government    => Government);
   end Sales_Tax;

   ---------------
   -- Sales_Tax --
   ---------------

   function Sales_Tax
     (Context   : Law_Context;
      Rate      : Unit_Real)
      return Law_Type
   is
      Government : constant Concorde.Government.Government_Type :=
                     Context_Government (Context);

   begin
      return new Sales_Tax_Law'
        (Level         => 2,
         Context       => Context,
         Previous_Rate => 0.0,
         New_Rate      => Rate,
         Commodity     => null,
         Government    => Government);
   end Sales_Tax;

end Concorde.Laws.Tax_Laws;
