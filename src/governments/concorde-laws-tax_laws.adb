with Concorde.Factions;
with Concorde.Government;
with Concorde.Powers.Taxation;
with Concorde.Trades;
with Concorde.Worlds;
with Concorde.People.Communities;

package body Concorde.Laws.Tax_Laws is

   type Commodity_Tax_Law is
     new Root_Tax_Law_Type with
      record
         Faction    : Concorde.Factions.Faction_Type;
         Government : Concorde.Government.Government_Type;
         Commodity  : Concorde.Commodities.Commodity_Type;
         Category   : Concorde.Trades.Market_Tax_Category;
      end record;

   overriding function Can_Enact (Law : Commodity_Tax_Law) return Boolean
   is (Law.Faction.Has_Power
       (Concorde.Powers.Taxation.Set_Tax_Rate (Law.Category)));

   overriding procedure Enact (Law : in out Commodity_Tax_Law);
   overriding procedure Repeal (Law : in out Commodity_Tax_Law);
   overriding function Show (Law : Commodity_Tax_Law) return String;

   function New_Tax_Law
     (Context   : Law_Context;
      Level     : Law_Level;
      Category  : Concorde.Trades.Market_Tax_Category;
      Commodity : Concorde.Commodities.Commodity_Type;
      Rate      : Unit_Real)
      return Law_Type;

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
           Concorde.Laws.Law_Target_Interface'Class)
         return Concorde.Government.Government_Type;

      ---------
      -- Gov --
      ---------

      function Gov
        (Item : not null access constant
           Concorde.Laws.Law_Target_Interface'Class)
         return Concorde.Government.Government_Type
      is
         use Concorde.Government;
         use Concorde.People.Communities;
      begin
         if Item.all in Root_Government_Type'Class then
            return Government_Type (Item);
         elsif Item.all in Root_Community_Type'Class then
            return Root_Community_Type'Class (Item.all).Government;
         else
            raise Constraint_Error with
              "cannot find a government for object "
              & Concorde.Objects.Object_Type (Item).Identifier;
         end if;
      end Gov;

   begin
      return Gov (Context.Target);
   end Context_Government;

   -----------
   -- Enact --
   -----------

   overriding procedure Enact (Law : in out Commodity_Tax_Law) is
      use type Concorde.Commodities.Commodity_Type;
   begin
      if Law.Commodity = null then
         Law.Government.Update.Set_Base_Tax_Rate
           (Law.Category, Law.Rate);
      else
         Law.Government.Update.Set_Tax_Rate
           (Law.Category, Law.Commodity, Law.Rate);
      end if;
   end Enact;

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
   begin
      return New_Tax_Law
        (Context, Legislation, Concorde.Trades.Import,
         Concorde.Commodities.Commodity_Type (Commodity), Rate);
   end Import_Tariff;

   -------------------
   -- Import_Tariff --
   -------------------

   function Import_Tariff
     (Context   : Law_Context;
      Rate      : Unit_Real)
      return Law_Type
   is
   begin
      return New_Tax_Law (Context, Legislation, Concorde.Trades.Import,
                          null, Rate);
   end Import_Tariff;

   -----------------
   -- New_Tax_Law --
   -----------------

   function New_Tax_Law
     (Context   : Law_Context;
      Level     : Law_Level;
      Category  : Concorde.Trades.Market_Tax_Category;
      Commodity : Concorde.Commodities.Commodity_Type;
      Rate      : Unit_Real)
      return Law_Type
   is
      use type Concorde.Commodities.Commodity_Type;
      Government : constant Concorde.Government.Government_Type :=
                     Context_Government (Context);

   begin
      return new Commodity_Tax_Law'
        (Level         => Level,
         Context       => Context,
         Previous_Rate =>
           (if Commodity = null
            then Government.Base_Tax_Rate (Category)
            else Government.Tax_Rate (Category, Commodity)),
         New_Rate      => Rate,
         Commodity     => Commodity,
         Faction       => Concorde.Factions.Faction_Type (Context.Legislator),
         Government    => Government,
         Category      => Category);
   end New_Tax_Law;

   ------------
   -- Repeal --
   ------------

   overriding procedure Repeal (Law : in out Commodity_Tax_Law) is
      use type Concorde.Commodities.Commodity_Type;
   begin
      if Law.Commodity = null then
         Law.Government.Update.Set_Base_Tax_Rate
           (Law.Category, Law.Previous_Rate);
      else
         Law.Government.Update.Set_Tax_Rate
           (Law.Category, Law.Commodity, Law.Previous_Rate);
      end if;
   end Repeal;

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
   begin
      return New_Tax_Law
        (Context, Legislation, Concorde.Trades.Sales,
         Concorde.Commodities.Commodity_Type (Commodity), Rate);
   end Sales_Tax;

   ---------------
   -- Sales_Tax --
   ---------------

   function Sales_Tax
     (Context   : Law_Context;
      Rate      : Unit_Real)
      return Law_Type
   is
   begin
      return New_Tax_Law
        (Context, Legislation, Concorde.Trades.Sales,
         null, Rate);
   end Sales_Tax;

   ----------
   -- Show --
   ----------

   overriding function Show
     (Law : Commodity_Tax_Law)
      return String
   is
      use type Concorde.Commodities.Commodity_Type;
      Category_Name : constant String :=
                        (case Law.Category is
                            when Concorde.Trades.Sales =>
                               "sales tax",
                            when Concorde.Trades.Import =>
                               "import tariff",
                            when Concorde.Trades.Export =>
                               "export tariff");
      Specific_Name : constant String :=
                        (if Law.Commodity = null
                         then "base " & Category_Name
                         else Category_Name & " on " & Law.Commodity.Name);
      Rate_Name : constant String :=
                    " at" & Natural'Image (Natural (Law.Rate * 100.0))
                    & "%";
   begin
      return Specific_Name & Rate_Name;
   end Show;

end Concorde.Laws.Tax_Laws;
