with Memor.Database;

package Concorde.Commodities.Db is
  new Memor.Database
    (Class_Name        => "commodity",
     Element_Type      => Root_Commodity_Type,
     Element_Reference => Commodity_Type);
