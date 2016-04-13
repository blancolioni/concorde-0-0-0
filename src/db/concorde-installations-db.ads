with Memor.Database;

private package Concorde.Installations.Db is
  new Memor.Database
    (Class_Name        => "installation",
     Element_Type      => Root_Installation_Type,
     Element_Reference => Installation_Type);
