with Memor.Database;

private package Concorde.People.Pops.Db is
  new Memor.Database
    (Class_Name        => "pop",
     Element_Type      => Root_Pop_Type,
     Element_Reference => Pop_Type);
