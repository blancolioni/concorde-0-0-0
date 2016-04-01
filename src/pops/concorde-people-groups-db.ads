with Memor.Database;

private package Concorde.People.Groups.Db is
  new Memor.Database
    (Class_Name        => "pop-group",
     Element_Type      => Root_Pop_Group,
     Element_Reference => Pop_Group);
