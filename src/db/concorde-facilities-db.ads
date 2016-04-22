with Memor.Database;

package Concorde.Facilities.Db is
  new Memor.Database
    (Class_Name        => "facility",
     Element_Type      => Root_Facility_Type,
     Element_Reference => Facility_Type);
