with Memor.Database;

package Concorde.People.Skills.Db is
  new Memor.Database
    (Class_Name        => "pop-skill",
     Element_Type      => Root_Pop_Skill,
     Element_Reference => Pop_Skill);
