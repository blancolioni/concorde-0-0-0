package Concorde.People.Proficiencies is

   type Proficiency_Type is (Education, Influence, Prestige, Contacts);

   type Proficiency_Score_Range is range 0 .. 30;

   function Exists
     (S : String)
      return Boolean;

   function Get
     (S : String)
      return Proficiency_Type
     with Pre => Exists (S);

end Concorde.People.Proficiencies;
