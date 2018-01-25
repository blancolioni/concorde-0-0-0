package Concorde.People.Abilities is

   type Ability_Type is
     (Avarice, Charisma, Empathy, Energy,
      Health, Honesty, Intelligence, Strength);

   type Ability_Score_Range is range 0 .. 30;

   function Exists
     (S : String)
      return Boolean;

   function Get
     (S : String)
      return Ability_Type
     with Pre => Exists (S);

end Concorde.People.Abilities;
