package body Concorde.AI.Defensive is

   type Defensive_AI_Type is new Root_AI_Type with null record;

   overriding function Minimum_Attack_Factor
     (AI : Defensive_AI_Type)
      return Non_Negative_Real
   is (3.0);

   overriding function Minimum_Defense_Factor
     (AI : Defensive_AI_Type)
      return Non_Negative_Real
   is (1.2);

   ----------------
   -- Defensive_AI --
   ----------------

   function Defensive_AI return AI_Type is
   begin
      return new Defensive_AI_Type;
   end Defensive_AI;

end Concorde.AI.Defensive;
