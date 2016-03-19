package body Concorde.AI.Default is

   type Default_AI_Type is new Root_AI_Type with null record;

   ----------------
   -- Default_AI --
   ----------------

   function Default_AI return AI_Type is
   begin
      return new Default_AI_Type;
   end Default_AI;

end Concorde.AI.Default;
