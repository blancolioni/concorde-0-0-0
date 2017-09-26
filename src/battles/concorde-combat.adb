package body Concorde.Combat is

   ----------
   -- Done --
   ----------

   function Done (Arena : in out Root_Combat_Arena'Class) return Boolean is
   begin
      return Arena.Finished;
   end Done;

   -------------
   -- Execute --
   -------------

   procedure Execute (Arena : in out Root_Combat_Arena'Class) is
   begin
      while not Arena.Done loop
         Arena.Tick;
      end loop;
   end Execute;

   ------------
   -- Winner --
   ------------

   function Winner
     (Arena : Root_Combat_Arena)
      return Concorde.Factions.Faction_Type
   is
   begin
      return Arena.Winner;
   end Winner;

end Concorde.Combat;
