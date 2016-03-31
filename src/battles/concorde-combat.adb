package body Concorde.Combat is

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
      return Concorde.Empires.Empire_Type
   is
   begin
      return Arena.Winner;
   end Winner;

end Concorde.Combat;
