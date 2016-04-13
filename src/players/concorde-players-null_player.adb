package body Concorde.Players.Null_Player is

   type Root_Null_Player_Type is
     new Root_Player_Type with null record;

   overriding procedure On_Start
     (Player : in out Root_Null_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class)
   is null;

   overriding procedure On_Ship_Completed
     (Player : in out Root_Null_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      Ship   : Concorde.Ships.Ship_Type)
   is null;

   overriding procedure On_Ship_Arrived
     (Player : in out Root_Null_Player_Type;
      Empire : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Ship   : Concorde.Ships.Ship_Type)
   is null;

   Local_Null_Player : aliased Root_Null_Player_Type;

   ------------
   -- Create --
   ------------

   function Create return Player_Type is
   begin
      return Local_Null_Player'Access;
   end Create;

end Concorde.Players.Null_Player;
