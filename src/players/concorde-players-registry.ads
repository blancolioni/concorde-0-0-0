package Concorde.Players.Registry is

   procedure Register_Players;

   function Get (Name : String) return Player_Type;

end Concorde.Players.Registry;
