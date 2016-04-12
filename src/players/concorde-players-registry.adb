with Concorde.Hash_Table;

with Concorde.Players.Null_Player;
with Concorde.Players.Simple_Player;

package body Concorde.Players.Registry is

   package Player_Table is
     new Concorde.Hash_Table (Player_Type);

   Players : Player_Table.Map;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Player_Type is
   begin
      return Players.Element (Name);
   end Get;

   ----------------------
   -- Register_Players --
   ----------------------

   procedure Register_Players is
   begin
      Players.Insert
        ("null", Null_Player.Get_Null_Player);
      Players.Insert
        ("simple", Simple_Player.Get_Simple_Player);
   end Register_Players;

end Concorde.Players.Registry;
