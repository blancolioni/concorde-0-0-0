with Concorde.Hash_Table;

with Concorde.Players.Null_Player;
with Concorde.Players.Simple_Player;

package body Concorde.Players.Registry is

   type Player_Create_Function is access
     function return Player_Type;

   package Player_Table is
     new Concorde.Hash_Table (Player_Create_Function);

   Players : Player_Table.Map;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Player_Type is
   begin
      return Players.Element (Name).all;
   end Get;

   ----------------------
   -- Register_Players --
   ----------------------

   procedure Register_Players is
   begin
      Players.Insert
        ("null", Null_Player.Create'Access);
      Players.Insert
        ("simple", Simple_Player.Create'Access);
   end Register_Players;

end Concorde.Players.Registry;
