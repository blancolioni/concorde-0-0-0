with Concorde.Galaxy;

with Concorde.Empires.Db;
with Concorde.Ships.Lists;
with Concorde.Systems.Lists;

package body Concorde.Players.Simple_Player is

   type Root_Simple_Player_Type is new Root_Player_Type with
      record
         Idle_Ships : Concorde.Ships.Lists.List;
         Unexplored_Targets : Concorde.Systems.Lists.List;
      end record;

   overriding procedure On_Start
     (Player : in out Root_Simple_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class);

   overriding procedure On_Border_Changed
     (Player : in out Root_Simple_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class)
   is null;

   overriding procedure On_Ship_Completed
     (Player : in out Root_Simple_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      Ship   : Concorde.Ships.Ship_Type);

   overriding procedure On_Ship_Arrived
     (Player : in out Root_Simple_Player_Type;
      Empire : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Ship   : Concorde.Ships.Root_Ship_Type'Class)
   is null;

   Local_Simple_Player : aliased Root_Simple_Player_Type;

   ---------------------
   -- Get_Simple_Player --
   ---------------------

   function Get_Simple_Player return Player_Type is
   begin
      return Local_Simple_Player'Access;
   end Get_Simple_Player;

   -----------------------
   -- On_Ship_Completed --
   -----------------------

   overriding procedure On_Ship_Completed
     (Player : in out Root_Simple_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      Ship   : Concorde.Ships.Ship_Type)
   is
   begin
      if Player.Unexplored_Targets.Is_Empty then
         Player.Idle_Ships.Append (Ship);
      else
         Player.Order_Move_Ship
           (Concorde.Empires.Db.Reference (Empire),
            Ship, Player.Unexplored_Targets.First_Element);
         Player.Unexplored_Targets.Delete_First;
      end if;
   end On_Ship_Completed;

   --------------
   -- On_Start --
   --------------

   overriding procedure On_Start
     (Player : in out Root_Simple_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class)
   is
      use Concorde.Galaxy;
      Ns : constant Array_Of_Star_Systems :=
             Neighbours (Empire.Capital);
   begin
      for N of Ns loop
         if not N.Owned then
            Player.Unexplored_Targets.Append (N);
         end if;
      end loop;
   end On_Start;

end Concorde.Players.Simple_Player;
