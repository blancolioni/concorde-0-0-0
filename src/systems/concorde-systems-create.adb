package body Concorde.Systems.Create is

   ----------------
   -- New_System --
   ----------------

   function New_System
     (Index      : Positive;
      Name       : String;
      X, Y       : Real;
      Production : Non_Negative_Real;
      Capacity   : Non_Negative_Real)
      return Star_System_Access
   is
      Result : constant Star_System_Access :=
                 new Root_Star_System_Type'
                   (Concorde.Objects.Root_Named_Object_Type with
                    Index         => Index,
                    X             => X, Y => Y,
                    Production    => Production,
                    Capacity      => Capacity,
                    Progress      => 0.0,
                    Ships         => Concorde.Ships.Lists.Empty_List,
                    Arriving      => Concorde.Ships.Lists.Empty_List,
                    Departing     => Concorde.Ships.Lists.Empty_List,
                    Last_Battle   => 0,
                    Battle_Size   => 0,
                    Last_Attacker => null,
                    Capital       => False,
                    Owner         => null,
                    Edges         => Edge_Info_Lists.Empty_List);
   begin
      Result.Set_Name (Name);
      return Result;
   end New_System;

end Concorde.Systems.Create;
