with Ada.Unchecked_Deallocation;

with WL.Random;

with Lui.Colours;

with Concorde.Elementary_Functions;
with Concorde.Random;

with Concorde.Empires.Relations;

package body Concorde.Combat.Ship_Combat is

   Max_Turns : constant := 1_000;

   function Distance (X1, Y1, X2, Y2 : Real) return Non_Negative_Real
   is (Concorde.Elementary_Functions.Sqrt
       ((X1 - X2) ** 2 + (Y1 - Y2) ** 2));

   -------------------
   -- Add_Combatant --
   -------------------

   procedure Add_Combatant
     (Arena     : in out Root_Combat_Arena'Class;
      Combatant : not null access Concorde.Ships.Root_Ship_Type'Class;
      Team      : not null access Concorde.Empires.Root_Empire_Type'Class;
      X, Y      : Real;
      Facing    : Radians)
   is
      use Concorde.Empires;
      use Concorde.Ships;
      Empire : constant Empire_Type := Empire_Type (Team);
      New_Team : Boolean := True;
   begin
      Arena.Ships.Append
        ((Ship_Type (Combatant), Arena.Ships.Last_Index + 1,
         X, Y, Facing, 0));

      for I in 1 .. Arena.Teams.Last_Index loop
         if Arena.Teams (I).Leader = Empire then
            Arena.Teams (I).Ships.Append (Arena.Ships.Last_Index);
            New_Team := False;
            exit;
         end if;
      end loop;

      if New_Team then
         declare
            Ss : Ship_Index_Vectors.Vector;
         begin
            Ss.Append (Arena.Ships.Last_Index);
            Arena.Teams.Append ((Empire, Ss));
         end;
      end if;

   end Add_Combatant;

   -------------------
   -- Choose_Target --
   -------------------

   procedure Choose_Target
     (Model : in out Root_Combat_Arena;
      Ship  : in out Ship_Record)
   is
   begin
      for Team of Model.Teams loop
         if not Team.Ships.Is_Empty
           and then Concorde.Empires.Relations.At_War
             (Ship.Ship.Owner, Team.Leader)
         then
            declare
               Index : constant Positive :=
                         WL.Random.Random_Number (1, Team.Ships.Last_Index);
            begin
               Ship.Target := Team.Ships (Index);
               return;
            end;
         end if;
      end loop;
      Ship.Target := 0;
   end Choose_Target;

   -----------------
   -- Close_Arena --
   -----------------

   procedure Close_Arena
     (Arena : in out Combat_Arena)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Root_Combat_Arena'Class, Combat_Arena);
   begin
      Free (Arena);
   end Close_Arena;

   ------------------
   -- Commit_Event --
   ------------------

   procedure Commit_Event
     (Model  : in out Root_Combat_Arena;
      Event  : Combat_Event)
   is
   begin
      case Event.Event is
         when Weapon_Fired =>
            Model.Ships (Event.Target).Ship.Hit
              (Event.Weapon.Ranged_Damage
                 (Model.Ship_Range (Event.Attacker, Event.Target),
                  Event.Effectiveness));
      end case;
   end Commit_Event;

   ----------
   -- Done --
   ----------

   function Done (Arena : Root_Combat_Arena'Class) return Boolean is
      Active_Team : Boolean := False;
   begin
      if Arena.Turns >= Max_Turns then
         return True;
      end if;

      for Team of Arena.Teams loop
         for Ship_Index of Team.Ships loop
            if Arena.Ships (Ship_Index).Ship.Alive then
               if Active_Team then
                  return False;
               else
                  Active_Team := True;
                  exit;
               end if;
            end if;
         end loop;
      end loop;
      return True;
   end Done;

   -------------
   -- Empires --
   -------------

   function Empires
     (Arena : Root_Combat_Arena'Class)
      return Concorde.Empires.Array_Of_Empires
   is
      use Concorde.Empires;
      Result : Array_Of_Empires (1 .. Arena.Teams.Last_Index);
   begin
      for I in Result'Range loop
         Result (I) := Arena.Teams (I).Leader;
      end loop;
      return Result;
   end Empires;

   -------------
   -- Execute --
   -------------

   procedure Execute (Arena : in out Root_Combat_Arena'Class) is
   begin
      while not Arena.Done loop
         Arena.Tick;
      end loop;
   end Execute;

   -----------------
   -- Fire_Weapon --
   -----------------

   procedure Fire_Weapon
     (Model  : in out Root_Combat_Arena;
      Ship   : in out Ship_Record;
      Module : Concorde.Modules.Module_Type)
   is
      use Concorde.Components.Weapons;
      Weapon : constant Weapon_Component :=
                 Weapon_Component (Module.Component);
   begin
      Model.Events.Insert
        (Model.Events.First,
         (Weapon_Fired, Model.Turns, Ship.Index, Ship.Target,
          Module, Weapon, Module.Stored_Energy,
          1.0 - Module.Damage));
      Module.Execute;
   end Fire_Weapon;

   -----------------
   -- Idle_Update --
   -----------------

   overriding procedure Idle_Update
     (Model    : in out Root_Combat_Arena;
      Updated  : out Boolean)
   is
   begin
      Model.Tick;
      Updated := True;
   end Idle_Update;

   ---------------
   -- New_Arena --
   ---------------

   function New_Arena
     (Name               : String;
      Radius             : Non_Negative_Real;
      Planet_X, Planet_Y : Real;
      Planet_Radius      : Non_Negative_Real)
      return Combat_Arena
   is
      Arena : constant Combat_Arena :=
                new Root_Combat_Arena'
                  (Lui.Models.Root_Object_Model with
                   Radius        => Radius,
                   Planet_X      => Planet_X,
                   Planet_Y      => Planet_Y,
                   Planet_Radius => Planet_Radius,
                   Teams         => Team_Vectors.Empty_Vector,
                   Ships         => Combat_Ship_Vectors.Empty_Vector,
                   Turns         => 0,
                   Events        => List_Of_Combat_Events.Empty_List);
   begin
      Arena.Initialise (Name);
      Arena.Set_Eye_Position (0.0, 0.0, 4000.0);
      return Arena;
   end New_Arena;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Root_Combat_Arena;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      Health_Bar_Width : constant := 10;
      Health_Bar_Height : constant := 4;
   begin
      for Combat_Ship of Model.Ships loop
         declare
            Outline : constant Lui.Rendering.Buffer_Points :=
                        Model.Ship_Outline (Combat_Ship);
            Colour  : Lui.Colours.Colour_Type :=
                        Combat_Ship.Ship.Owner.Colour;
            X, Y    : Integer;
            W       : constant Natural :=
                        Natural (Combat_Ship.Ship.Damage
                                 * Real (Health_Bar_Width));
         begin
            if not Combat_Ship.Ship.Alive then
               Colour.Alpha := 0.3;
            end if;
            Renderer.Draw_Polygon
              (Vertices => Outline,
               Colour   => Colour,
               Filled   => True);
            Model.Ship_Centre (Combat_Ship, X, Y);
            Renderer.Draw_Rectangle
              (X - Health_Bar_Width / 2,
               Y + Combat_Ship.Ship.Size, W, Health_Bar_Height,
               (0.8, 0.0, 0.0, 1.0), True);
            Renderer.Draw_Rectangle
              (X - Health_Bar_Width / 2 + W,
               Y + Combat_Ship.Ship.Size,
               Health_Bar_Width - W, Health_Bar_Height,
               (0.0, 0.8, 0.0, 1.0), True);

         end;
      end loop;

      for Event of Model.Events loop
         case Event.Event is
            when Weapon_Fired =>
               if Model.Turns - Event.Turn < 5 then
                  declare
                     Attacker : Ship_Record renames
                                  Model.Ships (Event.Attacker);
                     Target   : Ship_Record renames
                                  Model.Ships (Event.Target);
                     X1, X2, Y1, Y2 : Integer;
                     Z              : Real;
                     Colour         : Lui.Colours.Colour_Type :=
                                        Lui.Colours.White;
                  begin
                     Model.Get_Screen_Coordinates
                       (Attacker.X, Attacker.Y, 0.0, X1, Y1, Z);
                     Model.Get_Screen_Coordinates
                       (Target.X, Target.Y, 0.0, X2, Y2, Z);
                     Colour.Alpha := 1.0 -
                       Real (Model.Turns - Event.Turn) / 8.0
                       * Event.Effectiveness;
                     Renderer.Draw_Line
                       (X1, Y1, X2, Y2, Colour, 1);
                  end;
               end if;
         end case;
      end loop;

      if Model.Done then
         declare
            use Concorde.Empires;
            Winner : Empire_Type := null;
         begin
            for Team of Model.Teams loop
               for Ship_Index of Team.Ships loop
                  if Model.Ships (Ship_Index).Ship.Alive then
                     Winner := Team.Leader;
                     exit;
                  end if;
               end loop;
               exit when Winner /= null;
            end loop;

            Renderer.Draw_String
              (10, 10, 16, Lui.Colours.White,
               "Victory to " & Winner.Name);
         end;
      end if;

   end Render;

   -----------------
   -- Ship_Centre --
   -----------------

   procedure Ship_Centre
     (Model : Root_Combat_Arena'Class;
      Ship  : Ship_Record;
      X, Y  : out Integer)
   is
      Z : Real;
   begin
      Model.Get_Screen_Coordinates (Ship.X, Ship.Y, 0.0,
                                    X, Y, Z);
   end Ship_Centre;

   ------------------
   -- Ship_Outline --
   ------------------

   function Ship_Outline
     (Model : Root_Combat_Arena;
      Ship  : Ship_Record)
      return Lui.Rendering.Buffer_Points
   is
      use Concorde.Elementary_Functions;
      Raw : constant array (Positive range <>) of Point_Type :=
              ((1.0, 0.0), (-0.5, -1.0), (0.0, 0.0), (-0.5, 1.0));
      Result : Lui.Rendering.Buffer_Points (Raw'Range);
      C_X, C_Y : Integer;
      C_Z      : Real;
   begin
      Model.Get_Screen_Coordinates (Ship.X, Ship.Y, 0.0,
                                    C_X, C_Y, C_Z);
      for I in Raw'Range loop
         declare
            P : Point_Type := Raw (I);
         begin
            P.X := P.X * Cos (Ship.Facing) - P.Y * Sin (Ship.Facing);
            P.Y := P.X * Sin (Ship.Facing) + P.Y * Cos (Ship.Facing);
            P.X := P.X * Real (Ship.Ship.Size) / 2.0;
            P.Y := P.Y * Real (Ship.Ship.Size) / 2.0;
            Result (I) :=
              (X => C_X + Integer (P.X),
               Y => C_Y + Integer (P.Y));
         end;
      end loop;

      return Result;
   end Ship_Outline;

   ----------------
   -- Ship_Range --
   ----------------

   function Ship_Range
     (Model : Root_Combat_Arena;
      Index_1, Index_2 : Positive)
      return Non_Negative_Real
   is
      S1 : Ship_Record renames Model.Ships (Index_1);
      S2 : Ship_Record renames Model.Ships (Index_2);
   begin
      return Distance (S1.X, S1.Y, S2.X, S2.Y);
   end Ship_Range;

   ----------
   -- Tick --
   ----------

   procedure Tick (Arena : in out Root_Combat_Arena'Class) is
   begin
      Arena.Turns := Arena.Turns + 1;

      if not Arena.Done then
         for I in 1 .. Arena.Ships.Last_Index loop
            Arena.Update_Ship (Arena.Ships (I));
         end loop;
      end if;

      for Event of Arena.Events loop
         exit when Event.Turn < Arena.Turns;

         Arena.Commit_Event (Event);
      end loop;

   end Tick;

   ----------------------
   -- Total_Combatants --
   ----------------------

   function Total_Combatants
     (Arena : Root_Combat_Arena'Class)
      return Natural
   is
   begin
      return Arena.Ships.Last_Index;
   end Total_Combatants;

   -----------------
   -- Update_Ship --
   -----------------

   procedure Update_Ship
     (Model : in out Root_Combat_Arena;
      Ship  : in out Ship_Record)
   is
   begin
      if not Ship.Ship.Alive then
         return;
      end if;

      if Ship.Target = 0
        or else not Model.Ships (Ship.Target).Ship.Alive
      then
         Model.Choose_Target (Ship);
      end if;
      Ship.Ship.Update_Power;

      declare
         Ws : constant Concorde.Modules.Array_Of_Modules :=
                Ship.Ship.Get_Weapon_Modules;
      begin
         for W of Ws loop
            if Concorde.Random.Unit_Random < W.Charge then
               Model.Fire_Weapon (Ship, W);
            end if;
         end loop;
      end;
   end Update_Ship;

end Concorde.Combat.Ship_Combat;
