with Ada.Unchecked_Deallocation;

with Lui.Colors;

with Concorde.Real_Images;

--  with Concorde.Calendar;
with Concorde.Elementary_Functions;

with Concorde.Random;

with Concorde.Factions.Logging;
with Concorde.Factions.Relations;

package body Concorde.Combat.Ship_Combat is

--     Log_File : Ada.Text_IO.File_Type;

   procedure Log
     (Arena : Root_Space_Combat_Arena'Class;
      Message : String);

   Max_Turns : constant := 1_000;

   function Distance (X1, Y1, X2, Y2 : Real) return Non_Negative_Real
   is (Concorde.Elementary_Functions.Sqrt
       ((X1 - X2) ** 2 + (Y1 - Y2) ** 2));

   function Distance (P1, P2 : Point_Type) return Non_Negative_Real
   is (Distance (P1.X, P1.Y, P2.X, P2.Y));

   function Make_Combat_Ship
     (Ship   : Concorde.Ships.Ship_Type;
      Index  : Positive;
      X, Y   : Real;
      Facing : Concorde.Geometry.Radians)
      return Ship_Record
   is ((Ship      => Ship,
        Index     => Index,
        Location  => (X, Y),
        Facing    => Facing,
        Target    => 0,
        Hit       => False,
        Shield_R1 => 0,
        Shield_R2 => 0));

   -------------------
   -- Add_Combatant --
   -------------------

   procedure Add_Combatant
     (Arena     : in out Root_Space_Combat_Arena'Class;
      Combatant : Concorde.Ships.Ship_Type;
      Faction    : Concorde.Factions.Faction_Type;
      X, Y      : Real;
      Facing    : Concorde.Geometry.Radians)
   is
      use Concorde.Factions;
      use Concorde.Ships;
      New_Team : Boolean := True;
   begin
      Arena.Ships.Append
        (Make_Combat_Ship
           (Ship   => Combatant,
            Index  => Arena.Ships.Last_Index + 1,
            X      => X,
            Y      => Y,
            Facing => Facing));

      for I in 1 .. Arena.Teams.Last_Index loop
         if Arena.Teams (I).Leader = Faction then
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
            Arena.Teams.Append ((Faction, Ss));
         end;
      end if;

   end Add_Combatant;

   --------------------
   -- Check_Finished --
   --------------------

   procedure Check_Finished
     (Arena : in out Root_Space_Combat_Arena'Class)
   is
      Active_Team : Boolean := False;
      Winner      : Concorde.Factions.Faction_Type;
   begin
      if Arena.Finished then
         return;
      end if;

      if Arena.Turns >= Max_Turns then
         Arena.Finished := True;
         Arena.Winner := null;
         for Team of Arena.Teams loop
            Concorde.Factions.Logging.Log
              (Team.Leader,
               "The battle continues");
         end loop;

         return;
      end if;

      for Projectile of Arena.Projectiles loop
         if Projectile.Active then
            Arena.Finished := False;
            return;
         end if;
      end loop;

      for Team of Arena.Teams loop
         for Ship_Index of Team.Ships loop
            declare
               use Concorde.Ships;
               Ship           : constant Concorde.Ships.Ship_Type :=
                                  Arena.Ships (Ship_Index).Ship;
               Damaged_Mounts : constant Array_Of_Mounted_Modules :=
                                  Ship.Get_Damaged_Mounts;
            begin
               for Mount of Damaged_Mounts loop
                  declare
                     Module : constant Concorde.Modules.Module_Type :=
                                Ship.Get_Module (Mount);
                  begin
                     if Module.Exploding then
                        if Module.Explosion_Timer > -10 then
                           Arena.Finished := False;
                           return;
                        end if;
                     end if;
                  end;
               end loop;
            end;
         end loop;
      end loop;

      for Team of Arena.Teams loop
         for Ship_Index of Team.Ships loop
            if Arena.Ships (Ship_Index).Ship.Alive
              and then Arena.Ships (Ship_Index).Ship.Has_Effective_Weapon
            then
               if Active_Team then
                  Arena.Finished := False;
                  return;
               else
                  Active_Team := True;
                  Winner := Team.Leader;
                  exit;
               end if;
            end if;
         end loop;
      end loop;

      Arena.Finished := True;
      Arena.Winner := Winner;

      if not Active_Team then
         for Team of Arena.Teams loop
            Concorde.Factions.Logging.Log
              (Team.Leader,
               "Mutual annihilation");
         end loop;
         return;
      end if;

      for Team of Arena.Teams loop
         Concorde.Factions.Logging.Log
           (Team.Leader,
            "Victory to " & Arena.Winner.Name);
      end loop;

      declare
         use type Concorde.Factions.Faction_Type;

         Captured_Ships : Natural := 0;

      begin
         if Winner /= null then
            for Team of Arena.Teams loop
               if Team.Leader /= Winner then
                  Captured_Ships := 0;
                  for Ship_Index of Team.Ships loop
                     declare
                        use Concorde.Ships;
                        Ship : constant Vessel_Type :=
                                 Arena.Ships (Ship_Index).Ship;
                     begin
                        if Ship.Alive
                          and then not Ship.Has_Effective_Engine
                        then
                           Concorde.Factions.Logging.Log
                             (Ship.Owner,
                              Ship.Short_Description
                              & " captured by " & Winner.Name);
                           Concorde.Factions.Logging.Log
                             (Winner,
                              Ship.Short_Description & " captured from "
                              & Ship.Owner.Name);
                           Ship.Update.Set_Owner (Winner);
                           Captured_Ships := Captured_Ships + 1;
                        end if;
                     end;
                  end loop;
                  if Captured_Ships > 0 then
                     Winner.Update.Change_Ships (Captured_Ships);
                     Team.Leader.Update.Change_Ships (-Captured_Ships);
                  end if;
               end if;
            end loop;
         end if;
      end;

   end Check_Finished;

   -------------------
   -- Choose_Target --
   -------------------

   procedure Choose_Target
     (Model      : in out Root_Space_Combat_Arena;
      Ship       : in out Ship_Record;
      Target_All : Boolean)
   is
   begin
      for Team of Model.Teams loop
         if not Team.Ships.Is_Empty
           and then Concorde.Factions.Relations.At_War
             (Ship.Ship.Owner.all, Team.Leader.all)
         then
            declare
               use Concorde.Geometry;
               Best_Angle  : Radians := Degrees_To_Radians (0.0);
               Best_Target : Natural := 0;
            begin
               for Ship_Index of Team.Ships loop
                  declare
                     use Concorde.Ships;
                     Target : Ship_Record renames
                                Model.Ships (Ship_Index);
                  begin
                     if Target.Ship.Alive
                       and then (Target_All
                                 or else Target.Ship.Has_Effective_Weapon)
                     then
                        declare
                           Bearing : constant Radians :=
                                       Concorde.Geometry.Angular_Distance
                                         (Ship.Location, Ship.Facing,
                                          Target.Location);
                        begin
                           if Best_Target = 0
                             or else Bearing < Best_Angle
                           then
                              Best_Angle := Bearing;
                              Best_Target := Ship_Index;
                           end if;
                        end;
                     end if;
                  end;
               end loop;

               if Best_Target /= 0 then
                  Ship.Target := Best_Target;
                  Log (Model,
                       Ship.Ship.Name & " targets "
                       & Model.Ships.Element (Best_Target).Ship.Name);
                  return;
               end if;
            end;
         end if;
      end loop;
      Ship.Target := 0;
   end Choose_Target;

   -----------------
   -- Close_Arena --
   -----------------

   procedure Close_Arena
     (Arena : in out Space_Combat_Arena)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Root_Space_Combat_Arena'Class, Space_Combat_Arena);
   begin
      Free (Arena);
--        Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
--        Ada.Text_IO.Close (Log_File);
   end Close_Arena;

   ------------------
   -- Commit_Event --
   ------------------

   procedure Commit_Event
     (Model  : in out Root_Space_Combat_Arena;
      Event  : Combat_Event)
   is
   begin
      case Event.Event is
         when Weapon_Fired =>
            case Concorde.Components.Weapon_Class (Event.Weapon.Class) is
               when Concorde.Components.Energy_Weapon =>
                  declare
                     Damage : constant Natural :=
                                Event.Weapon.Effective_Damage
                                  (Power         => Event.Power,
                                   Effectiveness => Event.Effectiveness,
                                   At_Range      => Event.Distance);

                  begin
                     Log (Model,
                          Model.Ships (Event.Target).Ship.Name
                          & " takes" & Damage'Img & " damage");
                     Model.Ships.Element (Event.Target)
                       .Ship.Update.Hit (Damage);
                     Model.Ships (Event.Target).Hit := True;
                  end;

               when Concorde.Components.Kinetic_Weapon =>
                  null;

               when Concorde.Components.Launcher =>
                  null;
            end case;
      end case;
   end Commit_Event;

   -------------
   -- Factions --
   -------------

   function Factions
     (Arena : Root_Space_Combat_Arena'Class)
      return Concorde.Factions.Array_Of_Factions
   is
      use Concorde.Factions;
      Result : Array_Of_Factions (1 .. Arena.Teams.Last_Index);
   begin
      for I in Result'Range loop
         Result (I) := Arena.Teams (I).Leader;
      end loop;
      return Result;
   end Factions;

   -----------------
   -- Fire_Weapon --
   -----------------

   procedure Fire_Weapon
     (Model  : in out Root_Space_Combat_Arena;
      Ship   : in out Ship_Record;
      Module : Concorde.Modules.Module_Type)
   is
   begin
      Log (Model,
           Ship.Ship.Name & " fires " & Module.Name
           & " with charge "
           & Concorde.Real_Images.Approximate_Image (Module.Stored_Energy)
           & "/"
           & Concorde.Real_Images.Approximate_Image
             (Module.Maximum_Stored_Energy));

      Model.Events.Insert
        (Model.Events.First,
         (Weapon_Fired, Model.Turns, Ship.Index, Ship.Target,
          Module, Module.Component, Module.Stored_Energy,
          Module.Effectiveness, Model.Ship_Range (Ship.Index, Ship.Target)));

      Module.Update.Execute;

   end Fire_Weapon;

   ----------------
   -- Fleet_Size --
   ----------------

   function Fleet_Size
     (Arena  : Root_Space_Combat_Arena'Class;
      Faction : Concorde.Factions.Faction_Type)
      return Natural
   is
      use type Concorde.Factions.Faction_Type;
   begin
      for Team of Arena.Teams loop
         if Team.Leader = Faction then
            return Team.Ships.Last_Index;
         end if;
      end loop;
      return 0;
   end Fleet_Size;

   -------------------
   -- Handle_Update --
   -------------------

   overriding function Handle_Update
     (Model    : in out Root_Space_Combat_Arena)
      return Boolean
   is
   begin
      Model.Tick;
      return True;
   end Handle_Update;

   ---------
   -- Log --
   ---------

   procedure Log
     (Arena : Root_Space_Combat_Arena'Class;
      Message : String)
   is
   begin
      null;
--        Ada.Text_IO.Put_Line
--          (Log_File,
--           Arena.Turns'Img & ": " & Message);
   end Log;

   ---------------
   -- New_Arena --
   ---------------

   function New_Arena
     (Name               : String;
      Radius             : Non_Negative_Real;
      Planet_X, Planet_Y : Real;
      Planet_Radius      : Non_Negative_Real)
      return Space_Combat_Arena
   is
      Arena : constant Space_Combat_Arena :=
                new Root_Space_Combat_Arena'
                  (Lui.Models.Root_Object_Model with
                   Radius        => Radius,
                   Planet_X      => Planet_X,
                   Planet_Y      => Planet_Y,
                   Planet_Radius => Planet_Radius,
                   Teams         => Team_Vectors.Empty_Vector,
                   Ships         => Combat_Ship_Vectors.Empty_Vector,
                   Turns         => 0,
                   Finished      => False,
                   Winner        => null,
                   Events        => List_Of_Combat_Events.Empty_List,
                   Projectiles   => List_Of_Projectiles.Empty_List);

   begin
--        Ada.Text_IO.Create (Log_File, Ada.Text_IO.Out_File,
--                            Name & "-"
--                            & Concorde.Calendar.Clock_To_String
--                            & ".txt");
--        Ada.Text_IO.Set_Output (Log_File);

      Arena.Initialise (Name);
      Arena.Set_Eye_Position (0.0, 0.0, 4000.0);
      return Arena;
   end New_Arena;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Root_Space_Combat_Arena;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      Health_Bar_Width  : constant := 20;
      Health_Bar_Height : constant := 8;
      Charge_Bar_Width  : constant := 20;
      Charge_Bar_Height : constant := 4;
   begin
      for Position in Model.Projectiles.Iterate loop
         if List_Of_Projectiles.Element (Position).Active then
            declare
               Projectile : Projectile_Record :=
                              List_Of_Projectiles.Element (Position);
               Progress : constant Unit_Real :=
                            Real'Min (Projectile.Progress
                                      + Projectile.Velocity,
                                      1.0);
               X          : constant Real :=
                            Projectile.Start.X + Projectile.DX * Progress;
               Y        : constant Real :=
                            Projectile.Start.Y + Projectile.DY * Progress;
            begin
               case Projectile.Projectile is
                  when Beam =>
                     declare
                        Beam_DX : constant Real :=
                                    Real (Projectile.Size)
                                    * Projectile.DX / Projectile.Distance;
                        Beam_DY : constant Real :=
                                    Real (Projectile.Size)
                                    * Projectile.DY / Projectile.Distance;
                        Beam_X1, Beam_Y1 : Integer;
                        Beam_X2, Beam_Y2 : Integer;
                        Beam_Z           : Lui.Real;
                     begin
                        Model.Get_Screen_Coordinates
                          (Lui.Real (X - Beam_DX), Lui.Real (Y - Beam_DY), 0.0,
                           Beam_X1, Beam_Y1, Beam_Z);
                        Model.Get_Screen_Coordinates
                          (Lui.Real (X), Lui.Real (Y), 0.0,
                           Beam_X2, Beam_Y2, Beam_Z);
                        Renderer.Draw_Line
                          (X1         => Beam_X1,
                           Y1         => Beam_Y1,
                           X2         => Beam_X2,
                           Y2         => Beam_Y2,
                           Color     => (0.2, 0.9, 0.15, 1.0),
                           Line_Width => 1);
                     end;
                  when Kinetic =>
                     null;
                  when Missile =>
                     null;
               end case;

               if Progress = 1.0 then
                  Model.Commit_Event
                    (List_Of_Combat_Events.Element (Projectile.Event));
                  Projectile.Active := False;
               end if;

               Projectile.Progress := Progress;

               Model.Projectiles.Replace_Element
                 (Position, Projectile);
            end;
         end if;
      end loop;

      for Combat_Ship of Model.Ships loop
         declare
            use Concorde.Ships;
            Outline  : constant Lui.Rendering.Buffer_Points :=
                         Model.Ship_Outline (Combat_Ship);
            Color   : Lui.Colors.Color_Type :=
                         Combat_Ship.Ship.Owner.Color;
            X, Y     : Integer;
            Health_X : Integer;
            W        : constant Natural :=
                         Natural ((1.0 - Combat_Ship.Ship.Damage)
                                  * Real (Health_Bar_Width));
            Shields  : constant Unit_Real := Combat_Ship.Ship.Shields;
            Shield_R1 : Natural := 0;
            Shield_R2 : Natural := 0;
         begin
            if not Combat_Ship.Ship.Alive then
               Color.Alpha := 0.3;
            end if;
            Renderer.Draw_Polygon
              (Vertices => Outline,
               Color   => Color,
               Filled   => True);

            Model.Ship_Centre (Combat_Ship, X, Y);

            if Shields > 0.0 then
               for P of Outline loop
                  Shield_R1 :=
                    Natural'Max
                      (abs (P.X - X),
                       Shield_R1);
                  Shield_R2 :=
                    Natural'Max
                      (abs (P.Y - Y),
                       Shield_R2);
               end loop;

               declare
                  Base   : constant Unit_Real := 0.1 + Shields * 0.8;
                  Color : Lui.Colors.Color_Type :=
                             (Lui.Unit_Real (Base),
                              Lui.Unit_Real (Base),
                              Lui.Unit_Real (Base),
                              1.0);
               begin
                  if Combat_Ship.Hit then
                     Color := (1.0, 1.0, 1.0, 1.0);
                  end if;
                  Renderer.Draw_Ellipse
                    (X          => X,
                     Y          => Y,
                     R1         => Shield_R1 + 2,
                     R2         => Shield_R2 + 2,
                     Color     => Color,
                     Filled     => False,
                     Line_Width => 1);
               end;
            end if;

            declare
               use Concorde.Geometry;
               East_Facing_Arc : constant Arc_Type :=
                                   Degree_Arc (-90.0, 90.0);
            begin
               if Contains (East_Facing_Arc, Combat_Ship.Facing) then
                  Health_X := X - Health_Bar_Width * 2;
               else
                  Health_X := X + Health_Bar_Width;
               end if;
            end;

            Renderer.Draw_Rectangle
              (Health_X, Y - Health_Bar_Height / 2,
               W, Health_Bar_Height,
               (0.0, 0.6, 0.0, 1.0), True);
            Renderer.Draw_Rectangle
              (Health_X + W, Y - Health_Bar_Height / 2,
               Health_Bar_Width - W, Health_Bar_Height,
               (0.7, 0.0, 0.0, 1.0), True);

            declare
               Damaged_Mounts : constant Array_Of_Mounted_Modules :=
                                  Combat_Ship.Ship.Get_Damaged_Mounts;
            begin
               for Mount of Damaged_Mounts loop
                  declare
                     Module : constant Concorde.Modules.Module_Type :=
                                Combat_Ship.Ship.Get_Module (Mount);
                  begin
                     if Module.Exploding then
                        if Module.Explosion_Timer in -9 .. 0 then
                           declare
                              Radius : constant Natural :=
                                         (8 - abs (Module.Explosion_Timer + 5))
                                         * Module.Explosion_Size / 10;
                              Alpha  : constant Unit_Real :=
                                         Real (10 + Module.Explosion_Timer)
                                         / 10.0 * 0.5 + 0.2;
                           begin
                              Renderer.Draw_Circle
                                (X          => X,
                                 Y          => Y,
                                 Radius     => Radius,
                                 Color     => (0.89, 0.34, 0.13,
                                                Lui.Unit_Real (Alpha)),
                                 Filled     => True,
                                 Line_Width => 1);
                           end;
                        end if;
                     end if;
                  end;
               end loop;
            end;
            declare
               Weapon_Mounts : constant Array_Of_Mounted_Modules :=
                                 Combat_Ship.Ship.Get_Weapon_Mounts;
               Charge_X      : constant Integer :=
                                 (if Health_X < X
                                  then Health_X - Charge_Bar_Width - 8
                                  else Health_X + Health_Bar_Width + 8);

               Charge_Y      : Integer :=
                                 Y -
                                   (Charge_Bar_Height
                                    * 3 * Weapon_Mounts'Length / 4);
            begin
               for Mount of Weapon_Mounts loop
                  declare
                     Module : constant Concorde.Modules.Module_Type :=
                                Combat_Ship.Ship.Get_Module (Mount);
                     Charge : constant Unit_Real :=
                                Module.Charge * Module.Effectiveness;
                     Width  : constant Natural :=
                                Natural (Real (Charge_Bar_Width) * Charge);
                  begin
                     Renderer.Draw_Rectangle
                       (Charge_X, Charge_Y,
                        Width, Charge_Bar_Height,
                     Color => (0.0, 1.0, 1.0, 1.0),
                        Filled => True);
                  end;

                  Charge_Y := Charge_Y + Charge_Bar_Height * 3 / 2;
               end loop;
            end;
         end;
      end loop;

      if Model.Done then
         declare
            use type Concorde.Factions.Faction_Type;
         begin
            Renderer.Draw_String
              (10, 10, 16, Lui.Colors.White,
               (if Model.Winner /= null
                then "Victory to " & Model.Winner.Name
                else "The battle continues"));
         end;
      end if;

      for I in 1 .. Model.Ships.Last_Index loop
         Model.Ships (I).Hit := False;
      end loop;

   end Render;

   ---------------
   -- Select_XY --
   ---------------

   overriding procedure Select_XY
     (Model : in out Root_Space_Combat_Arena;
      X, Y  : Natural)
   is
   begin
      if X in Model.Width - 32 .. Model.Width
        and then Y in 1 .. 32
      then
         Model.Parent_Model.Remove_Inline_Model (Model'Access);
      end if;
   end Select_XY;

   -----------------
   -- Ship_Centre --
   -----------------

   procedure Ship_Centre
     (Model : Root_Space_Combat_Arena'Class;
      Ship  : Ship_Record;
      X, Y  : out Integer)
   is
      S_X : constant Lui.Real := Lui.Real (Ship.Location.X);
      S_Y : constant Lui.Real := Lui.Real (Ship.Location.Y);
      Z : Lui.Real;
   begin
      Model.Get_Screen_Coordinates
        (S_X, S_Y, 0.0,
         X, Y, Z);
   end Ship_Centre;

   ------------------
   -- Ship_Outline --
   ------------------

   function Ship_Outline
     (Model : Root_Space_Combat_Arena;
      Ship  : Ship_Record)
      return Lui.Rendering.Buffer_Points
   is
      use Concorde.Geometry;
      Raw : constant array (Positive range <>) of Point_Type :=
              ((1.0, 0.0), (-0.5, -1.0), (0.0, 0.0), (-0.5, 1.0));
      Result : Lui.Rendering.Buffer_Points (Raw'Range);
      S_X      : constant Lui.Real := Lui.Real (Ship.Location.X);
      S_Y      : constant Lui.Real := Lui.Real (Ship.Location.Y);
      C_X, C_Y : Integer;
      C_Z      : Lui.Real;
   begin
      Model.Get_Screen_Coordinates (S_X, S_Y, 0.0,
                                    C_X, C_Y, C_Z);
      for I in Raw'Range loop
         declare
            P : Point_Type := Raw (I);
         begin
            P.X := P.X * Cos (Ship.Facing) - P.Y * Sin (Ship.Facing);
            P.Y := P.X * Sin (Ship.Facing) + P.Y * Cos (Ship.Facing);
            P.X := P.X * Real (Ship.Ship.Size.X) * 1.0;
            P.Y := P.Y * Real (Ship.Ship.Size.Z) * 1.0;
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
     (Model : Root_Space_Combat_Arena;
      Index_1, Index_2 : Positive)
      return Non_Negative_Real
   is
      S1 : Ship_Record renames Model.Ships (Index_1);
      S2 : Ship_Record renames Model.Ships (Index_2);
   begin
      return Distance
        (S1.Location.X, S1.Location.Y, S2.Location.X, S2.Location.Y);
   end Ship_Range;

   -----------------
   -- Start_Event --
   -----------------

   procedure Start_Event
     (Model    : in out Root_Space_Combat_Arena;
      Position : List_Of_Combat_Events.Cursor)
   is
      Event : constant Combat_Event :=
                List_Of_Combat_Events.Element (Position);
   begin
      case Event.Event is
         when Weapon_Fired =>
            declare
               use Concorde.Elementary_Functions;
               Ship : constant Ship_Record :=
                        Model.Ships (Event.Attacker);
               Target : constant Ship_Record :=
                          Model.Ships (Event.Target);
               D_D    : constant Real :=
                          (if Target.Ship.Shields > 0.0
                           then 0.9
                           else 1.0);
               DX     : constant Real := Target.Location.X - Ship.Location.X;
               DY     : constant Real := Target.Location.Y - Ship.Location.Y;
               Projectile : constant Projectile_Record :=
                              (Projectile => Beam,
                               Size       => 100,
                               Start => Ship.Location,
                               Distance   =>
                                 Distance (Ship.Location, Target.Location),
                               DX         => DX * D_D,
                               DY         => DY * D_D,
                               Velocity   => 0.1,
                               Progress   => 0.0,
                               Event      => Position,
                               Active     => True);
               Placed     : Boolean;
            begin
               for Pos in Model.Projectiles.Iterate loop
                  if not List_Of_Projectiles.Element (Pos).Active then
                     Model.Projectiles.Replace_Element (Pos, Projectile);
                     Placed := True;
                     exit;
                  end if;
               end loop;

               if not Placed then
                  Model.Projectiles.Append (Projectile);
               end if;
            end;
      end case;
   end Start_Event;

   ----------
   -- Tick --
   ----------

   overriding procedure Tick (Arena : in out Root_Space_Combat_Arena) is
   begin
      Arena.Turns := Arena.Turns + 1;

--        Ada.Text_IO.New_Line;
--        Ada.Text_IO.Put_Line
--          ("--  Turn" & Arena.Turns'Img);
--        Ada.Text_IO.New_Line;

      if not Arena.Done then
         for I in 1 .. Arena.Ships.Last_Index loop
            declare
               Ship : Ship_Record renames Arena.Ships (I);
            begin
               Arena.Update_Ship (Ship);
            end;
         end loop;
      end if;

      for Event_Position in Arena.Events.Iterate loop
         exit when List_Of_Combat_Events.Element (Event_Position).Turn
           < Arena.Turns;

         Arena.Start_Event (Event_Position);
      end loop;

      Arena.Check_Finished;

   end Tick;

   ----------------------
   -- Total_Combatants --
   ----------------------

   function Total_Combatants
     (Arena : Root_Space_Combat_Arena'Class)
      return Natural
   is
   begin
      return Arena.Ships.Last_Index;
   end Total_Combatants;

   -----------------
   -- Update_Ship --
   -----------------

   procedure Update_Ship
     (Model : in out Root_Space_Combat_Arena;
      Ship  : in out Ship_Record)
   is
      use Concorde.Geometry;
   begin

      Ship.Ship.Update_Damage;

      if not Ship.Ship.Alive then
         return;
      end if;

      if Ship.Target = 0
        or else not Model.Ships (Ship.Target).Ship.Alive
      then
         Model.Choose_Target (Ship, Target_All => False);
         if Ship.Target = 0 then
            Model.Choose_Target (Ship, Target_All => True);
         end if;
      elsif not Model.Ships (Ship.Target).Ship.Has_Effective_Weapon then
         declare
            Current_Target : constant Positive := Ship.Target;
         begin
            Model.Choose_Target (Ship, Target_All => False);
            if Ship.Target = 0 then
               Ship.Target := Current_Target;
            end if;
         end;
      end if;

      Ship.Ship.Update_Power;

      if Ship.Target /= 0 then
         if Angular_Distance (Ship.Location, Ship.Facing,
                              Model.Ships (Ship.Target).Location)
           < Degrees_To_Radians (1.0)
         then
            declare
               Ws : constant Concorde.Ships.Array_Of_Mounted_Modules :=
                      Ship.Ship.Get_Weapon_Mounts;
            begin
               for W of Ws loop
                  declare
                     Weapon : constant Concorde.Modules.Module_Type :=
                                Ship.Ship.Get_Module (W);
                     Charge : constant Unit_Real := Weapon.Charge;
                  begin
                     if Weapon.Effectiveness > 0.5
                       and then Charge > 0.5
                       and then Concorde.Random.Unit_Random
                         < Charge * 2.0 - 1.0
                     then
                        Model.Fire_Weapon (Ship, Weapon);
                     end if;
                  end;
               end loop;
            end;
         else
            declare
               Target_Location : constant Point_Type :=
                                   Model.Ships (Ship.Target).Location;
               Target_Heading  : constant Radians :=
                                   Bearing (Ship.Location, Target_Location);
               Current_Heading : constant Radians := Ship.Facing;
               Max_Turn        : constant Radians :=
                                   Degrees_To_Radians (Ship.Ship.Turn);
            begin
               if Target_Heading - Current_Heading < Max_Turn then
                  Ship.Facing := Target_Heading;
               else
                  Ship.Facing := Turn (Ship.Facing, Max_Turn, Target_Heading);
               end if;
            end;
         end if;
      end if;
   end Update_Ship;

end Concorde.Combat.Ship_Combat;
