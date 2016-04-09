with Ada.Unchecked_Deallocation;

with WL.Random;

with Lui.Colours;

--  with Concorde.Dates;
with Concorde.Elementary_Functions;
with Concorde.Random;

with Concorde.Empires.Relations;

with Concorde.Modules.Db;
with Concorde.Ships.Db;

package body Concorde.Combat.Ship_Combat is

--     Log_File : Ada.Text_IO.File_Type;

   procedure Log
     (Arena : Root_Space_Combat_Arena'Class;
      Message : String);

   Max_Turns : constant := 1_000;

   function Distance (X1, Y1, X2, Y2 : Real) return Non_Negative_Real
   is (Concorde.Elementary_Functions.Sqrt
       ((X1 - X2) ** 2 + (Y1 - Y2) ** 2));

   -------------------
   -- Add_Combatant --
   -------------------

   procedure Add_Combatant
     (Arena     : in out Root_Space_Combat_Arena'Class;
      Combatant : Concorde.Ships.Ship_Type;
      Empire    : Concorde.Empires.Empire_Type;
      X, Y      : Real;
      Facing    : Radians)
   is
      use Concorde.Empires;
      use Concorde.Ships;
      New_Team : Boolean := True;
   begin
      Arena.Ships.Append
        ((Combatant, Arena.Ships.Last_Index + 1,
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
     (Model      : in out Root_Space_Combat_Arena;
      Ship       : in out Ship_Record;
      Target_All : Boolean)
   is
   begin
      for Team of Model.Teams loop
         if not Team.Ships.Is_Empty
           and then Concorde.Empires.Relations.At_War
             (Ship.Ship.Owner.all, Team.Leader.all)
         then
            declare
               Effective_Ships : array
                 (1 .. Team.Ships.Last_Index) of Positive;
               Count           : Natural := 0;
            begin
               for Ship_Index of Team.Ships loop
                  if Target_All
                    or else Model.Ships (Ship_Index).Ship.Has_Effective_Weapon
                  then
                     Count := Count + 1;
                     Effective_Ships (Count) := Ship_Index;
                  end if;
               end loop;

               if Count > 0 then
                  declare
                     Index : constant Positive :=
                               WL.Random.Random_Number (1, Count);
                  begin
                     Ship.Target := Effective_Ships (Index);
                     Log (Model,
                          Ship.Ship.Name & " targets "
                          & Model.Ships.Element
                            (Team.Ships (Index)).Ship.Name);
                     return;
                  end;
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

                     procedure Hit
                       (Ship : in out Concorde.Ships.Root_Ship_Type'Class);

                     ---------
                     -- Hit --
                     ---------

                     procedure Hit
                       (Ship : in out Concorde.Ships.Root_Ship_Type'Class)
                     is
                     begin
                        Ship.Hit (Damage);
                     end Hit;

                  begin
                     Log (Model,
                          Model.Ships (Event.Target).Ship.Name
                          & " takes" & Damage'Img & " damage");
                     Concorde.Ships.Db.Update
                       (Model.Ships (Event.Target).Ship.Reference,
                        Hit'Access);
                  end;

               when Concorde.Components.Kinetic_Weapon =>
                  null;

               when Concorde.Components.Launcher =>
                  null;
            end case;
      end case;
   end Commit_Event;

   ----------
   -- Done --
   ----------

   overriding function Done
     (Arena : in out Root_Space_Combat_Arena)
      return Boolean
   is
      Active_Team : Boolean := False;
      Winner      : Concorde.Empires.Empire_Type;
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
                  Winner := Team.Leader;
                  exit;
               end if;
            end if;
         end loop;
      end loop;

      Arena.Winner := Winner;
      return True;
   end Done;

   -------------
   -- Empires --
   -------------

   function Empires
     (Arena : Root_Space_Combat_Arena'Class)
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

   -----------------
   -- Fire_Weapon --
   -----------------

   procedure Fire_Weapon
     (Model  : in out Root_Space_Combat_Arena;
      Ship   : in out Ship_Record;
      Module : Concorde.Modules.Module_Type)
   is
      procedure Update_Module
        (Module : in out Concorde.Modules.Root_Module_Type'Class);

      -------------------
      -- Update_Module --
      -------------------

      procedure Update_Module
        (Module : in out Concorde.Modules.Root_Module_Type'Class)
      is
      begin
         Module.Execute;
      end Update_Module;

   begin
      Log (Model,
           Ship.Ship.Name & " fires " & Module.Name
           & " with charge "
           & Lui.Approximate_Image (Module.Stored_Energy)
           & "/"
           & Lui.Approximate_Image (Module.Maximum_Stored_Energy));

      Model.Events.Insert
        (Model.Events.First,
         (Weapon_Fired, Model.Turns, Ship.Index, Ship.Target,
          Module, Module.Component, Module.Stored_Energy,
          Module.Effectiveness, Model.Ship_Range (Ship.Index, Ship.Target)));

      Concorde.Modules.Db.Update
        (Module.Reference, Update_Module'Access);

   end Fire_Weapon;

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
                   Winner        => null,
                   Events        => List_Of_Combat_Events.Empty_List);
   begin
--        Ada.Text_IO.Create (Log_File, Ada.Text_IO.Out_File,
--                            Name & "-"
--                            & Concorde.Dates.Current_Date_To_String
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
      for Event of Model.Events loop
         case Event.Event is
            when Weapon_Fired =>
               if Model.Turns - Event.Turn < 1 then
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

      for Combat_Ship of Model.Ships loop
         declare
            use Concorde.Ships;
            Outline  : constant Lui.Rendering.Buffer_Points :=
                         Model.Ship_Outline (Combat_Ship);
            Colour   : Lui.Colours.Colour_Type :=
                         Combat_Ship.Ship.Owner.Colour;
            Pi       : constant := Ada.Numerics.Pi;
            X, Y     : Integer;
            Health_X : Integer;
            W        : constant Natural :=
                         Natural ((1.0 - Combat_Ship.Ship.Damage)
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
            if Combat_Ship.Facing in -Pi / 2.0 .. Pi / 2.0 then
               Health_X := X - Health_Bar_Width * 2;
            else
               Health_X := X + Health_Bar_Width;
            end if;
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
                           Renderer.Draw_Circle
                             (X          => X,
                              Y          => Y,
                              Radius     =>
                                (10 + Module.Explosion_Timer)
                              * Module.Explosion_Size / 5,
                              Colour     => (0.89, 0.34, 0.13, 0.7),
                              Filled     => True,
                              Line_Width => 1);
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
                     Colour => (0.0, 1.0, 1.0, 1.0),
                        Filled => True);
                  end;

                  Charge_Y := Charge_Y + Charge_Bar_Height * 3 / 2;
               end loop;
            end;
         end;
      end loop;

      if Model.Done then
         declare
            use type Concorde.Empires.Empire_Type;
         begin
            Renderer.Draw_String
              (10, 10, 16, Lui.Colours.White,
               (if Model.Winner /= null
                then "Victory to " & Model.Winner.Name
                else "The battle continues"));
         end;
      end if;

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
      Z : Real;
   begin
      Model.Get_Screen_Coordinates (Ship.X, Ship.Y, 0.0,
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
      return Distance (S1.X, S1.Y, S2.X, S2.Y);
   end Ship_Range;

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
                 and then Concorde.Random.Unit_Random < Charge * 2.0 - 1.0
               then
                  Model.Fire_Weapon (Ship, Weapon);
               end if;
            end;
         end loop;
      end;
   end Update_Ship;

end Concorde.Combat.Ship_Combat;
