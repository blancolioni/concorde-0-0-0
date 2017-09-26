private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

private with Lui.Rendering;

with Concorde.Geometry;

with Concorde.Factions;
with Concorde.Modules;
with Concorde.Ships;

with Concorde.Components;

package Concorde.Combat.Ship_Combat is

   type Root_Space_Combat_Arena is
     new Root_Combat_Arena with private;

   procedure Add_Combatant
     (Arena     : in out Root_Space_Combat_Arena'Class;
      Combatant : Concorde.Ships.Ship_Type;
      Faction    : Concorde.Factions.Faction_Type;
      X, Y      : Real;
      Facing    : Concorde.Geometry.Radians);

   function Factions
     (Arena : Root_Space_Combat_Arena'Class)
      return Concorde.Factions.Array_Of_Factions;

   function Fleet_Size
     (Arena : Root_Space_Combat_Arena'Class;
      Faction : Concorde.Factions.Faction_Type)
      return Natural;

   function Total_Combatants
     (Arena : Root_Space_Combat_Arena'Class)
      return Natural;

   type Space_Combat_Arena is access all Root_Space_Combat_Arena'Class;

   function New_Arena
     (Name               : String;
      Radius             : Non_Negative_Real;
      Planet_X, Planet_Y : Real;
      Planet_Radius      : Non_Negative_Real)
      return Space_Combat_Arena;

   procedure Close_Arena
     (Arena : in out Space_Combat_Arena);

private

   type Ship_Record is
      record
         Ship      : Concorde.Ships.Ship_Type;
         Index     : Positive;
         Location  : Point_Type;
         Facing    : Concorde.Geometry.Radians;
         Target    : Natural;
         Hit       : Boolean;
         Shield_R1 : Natural;
         Shield_R2 : Natural;
      end record;

   package Combat_Ship_Vectors is
     new Ada.Containers.Vectors (Positive, Ship_Record);

   package Ship_Index_Vectors is
     new Ada.Containers.Vectors (Positive, Positive);

   type Team_Record is
      record
         Leader : Concorde.Factions.Faction_Type;
         Ships  : Ship_Index_Vectors.Vector;
      end record;

   package Team_Vectors is
     new Ada.Containers.Vectors (Positive, Team_Record);

   type Event_Type is (Weapon_Fired);

   type Combat_Event (Event : Event_Type := Weapon_Fired) is
      record
         Turn : Natural;
         case Event is
            when Weapon_Fired =>
               Attacker      : Positive;
               Target        : Positive;
               Module        : Concorde.Modules.Module_Type;
               Weapon        : Concorde.Components.Component_Type;
               Power         : Non_Negative_Real;
               Effectiveness : Unit_Real;
               Distance      : Non_Negative_Real;
         end case;
      end record;

   package List_Of_Combat_Events is
     new Ada.Containers.Doubly_Linked_Lists (Combat_Event);

   type Projectile_Type is (Beam, Kinetic, Missile);

   type Projectile_Record is
      record
         Projectile : Projectile_Type;
         Size       : Positive;
         Start      : Point_Type;
         Distance   : Non_Negative_Real;
         DX, DY     : Real;
         Velocity   : Unit_Real;
         Progress   : Unit_Real;
         Event      : List_Of_Combat_Events.Cursor;
         Active     : Boolean;
      end record;

   package List_Of_Projectiles is
     new Ada.Containers.Doubly_Linked_Lists (Projectile_Record);

   type Root_Space_Combat_Arena is
     new Root_Combat_Arena with
      record
         Radius             : Non_Negative_Real;
         Planet_X, Planet_Y : Real;
         Planet_Radius      : Non_Negative_Real;
         Teams              : Team_Vectors.Vector;
         Ships              : Combat_Ship_Vectors.Vector;
         Turns              : Natural;
         Events             : List_Of_Combat_Events.List;
         Projectiles        : List_Of_Projectiles.List;
      end record;

   overriding procedure Tick
     (Arena : in out Root_Space_Combat_Arena);

   overriding function Handle_Update
     (Model    : in out Root_Space_Combat_Arena)
      return Boolean;

   overriding procedure Render
     (Model    : in out Root_Space_Combat_Arena;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   overriding procedure Select_XY
     (Model : in out Root_Space_Combat_Arena;
      X, Y  : Natural);

   procedure Check_Finished
     (Arena : in out Root_Space_Combat_Arena'Class);

   function Ship_Outline
     (Model : Root_Space_Combat_Arena;
      Ship  : Ship_Record)
      return Lui.Rendering.Buffer_Points;

   procedure Ship_Centre
     (Model : Root_Space_Combat_Arena'Class;
      Ship  : Ship_Record;
      X, Y  : out Integer);

   function Ship_Range
     (Model : Root_Space_Combat_Arena;
      Index_1, Index_2 : Positive)
      return Non_Negative_Real;

   procedure Update_Ship
     (Model : in out Root_Space_Combat_Arena;
      Ship  : in out Ship_Record);

   procedure Choose_Target
     (Model      : in out Root_Space_Combat_Arena;
      Ship       : in out Ship_Record;
      Target_All : Boolean);

   procedure Fire_Weapon
     (Model  : in out Root_Space_Combat_Arena;
      Ship   : in out Ship_Record;
      Module : Concorde.Modules.Module_Type);

   procedure Start_Event
     (Model    : in out Root_Space_Combat_Arena;
      Position : List_Of_Combat_Events.Cursor);

   procedure Commit_Event
     (Model  : in out Root_Space_Combat_Arena;
      Event  : Combat_Event);

end Concorde.Combat.Ship_Combat;
