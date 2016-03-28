private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

with Lui.Models;
private with Lui.Rendering;

with Concorde.Empires;
with Concorde.Modules;
with Concorde.Ships;

with Concorde.Components.Weapons;

package Concorde.Combat.Ship_Combat is

   type Root_Combat_Arena is
     new Lui.Models.Root_Object_Model with private;

   procedure Add_Combatant
     (Arena     : in out Root_Combat_Arena'Class;
      Combatant : not null access Concorde.Ships.Root_Ship_Type'Class;
      Team      : not null access Concorde.Empires.Root_Empire_Type'Class;
      X, Y      : Real;
      Facing    : Radians);

   procedure Tick (Arena : in out Root_Combat_Arena'Class);
   procedure Execute (Arena : in out Root_Combat_Arena'Class);

   function Done (Arena : Root_Combat_Arena'Class) return Boolean;

   function Empires
     (Arena : Root_Combat_Arena'Class)
      return Concorde.Empires.Array_Of_Empires;

   function Total_Combatants
     (Arena : Root_Combat_Arena'Class)
      return Natural;

   type Combat_Arena is access all Root_Combat_Arena'Class;

   function New_Arena
     (Name               : String;
      Radius             : Non_Negative_Real;
      Planet_X, Planet_Y : Real;
      Planet_Radius      : Non_Negative_Real)
      return Combat_Arena;

   procedure Close_Arena
     (Arena : in out Combat_Arena);

private

   type Ship_Record is
      record
         Ship     : Concorde.Ships.Ship_Type;
         Index    : Positive;
         X, Y     : Real;
         Facing   : Radians;
         Target   : Natural;
      end record;

   package Combat_Ship_Vectors is
     new Ada.Containers.Vectors (Positive, Ship_Record);

   package Ship_Index_Vectors is
     new Ada.Containers.Vectors (Positive, Positive);

   type Team_Record is
      record
         Leader : Concorde.Empires.Empire_Type;
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
               Weapon        : Concorde.Components.Weapons.Weapon_Component;
               Power         : Natural;
               Effectiveness : Unit_Real;
         end case;
      end record;

   package List_Of_Combat_Events is
     new Ada.Containers.Doubly_Linked_Lists (Combat_Event);

   type Root_Combat_Arena is
     new Lui.Models.Root_Object_Model with
      record
         Radius             : Non_Negative_Real;
         Planet_X, Planet_Y : Real;
         Planet_Radius      : Non_Negative_Real;
         Teams              : Team_Vectors.Vector;
         Ships              : Combat_Ship_Vectors.Vector;
         Turns              : Natural;
         Events             : List_Of_Combat_Events.List;
      end record;

   overriding procedure Idle_Update
     (Model    : in out Root_Combat_Arena;
      Updated  : out Boolean);

   overriding procedure Render
     (Model    : in out Root_Combat_Arena;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   overriding function Get_Drag_Behaviour
     (Model : Root_Combat_Arena)
      return Lui.Models.Drag_Behaviour
   is (Lui.Models.Translation);

   function Ship_Outline
     (Model : Root_Combat_Arena;
      Ship  : Ship_Record)
      return Lui.Rendering.Buffer_Points;

   procedure Ship_Centre
     (Model : Root_Combat_Arena'Class;
      Ship  : Ship_Record;
      X, Y  : out Integer);

   function Ship_Range
     (Model : Root_Combat_Arena;
      Index_1, Index_2 : Positive)
      return Non_Negative_Real;

   procedure Update_Ship
     (Model : in out Root_Combat_Arena;
      Ship  : in out Ship_Record);

   procedure Choose_Target
     (Model : in out Root_Combat_Arena;
      Ship  : in out Ship_Record);

   procedure Fire_Weapon
     (Model  : in out Root_Combat_Arena;
      Ship   : in out Ship_Record;
      Module : Concorde.Modules.Module_Type);

   procedure Commit_Event
     (Model  : in out Root_Combat_Arena;
      Event  : Combat_Event);

end Concorde.Combat.Ship_Combat;
