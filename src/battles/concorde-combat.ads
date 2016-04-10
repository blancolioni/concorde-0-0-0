with Lui.Models;

with Concorde.Empires;

package Concorde.Combat is

   type Root_Combat_Arena is
     abstract new Lui.Models.Root_Object_Model with private;

   procedure Execute (Arena : in out Root_Combat_Arena'Class);

   procedure Tick (Arena : in out Root_Combat_Arena)
   is abstract;

   function Done (Arena : in out Root_Combat_Arena'Class) return Boolean;

   function Winner
     (Arena : Root_Combat_Arena)
      return Concorde.Empires.Empire_Type;

private

   type Root_Combat_Arena is
     abstract new Lui.Models.Root_Object_Model with
      record
         Finished : Boolean := False;
         Winner   : Concorde.Empires.Empire_Type;
      end record;

end Concorde.Combat;
