with Concorde.Objects;

package Concorde.Laws is

   type Law_Level is range 0 .. 4;
   --  level 0: laws of the game
   --  level 1: constitutional
   --  level 2: legislation
   --  level 3: regulation
   --  level 4: customary

   type Law_Context is private;

   type Root_Law_Type is abstract tagged private;

   type Law_Type is access all Root_Law_Type'Class;

   procedure Enact
     (Law : in out Root_Law_Type)
   is abstract;

   procedure Repeal
     (Law : in out Root_Law_Type)
   is abstract;

   function Show (Law : Root_Law_Type) return String is abstract;

private

   type Law_Context is
      record
         Legislator : Concorde.Objects.Object_Type;
         Target     : Concorde.Objects.Object_Type;
      end record;

   type Root_Law_Type is abstract tagged
      record
         Level    : Law_Level;
         Context  : Law_Context;
      end record;

end Concorde.Laws;
