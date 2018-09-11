with Concorde.Objects;

package Concorde.Laws is

   type Law_Level is
     (
      Axiom,                 --  rule of the game, unchangeable
      Constitutional,        --  difficult to change
      Legislation,           --  changed by repeal
      Regulation,            --  changed by executive order
      Custom                 --  customary behaviour only; no force of law
     );

   type Law_Target_Interface is limited interface;

   type Law_Context is private;

   type Root_Law_Type is abstract tagged private;

   type Law_Type is access all Root_Law_Type'Class;

   type Array_Of_Laws is array (Positive range <>) of Law_Type;

   function Can_Enact
     (Law : Root_Law_Type)
      return Boolean
      is abstract;

   procedure Enact
     (Law : in out Root_Law_Type)
   is abstract
     with Pre'Class => Law.Can_Enact;

   procedure Repeal
     (Law : in out Root_Law_Type)
   is abstract
     with Pre'Class => Law.Can_Enact;

   function Show (Law : Root_Law_Type) return String is abstract;

private

   type Law_Context is
      record
         Legislator : Concorde.Objects.Object_Type;
         Target     : access constant Law_Target_Interface'Class;
      end record;

   type Root_Law_Type is abstract tagged
      record
         Level    : Law_Level;
         Context  : Law_Context;
      end record;

end Concorde.Laws;
