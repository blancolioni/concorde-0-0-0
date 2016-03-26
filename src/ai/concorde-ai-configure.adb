with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Fixed.Hash_Case_Insensitive;
with Ada.Containers.Indefinite_Hashed_Maps;

with Concorde.AI.Default;

package body Concorde.AI.Configure is

   package AI_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => AI_Constructor,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   AI_Registry : AI_Maps.Map;

   ------------
   -- Get_AI --
   ------------

   function Get_AI (Name : String) return AI_Type is
   begin
      return AI_Registry.Element (Name).all;
   end Get_AI;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Register ("default", Concorde.AI.Default.Default_AI'Access);
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name        : String;
      Constructor : AI_Constructor)
   is
   begin
      AI_Registry.Insert (Name, Constructor);
   end Register;

end Concorde.AI.Configure;
