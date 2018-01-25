with WL.String_Maps;

package body Concorde.People.Abilities is

   package Ability_Maps is
     new WL.String_Maps (Ability_Type);

   Map : Ability_Maps.Map;

   procedure Check_Map;

   ---------------
   -- Check_Map --
   ---------------

   procedure Check_Map is
   begin
      if Map.Is_Empty then
         for A in Ability_Type loop
            Map.Insert (Ability_Type'Image (A), A);
         end loop;
      end if;
   end Check_Map;

   ------------
   -- Exists --
   ------------

   function Exists
     (S : String)
      return Boolean
   is
   begin
      Check_Map;
      return Map.Contains (S);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (S : String)
      return Ability_Type
   is
   begin
      Check_Map;
      return Map.Element (S);
   end Get;

end Concorde.People.Abilities;
