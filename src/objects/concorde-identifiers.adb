with Ada.Characters.Handling;

with WL.Random;
with WL.String_Maps;

package body Concorde.Identifiers is

   package Identifier_Sets is
     new WL.String_Maps (Boolean);

   Used : Identifier_Sets.Map;
   Template : constant String := "0AA0-A00A";

   --------------------
   -- New_Identifier --
   --------------------

   function New_Identifier return String is
      use WL.Random;
   begin
      loop
         declare
            Id : String := Template;
         begin
            for Ch of Id loop
               if Ch in '0' .. '9' then
                  Ch := Character'Val (Random_Number (48, 57));
               elsif Ch in 'A' .. 'Z' then
                  Ch :=
                    Character'Val
                      (Random_Number
                         (Character'Pos ('A'), Character'Pos ('Z')));
               elsif Ch in 'a' .. 'z' then
                  Ch :=
                    Character'Val
                      (Random_Number
                         (Character'Pos ('a'), Character'Pos ('z')));
               end if;
            end loop;
            if not Used.Contains (Id) then
               Used.Insert (Id, True);
               return Id;
            end if;
         end;
      end loop;
      --  raise Constraint_Error with "this cannot be";
   end New_Identifier;

   --------------------
   -- New_Identifier --
   --------------------

   function New_Identifier
     (Class_Name : String)
      return String
   is
   begin
      return Ada.Characters.Handling.To_Upper (Class_Name)
        & "-" & New_Identifier;
   end New_Identifier;

end Concorde.Identifiers;
