package body Concorde.People.Individuals is

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Individual : in out Root_Individual_Type;
      New_Name   : String)
   is
   begin
      Individual.First_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (New_Name);
   end Set_Name;

end Concorde.People.Individuals;
