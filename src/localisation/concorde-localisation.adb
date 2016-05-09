with Tropos.Reader;

with Concorde.Paths;

with Concorde.Hash_Table;

package body Concorde.Localisation is

   package Local_Text_Table is
     new Concorde.Hash_Table (String);

   Local_Text : Local_Text_Table.Map;

   -----------------------
   -- Load_Localisation --
   -----------------------

   procedure Load_Localisation
     (Language : String)
   is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Concorde.Paths.Config_Path & "/localisation/"
                    & Language & ".txt");
   begin
      for Local of Config loop
         Local_Text.Insert (Local.Config_Name, Local.Value);
      end loop;
   end Load_Localisation;

   ----------------
   -- Local_Name --
   ----------------

   function Local_Name
     (Tag : String)
      return String
   is
   begin
      if Local_Text.Contains (Tag) then
         return Local_Text.Element (Tag);
      else
         return Tag;
      end if;
   end Local_Name;

end Concorde.Localisation;
