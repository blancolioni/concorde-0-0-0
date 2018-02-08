package body Concorde.People.Traits.Configure is

   ---------------------
   -- Configure_Trait --
   ---------------------

   procedure Configure_Trait
     (Config : Tropos.Configuration)
   is
      procedure Create (Trait : in out Root_Trait_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Trait : in out Root_Trait_Type'Class) is
      begin
         Trait.Set_Local_Tag (Config.Config_Name);
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Trait;

end Concorde.People.Traits.Configure;
