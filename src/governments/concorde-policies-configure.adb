package body Accord.Policies.Configure is

   ----------------------
   -- Configure_Policy --
   ----------------------

   function Configure_Policy
     (Config : Tropos.Configuration)
      return Accord_Policy_Type'Class
   is
      Policy : Accord_Policy_Type;
   begin
      Policy.Initialize_Object (Config.Config_Name);
      return Policy;
   end Configure_Policy;

end Accord.Policies.Configure;
