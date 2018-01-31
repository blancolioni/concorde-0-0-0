with Tropos;

package Concorde.Powers.Configure is

   function Get_Power (Name : String) return Power_Type;

   function Configure_Power
     (Config : Tropos.Configuration)
      return Power_Type;

   procedure Configure_Power_Set
     (Config : Tropos.Configuration;
      Set    : in out Power_Set);

end Concorde.Powers.Configure;
