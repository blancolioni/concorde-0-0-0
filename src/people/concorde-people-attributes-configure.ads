with Tropos;

package Concorde.People.Attributes.Configure is

   procedure Configure_Attributes
     (Container : in out Attribute_Container'Class;
      Config    : Tropos.Configuration);

   function Configure_Attribute
     (Config : Tropos.Configuration)
      return Attribute_Reference;

end Concorde.People.Attributes.Configure;
