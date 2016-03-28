package Concorde.Components.Manager is

   procedure Register
     (Name      : String;
      Component : not null access constant Root_Component_Type'Class);

   function Get
     (Name : String)
      return Component_Type;

end Concorde.Components.Manager;
