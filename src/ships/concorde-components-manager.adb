with WL.String_Maps;

package body Concorde.Components.Manager is

   package Component_Tables is
     new WL.String_Maps (Component_Type);

   Table : Component_Tables.Map;

   ---------
   -- Get --
   ---------

   function Get
     (Name : String)
      return Component_Type
   is
   begin
      return Table.Element (Name);
   end Get;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name      : String;
      Component : not null access constant Root_Component_Type'Class)
   is
   begin
      Table.Insert (Name, Component_Type (Component));
   end Register;

end Concorde.Components.Manager;
