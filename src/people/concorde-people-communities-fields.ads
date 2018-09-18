with Concorde.Network;

private package Concorde.People.Communities.Fields is

   function Have_Field (Name : String) return Boolean;
   function Get_Field (Community : Root_Community_Type'Class;
                       Name      : String)
                       return Concorde.Network.Expression_Value;

   procedure Create_Commodity_Fields;

end Concorde.People.Communities.Fields;
