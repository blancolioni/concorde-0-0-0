with Concorde.Network;

private package Concorde.Worlds.Fields is

   function Have_Field (Name : String) return Boolean;
   function Get_Field
     (World : Root_World_Type'Class;
      Name  : String)
      return Concorde.Network.Expression_Value;

end Concorde.Worlds.Fields;
