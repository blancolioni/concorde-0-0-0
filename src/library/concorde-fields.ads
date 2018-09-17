with Concorde.Network;

generic
   type Record_Type (<>) is limited private;
package Concorde.Fields is

   pragma Elaborate_Body;

   type Real_Field_Handler is access
     function (Rec : Record_Type) return Real;

   function Have_Field (Name : String) return Boolean;
   function Get_Field (Rec  : Record_Type;
                       Name : String)
                       return Concorde.Network.Expression_Value
     with Pre => Have_Field (Name);

   procedure Add_Field
     (Name : String;
      Fn   : Real_Field_Handler);

end Concorde.Fields;
