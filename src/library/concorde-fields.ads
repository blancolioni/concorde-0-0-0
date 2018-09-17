with Concorde.Network;

generic
   type Record_Type (<>) is limited private;
package Concorde.Fields is

   pragma Elaborate_Body;

   type Real_Field_Handler is access
     function (Rec : Record_Type) return Real;

   type Object_Field_Handler is access
     function (Rec : Record_Type)
               return access constant
     Concorde.Network.Expression_Object_Interface'Class;

   function Have_Field (Name : String) return Boolean;
   function Get_Field (Rec  : Record_Type;
                       Name : String)
                       return Concorde.Network.Expression_Value
     with Pre => Have_Field (Name);

   procedure Add_Field
     (Name : String;
      Fn   : Real_Field_Handler)
     with Pre => not Have_Field (Name) and then Fn /= null,
     Post => Have_Field (Name);

   procedure Add_Field
     (Name : String;
      Fn   : Object_Field_Handler)
     with Pre => not Have_Field (Name) and then Fn /= null,
     Post => Have_Field (Name);

end Concorde.Fields;
