with WL.String_Maps;

package body Concorde.Fields is

   package Field_Maps is
     new WL.String_Maps (Real_Field_Handler);

   Map : Field_Maps.Map;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field
     (Name : String;
      Fn   : Real_Field_Handler)
   is
   begin
      Map.Insert (Name, Fn);
   end Add_Field;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Rec  : Record_Type;
      Name : String)
      return Concorde.Network.Expression_Value
   is
   begin
      return Concorde.Network.To_Expression_Value
        (Map.Element (Name) (Rec));
   end Get_Field;

   ----------------
   -- Have_Field --
   ----------------

   function Have_Field (Name : String) return Boolean is
   begin
      return Map.Contains (Name);
   end Have_Field;

end Concorde.Fields;
