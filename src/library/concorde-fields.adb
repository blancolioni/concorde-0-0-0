with WL.String_Maps;

package body Concorde.Fields is

   type Handler_Record_Type is (Real_Handler, Object_Handler);

   type Handler_Record (Handler_Type : Handler_Record_Type) is
      record
         case Handler_Type is
            when Real_Handler =>
               Real_Fn : Real_Field_Handler;
            when Object_Handler =>
               Object_Fn : Object_Field_Handler;
         end case;
      end record;

   package Field_Maps is
     new WL.String_Maps (Handler_Record);

   Map : Field_Maps.Map;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field
     (Name : String;
      Fn   : Real_Field_Handler)
   is
   begin
      Map.Insert (Name, (Real_Handler, Fn));
   end Add_Field;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field
     (Name : String;
      Fn   : Object_Field_Handler)
   is
   begin
      Map.Insert (Name, (Object_Handler, Fn));
   end Add_Field;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Rec  : Record_Type;
      Name : String)
      return Concorde.Network.Expression_Value
   is
      Handler : constant Handler_Record := Map.Element (Name);
   begin
      case Handler.Handler_Type is
         when Real_Handler =>
            return Concorde.Network.To_Expression_Value
              (Map.Element (Name).Real_Fn (Rec));
         when Object_Handler =>
            return Concorde.Network.To_Expression_Value
              (Map.Element (Name).Object_Fn (Rec));
      end case;
   end Get_Field;

   ----------------
   -- Have_Field --
   ----------------

   function Have_Field (Name : String) return Boolean is
   begin
      return Map.Contains (Name);
   end Have_Field;

end Concorde.Fields;
