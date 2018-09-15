with WL.String_Maps;

package body Concorde.People.Communities.Fields is

   type Field_Get_Handler is access
     function (W : Root_Community_Type'Class)
               return Concorde.Network.Expression_Value;

   package Field_Handler_Maps is
     new WL.String_Maps (Field_Get_Handler);

   Field_Handlers : Field_Handler_Maps.Map;

   function World
     (Community : Root_Community_Type'Class)
      return Concorde.Network.Expression_Value
   is (Concorde.Network.To_Expression_Value (Community.World));

   function World_Occupation
     (Community : Root_Community_Type'Class)
      return Concorde.Network.Expression_Value
   is (Concorde.Network.To_Expression_Value (Community.Occupation));

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Community : Root_Community_Type'Class;
      Name  : String)
      return Concorde.Network.Expression_Value
   is
   begin
      return Field_Handlers.Element (Name) (Community);
   end Get_Field;

   ----------------
   -- Have_Field --
   ----------------

   function Have_Field (Name : String) return Boolean is
   begin
      return Field_Handlers.Contains (Name);
   end Have_Field;

begin
   Field_Handlers.Insert ("world", World'Access);
   Field_Handlers.Insert ("world-occupation", World_Occupation'Access);
end Concorde.People.Communities.Fields;
