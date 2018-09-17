with Concorde.Fields;

package body Concorde.People.Communities.Fields is

   package Community_Fields is
     new Concorde.Fields (Root_Community_Type'Class);

   function World
     (Community : Root_Community_Type'Class)
      return access constant
     Concorde.Network.Expression_Object_Interface'Class
   is (Community.World);

   function World_Occupation
     (Community : Root_Community_Type'Class)
      return Real
   is (Community.Occupation);

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Community : Root_Community_Type'Class;
      Name  : String)
      return Concorde.Network.Expression_Value
   is
   begin
      return Community_Fields.Get_Field (Community, Name);
   end Get_Field;

   ----------------
   -- Have_Field --
   ----------------

   function Have_Field (Name : String) return Boolean is
   begin
      return Community_Fields.Have_Field (Name);
   end Have_Field;

begin
   Community_Fields.Add_Field ("world", World'Access);
   Community_Fields.Add_Field ("world-occupation", World_Occupation'Access);
end Concorde.People.Communities.Fields;
