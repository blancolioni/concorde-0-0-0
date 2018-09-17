with Concorde.Fields;

package body Concorde.Worlds.Fields is

   package World_Fields is
     new Concorde.Fields (Root_World_Type'Class);

   function Get_Habitability
     (World : Root_World_Type'Class)
      return Real
   is (Unit_Clamp (1.0 - abs (World.Surface_Temp - 288.0) / 20.0));

   function Get_Hydrosphere
     (World : Root_World_Type'Class)
      return Real
   is (World.Hydrosphere);

   function Get_Surface_Area
     (World : Root_World_Type'Class)
      return Real
   is (4.0 / 3.0 * World.Radius ** 2);

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (World : Root_World_Type'Class;
      Name  : String)
      return Concorde.Network.Expression_Value
   is
   begin
      return World_Fields.Get_Field (World, Name);
   end Get_Field;

   ----------------
   -- Have_Field --
   ----------------

   function Have_Field (Name : String) return Boolean is
   begin
      return World_Fields.Have_Field (Name);
   end Have_Field;

begin
   World_Fields.Add_Field ("habitability", Get_Habitability'Access);
   World_Fields.Add_Field ("hydrosphere", Get_Hydrosphere'Access);
   World_Fields.Add_Field ("surface-area", Get_Surface_Area'Access);
end Concorde.Worlds.Fields;
