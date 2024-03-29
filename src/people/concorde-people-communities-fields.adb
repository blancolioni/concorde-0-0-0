with Concorde.Fields;
with Concorde.Worlds;

package body Concorde.People.Communities.Fields is

   package Community_Fields is
     new Concorde.Fields (Root_Community_Type'Class);

   function Get_Agricultural_Land_Use
     (Community : Root_Community_Type'Class)
      return Real
   is (Community.Land_Use (Agricultural).Absolute);

   function Get_World
     (Community : Root_Community_Type'Class)
      return access constant
     Concorde.Network.Expression_Object_Interface'Class
   is (Community.World);

   function Get_World_Occupation
     (Community : Root_Community_Type'Class)
      return Real
   is (Community.Occupation);

   function Get_Local_Commodity
     (Community : Root_Community_Type'Class;
      Name      : String)
      return access constant
     Concorde.Network.Expression_Object_Interface'Class
   is (Community.Local_Commodities.Element
       (Commodities.Get (Name)));

   -----------------------------
   -- Create_Commodity_Fields --
   -----------------------------

   procedure Create_Commodity_Fields is
      procedure Add (Commodity : Concorde.Commodities.Commodity_Type);

      ---------
      -- Add --
      ---------

      procedure Add (Commodity : Concorde.Commodities.Commodity_Type) is
      begin
         Community_Fields.Add_Field (Commodity.Identifier,
                                     Get_Local_Commodity'Access);
      end Add;

   begin
      Concorde.Commodities.Scan (Add'Access);
   end Create_Commodity_Fields;

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
   Community_Fields.Add_Field ("world",
                               Get_World'Access);
   Community_Fields.Add_Field ("world-occupation",
                               Get_World_Occupation'Access);
   Community_Fields.Add_Field ("agricultural-land",
                               Get_Agricultural_Land_Use'Access);
end Concorde.People.Communities.Fields;
