package body Concorde.Network is

   -------------------------
   -- To_Expression_Value --
   -------------------------

   function To_Expression_Value (X : Real) return Expression_Value is
   begin
      return Expression_Value'
        (Real_Value   => X,
         Object_Value => null);
   end To_Expression_Value;

   -------------------------
   -- To_Expression_Value --
   -------------------------

   function To_Expression_Value
     (X : not null access constant Expression_Object_Interface'Class)
      return Expression_Value
   is
   begin
      return Expression_Value'
        (Real_Value   => 0.0,
         Object_Value => Expression_Object (X));
   end To_Expression_Value;

end Concorde.Network;
