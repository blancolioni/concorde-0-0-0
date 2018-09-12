package Concorde.Network.Expressions.Parser is

   function Parse_String
     (Text : String)
      return Expression_Type;

   procedure Parse_Configuration
     (Path      : String;
      On_Enter  : not null access
        procedure (Field_Name : String);
      On_Leave  : not null access
        procedure (Field_Name : String);
      On_Config : not null access
        procedure (Field_Name : String;
                   Field_Value : Expression_Type));

end Concorde.Network.Expressions.Parser;
