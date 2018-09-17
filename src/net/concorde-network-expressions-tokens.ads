private package Concorde.Network.Expressions.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier,
       Tok_Integer_Constant, Tok_Float_Constant, Tok_String_Constant,

       Tok_If, Tok_Then, Tok_Else, Tok_Sum, Tok_Max, Tok_Min,

       Tok_Colon, Tok_Semi,
       Tok_Left_Paren, Tok_Right_Paren,
       Tok_Left_Brace, Tok_Right_Brace,
       Tok_Comma, Tok_Arrow, Tok_Vertical_Bar,
       Tok_Dot,
       Tok_Plus, Tok_Minus, Tok_Multiply, Tok_Divide, Tok_Power);

end Concorde.Network.Expressions.Tokens;
