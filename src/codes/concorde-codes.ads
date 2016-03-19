package Concorde.Codes is

   type Code_Type is private;

   function New_Code return Code_Type;

   function Distance (From, To : Code_Type) return Non_Negative_Real;

end Concorde.Codes;
