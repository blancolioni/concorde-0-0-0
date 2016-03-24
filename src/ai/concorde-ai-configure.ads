package Concorde.AI.Configure is

   procedure Register;

   type AI_Constructor is
     access function return AI_Type;

   procedure Register
     (Name        : String;
      Constructor : AI_Constructor);

   function Get_AI (Name : String) return AI_Type;

end Concorde.AI.Configure;
