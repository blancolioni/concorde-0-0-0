package Concorde.Stars.Classification is

   function Get_Stellar_Classification
     (Name : String)
      return Stellar_Classification;

   function Solar_Masses
     (Classification : Stellar_Classification)
      return Non_Negative_Real;

end Concorde.Stars.Classification;
