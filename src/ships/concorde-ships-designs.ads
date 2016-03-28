package Concorde.Ships.Designs is

   procedure Configure_Designs;

   function Create_Ship_From_Design
     (Name : String)
     return Ship_Type;

end Concorde.Ships.Designs;
