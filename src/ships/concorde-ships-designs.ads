package Concorde.Ships.Designs is

   procedure Configure_Designs;

   procedure Create_Ship_From_Design
     (Design_Name : String;
      Ship        : in out Root_Ship_Type'Class);

end Concorde.Ships.Designs;
