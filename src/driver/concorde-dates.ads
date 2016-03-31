package Concorde.Dates is

   type Date_Type is new Natural;

   function Current_Date return Date_Type;
   function Current_Date_To_String return String;

   procedure Tick;

   function To_String (Date : Date_Type) return String;

end Concorde.Dates;
