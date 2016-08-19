with Concorde.Elementary_Functions;

with Concorde.Stars.Db;
with Concorde.Stars.Tables;

package body Concorde.Stars.Create is

   --------------
   -- New_Star --
   --------------

   function New_Main_Sequence_Star
     (Name         : String;
      Solar_Masses : Non_Negative_Real)
      return Star_Type
   is

      Class      : Stellar_Class_Type;
      Subclass   : Stellar_Subclass_Type;
      Colour     : Lui.Colours.Colour_Type;
      Radius     : Non_Negative_Real;
      Luminosity : Non_Negative_Real;

      procedure Create (Star : in out Root_Star_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Star : in out Root_Star_Type'Class) is
      begin
         Star.Set_Name (Name);
         Star.Solar_Masses := Solar_Masses;
         Star.Class := Class;
         Star.Subclass := Subclass;
         Star.Size := Main_Sequence;
         Star.Colour := Colour;
         Star.Radius := Radius;
         Star.Luminosity := Luminosity;
         Star.Ecosphere := Concorde.Elementary_Functions.Sqrt (Luminosity);
         Star.Age := 1.0E10 * Solar_Masses / Luminosity;
      end Create;

   begin
      Concorde.Stars.Tables.Get_Main_Sequence_Info
        (Solar_Masses => Solar_Masses,
         Class        => Class,
         Subclass     => Subclass,
         Radius       => Radius,
         Luminosity   => Luminosity,
         Colour       => Colour);

      return Concorde.Stars.Db.Create (Create'Access);
   end New_Main_Sequence_Star;

end Concorde.Stars.Create;
