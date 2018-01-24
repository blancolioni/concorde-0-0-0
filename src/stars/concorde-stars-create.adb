with Concorde.Elementary_Functions;

with Concorde.Stars.Tables;

with Concorde.Solar_System;

package body Concorde.Stars.Create is

   --------------
   -- New_Star --
   --------------

   function New_Main_Sequence_Star
     (System       : Concorde.Systems.Star_System_Type;
      Name         : String;
      Solar_Masses : Non_Negative_Real)
      return Star_Type
   is

      Class      : Stellar_Class_Type;
      Subclass   : Stellar_Subclass_Type;
      Color      : Xi.Color.Xi_Color;
      Radius     : Non_Negative_Real;
      Luminosity : Non_Negative_Real;

      procedure Create (Star : in out Root_Star_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Star : in out Root_Star_Type'Class) is
      begin
         Star.Set_Name (Name);
         Star.System := System;
         Star.Set_Location
           (Concorde.Locations.System_Point
              (System, (0.0, 0.0, 0.0), (0.0, 0.0, 0.0)));

         Star.Solar_Masses := Solar_Masses;
         Star.Class := Class;
         Star.Subclass := Subclass;
         Star.Size := Main_Sequence;
         Star.Color := Color;
         Star.Radius := Radius * Concorde.Solar_System.Solar_Radius;
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
         Color        => Color);

      return Concorde.Stars.Db.Create (Create'Access);
   end New_Main_Sequence_Star;

end Concorde.Stars.Create;
