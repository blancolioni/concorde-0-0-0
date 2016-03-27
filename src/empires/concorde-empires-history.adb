with Ada.Containers.Vectors;

package body Concorde.Empires.History is

   use Concorde.Dates;

   type Metric_Record is
     array (Historical_Metric) of Real;

   package History_Record_Vectors is
     new Ada.Containers.Vectors (Positive, Metric_Record);

   package History_Vectors is
     new Ada.Containers.Vectors
       (Date_Type, History_Record_Vectors.Vector,
        History_Record_Vectors."=");

   History : History_Vectors.Vector;

   ----------------
   -- Get_Metric --
   ----------------

   function Get_Metric
     (Date   : Concorde.Dates.Date_Type;
      Metric : Historical_Metric;
      Empire : Empire_Type)
      return Real
   is
   begin
      return History.Element (Date).Element (Empire.Index) (Metric);
   end Get_Metric;

   --------------------
   -- Update_History --
   --------------------

   procedure Update_History is
      R : History_Record_Vectors.Vector;
   begin
      for Empire of Vector loop
         declare
            M : constant Metric_Record :=
                  (Controlled_Systems => Real (Empire.Current_Systems),
                   Ship_Count         => Real (Empire.Current_Ships),
                   Capacity           => Empire.Max_Ships,
                   Production         => 0.0);
         begin
            R.Append (M);
         end;
      end loop;

      History.Append (R);

   end Update_History;

end Concorde.Empires.History;
