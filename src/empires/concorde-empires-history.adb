with Ada.Containers.Vectors;

with Memor.Element_Vectors;

with Concorde.Empires.Db;

package body Concorde.Empires.History is

   use Concorde.Dates;

   type Metric_Record is
     array (Historical_Metric) of Real;

   package History_Record_Vectors is
     new Memor.Element_Vectors (Metric_Record, (others => 0.0));

   package History_Vectors is
     new Ada.Containers.Vectors
       (Day_Index, History_Record_Vectors.Vector,
        History_Record_Vectors."=");

   History : History_Vectors.Vector;

   ----------------
   -- Get_Metric --
   ----------------

   function Get_Metric
     (Date   : Concorde.Dates.Day_Index;
      Metric : Historical_Metric;
      Empire : Empire_Type)
      return Real
   is
      Info : Metric_Record renames
               History.Element (Date).Element (Empire.Reference);
   begin
      return Info (Metric);
   end Get_Metric;

   --------------------
   -- Update_History --
   --------------------

   procedure Update_History is

      R : History_Record_Vectors.Vector;

      procedure Update (Empire : Root_Empire_Type'Class);

      ------------
      -- Update --
      ------------

      procedure Update (Empire : Root_Empire_Type'Class) is
         M : constant Metric_Record :=
               (Controlled_Systems => Real (Empire.Current_Systems),
                Ship_Count         => Real (Empire.Current_Ships),
                Production         => 0.0);
      begin
         R.Replace_Element (Empire.Reference, M);
      end Update;

   begin
      Db.Scan (Update'Access);
      History.Append (R);

   end Update_History;

end Concorde.Empires.History;
