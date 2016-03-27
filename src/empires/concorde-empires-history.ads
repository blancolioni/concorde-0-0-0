with Concorde.Dates;

package Concorde.Empires.History is

   procedure Update_History;

   type Historical_Metric is
     (Controlled_Systems,
      Ship_Count,
      Capacity,
      Production);

   function Get_Metric
     (Date   : Concorde.Dates.Date_Type;
      Metric : Historical_Metric;
      Empire : Empire_Type)
      return Real;

end Concorde.Empires.History;
