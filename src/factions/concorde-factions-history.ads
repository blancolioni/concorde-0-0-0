with Concorde.Dates;

package Concorde.Factions.History is

   procedure Update_History;

   type Historical_Metric is
     (Controlled_Systems,
      Ship_Count,
      Production);

   function Get_Metric
     (Date   : Concorde.Dates.Day_Index;
      Metric : Historical_Metric;
      Faction : Faction_Type)
      return Real;

end Concorde.Factions.History;
