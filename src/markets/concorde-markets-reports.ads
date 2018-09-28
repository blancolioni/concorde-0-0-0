package Concorde.Markets.Reports is

   procedure Report_Market
     (Market : Market_Interface'Class);

   procedure Log_Market_State
     (Market : not null access constant Market_Interface'Class);

end Concorde.Markets.Reports;
