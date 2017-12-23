with Ada.Characters.Handling;

package body Concorde.Trades is

   ---------------
   -- Metric_Id --
   ---------------

   function Metric_Id (Metric : Trade_Metric) return String is
      Result : String :=
                 Ada.Characters.Handling.To_Lower
                   (Trade_Metric'Image (Metric));
   begin
      for Ch of Result loop
         if Ch = '_' then
            Ch := '-';
         end if;
      end loop;
      return Result;
   end Metric_Id;

end Concorde.Trades;
