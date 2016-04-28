with Concorde.Systems.Updates;
with Concorde.Systems.Db;

package body Concorde.Economy.Updates is

   ------------------
   -- Daily_Update --
   ------------------

   procedure Daily_Update is
   begin
      Concorde.Systems.Db.Scan
        (Concorde.Systems.Has_Market'Access,
         Concorde.Systems.Updates.Update_Market'Access);
      Concorde.Systems.Db.Scan
        (Concorde.Systems.Has_Market'Access,
         Concorde.Systems.Updates.Execute_Trades'Access);
      Concorde.Systems.Db.Scan
        (Concorde.Systems.Has_Market'Access,
         Concorde.Systems.Updates.Execute_Production'Access);
   end Daily_Update;

end Concorde.Economy.Updates;
