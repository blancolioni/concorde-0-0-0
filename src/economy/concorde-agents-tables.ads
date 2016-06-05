with Lui.Tables;

package Concorde.Agents.Tables is

   function Account_Table
     (Agent : not null access constant Root_Agent_Type'Class)
      return Lui.Tables.Model_Table;

end Concorde.Agents.Tables;
