package Concorde.Agents.Updates is

   procedure Update_Agents
     (Update : not null access
        procedure (Agent : in out Root_Agent_Type'Class));

   procedure Start_Of_Update
     (Agent : in out Root_Agent_Type'Class);

   procedure End_Of_Update
     (Agent : in out Root_Agent_Type'Class);

end Concorde.Agents.Updates;
