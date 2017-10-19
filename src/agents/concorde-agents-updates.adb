package body Concorde.Agents.Updates is

   -------------------
   -- End_Of_Update --
   -------------------

   procedure End_Of_Update
     (Agent : in out Root_Agent_Type'Class)
   is
   begin
      Agent.Age := Agent.Age + 1;
      Agent.On_Update_End;
   end End_Of_Update;

   ---------------------
   -- Start_Of_Update --
   ---------------------

   procedure Start_Of_Update
     (Agent : in out Root_Agent_Type'Class)
   is
   begin
      Agent.On_Update_Start;
   end Start_Of_Update;

   -------------------
   -- Update_Agents --
   -------------------

   procedure Update_Agents
     (Update : not null access
        procedure (Agent : in out Root_Agent_Type'Class))
   is

      procedure Update_Rec (Rec : not null access
                              Memor.Root_Record_Type'Class);

      ----------------
      -- Update_Rec --
      ----------------

      procedure Update_Rec (Rec : not null access
                              Memor.Root_Record_Type'Class)
      is
      begin
         Update (Root_Agent_Type'Class (Rec.all));
      end Update_Rec;

   begin
      for Agent of Agent_List loop
         Agent.Object_Database.Update (Agent.Reference, Update_Rec'Access);
      end loop;
   end Update_Agents;

end Concorde.Agents.Updates;
