with Concorde.Empires.Db;
with Concorde.Installations.Db;
with Concorde.People.Pops.Db;

package body Concorde.Agents.Updates is

   -------------------
   -- End_Of_Update --
   -------------------

   procedure End_Of_Update
     (Agent : in out Root_Agent_Type'Class)
   is
   begin
      Agent.Age := Agent.Age + 1;
   end End_Of_Update;

   ---------------------
   -- Start_Of_Update --
   ---------------------

   procedure Start_Of_Update
     (Agent : in out Root_Agent_Type'Class)
   is
      pragma Unreferenced (Agent);
   begin
      null;
   end Start_Of_Update;

   -------------------
   -- Update_Agents --
   -------------------

   procedure Update_Agents
     (Update : not null access
        procedure (Agent : in out Root_Agent_Type'Class))
   is
      procedure Update_Rec (Rec : in out Memor.Root_Record_Type'Class);

      ----------------
      -- Update_Rec --
      ----------------

      procedure Update_Rec (Rec : in out Memor.Root_Record_Type'Class) is
      begin
         Update (Root_Agent_Type'Class (Rec));
      end Update_Rec;

   begin
      Concorde.Empires.Db.Iterate (Update_Rec'Access);
      Concorde.Installations.Db.Iterate (Update_Rec'Access);
      Concorde.People.Pops.Db.Iterate (Update_Rec'Access);
   end Update_Agents;

end Concorde.Agents.Updates;
