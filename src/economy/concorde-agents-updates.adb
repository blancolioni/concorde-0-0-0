with Ada.Containers.Doubly_Linked_Lists;

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

      package List_Of_References is
        new Ada.Containers.Doubly_Linked_Lists
          (Memor.Database_Reference, Memor."=");

      Hub_Refs : List_Of_References.List;

      procedure Update_Hub
        (Hub : in out Concorde.Installations.Root_Installation_Type'Class);
      procedure Update_Rec (Rec : in out Memor.Root_Record_Type'Class);

      ----------------
      -- Update_Hub --
      ----------------

      procedure Update_Hub
        (Hub : in out Concorde.Installations.Root_Installation_Type'Class)
      is
      begin
         Update (Hub);
      end Update_Hub;

      ----------------
      -- Update_Rec --
      ----------------

      procedure Update_Rec (Rec : in out Memor.Root_Record_Type'Class) is
      begin
         if Rec in Concorde.Installations.Root_Installation_Type'Class
           and then Concorde.Installations.Root_Installation_Type'Class
             (Rec).Is_Colony_Hub
         then
            Hub_Refs.Append (Rec.Reference);
         else
            Update (Root_Agent_Type'Class (Rec));
         end if;
      end Update_Rec;

   begin
      Concorde.Empires.Db.Iterate (Update_Rec'Access);
      Concorde.Installations.Db.Iterate (Update_Rec'Access);
      Concorde.People.Pops.Db.Iterate (Update_Rec'Access);
      for Ref of Hub_Refs loop
         Concorde.Installations.Db.Update
           (Ref, Update_Hub'Access);
      end loop;

   end Update_Agents;

end Concorde.Agents.Updates;
