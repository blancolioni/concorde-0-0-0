with Concorde.People.Groups;

package body Concorde.People.Communities is

   --------------
   -- Add_Node --
   --------------

   overriding procedure Add_Node
     (Community : in out Root_Community_Type;
      Node      : Concorde.Network.Node_State_Access)
   is
   begin
      Community.Network.Add_Node (Node);
   end Add_Node;

   -------------
   -- Add_Pop --
   -------------

   procedure Add_Pop
     (Community : in out Root_Community_Type'Class;
      Pop       : not null access constant
        Concorde.People.Pops.Root_Pop_Type'Class)
   is
   begin
      Community.Pops.Append (Concorde.People.Pops.Pop_Type (Pop));
   end Add_Pop;

   ----------------------
   -- Current_Location --
   ----------------------

   overriding function Current_Location
     (Community : Root_Community_Type)
      return Concorde.Locations.Object_Location
   is
   begin
      return Concorde.Locations.World_Surface
        (Community.World, 1);
   end Current_Location;

   -------------------------
   -- Evaluate_Constraint --
   -------------------------

   overriding function Evaluate_Constraint
     (From             : Root_Community_Type;
      Class_Name       : String;
      Constraint_Name  : String;
      Constraint_Value : String)
      return Concorde.Network.Array_Of_Values
   is

      type Constraint_Type is (All_Constraint, Group_Constraint);

      function Eval_Pop_Constraint return Concorde.Network.Array_Of_Values;

      -------------------------
      -- Eval_Pop_Constraint --
      -------------------------

      function Eval_Pop_Constraint return Concorde.Network.Array_Of_Values is
         Result : Concorde.Network.Array_Of_Values
           (1 .. Natural (From.Pops.Length));
         Count  : Natural := 0;
         Constraint : constant Constraint_Type :=
                        (if Constraint_Name = "group"
                         then Group_Constraint
                         elsif Constraint_Name = "all"
                         then All_Constraint
                         else raise Constraint_Error with
                           "no such pop constraint: " & Constraint_Name);
         Group      : constant Concorde.People.Groups.Pop_Group :=
                        (case Constraint is
                            when All_Constraint =>
                              null,
                            when Group_Constraint =>
                              Concorde.People.Groups.Get (Constraint_Value));
      begin
         for Pop of From.Pops loop
            declare
               Include : constant Boolean :=
                           (case Constraint is
                               when All_Constraint => True,
                               when Group_Constraint =>
                                 Pop.Is_Member_Of (Group));
            begin
               if Include then
                  Count := Count + 1;
                  Result (Count) :=
                    Concorde.Network.To_Expression_Value (Pop);
               end if;
            end;
         end loop;
         return Result (1 .. Count);
      end Eval_Pop_Constraint;

   begin
      if Class_Name = "pops" then
         return Eval_Pop_Constraint;
      else
         return From.Network.Evaluate_Constraint
           (Class_Name, Constraint_Name, Constraint_Value);
      end if;
   end Evaluate_Constraint;

   -----------------
   -- Location_At --
   -----------------

   overriding function Location_At
     (Community : Root_Community_Type;
      Time      : Concorde.Calendar.Time)
      return Concorde.Locations.Object_Location
   is
      pragma Unreferenced (Time);
   begin
      return Root_Community_Type'Class (Community).Current_Location;
   end Location_At;

   ----------
   -- Node --
   ----------

   overriding function Node
     (Community : Root_Community_Type;
      Name      : String)
      return Concorde.Network.Node_State_Access
   is
   begin
      return Community.Network.Node (Name);
   end Node;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Community_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   ----------
   -- Scan --
   ----------

   procedure Scan (Process : not null access
                     procedure (Community : Community_Type))
   is
   begin
      Db.Scan (Process);
   end Scan;

   ----------------------
   -- Scan_Individuals --
   ----------------------

   procedure Scan_Individuals
     (Community : Root_Community_Type'Class;
      Process   : not null access
        procedure (Individual : Concorde.People.Individuals.Individual_Type))
   is
   begin
      for Individual of Community.Individuals loop
         Process (Individual);
      end loop;
   end Scan_Individuals;

   ---------------
   -- Scan_Pops --
   ---------------

   procedure Scan_Pops
     (Community : Root_Community_Type'Class;
      Process   : not null access
        procedure (Pop : Concorde.People.Pops.Pop_Type))
   is
   begin
      for Pop of Community.Pops loop
         Process (Pop);
      end loop;
   end Scan_Pops;

   ----------------------
   -- Scan_State_Nodes --
   ----------------------

   overriding procedure Scan_State_Nodes
     (Community : Root_Community_Type;
      Process   : not null access
        procedure (Node_State : Concorde.Network.Node_State_Access))
   is
      procedure Internal_Process
        (Node_State : Concorde.Network.Node_State_Access);

      ----------------------
      -- Internal_Process --
      ----------------------

      procedure Internal_Process
        (Node_State : Concorde.Network.Node_State_Access)
      is
      begin
         Process (Node_State);
      end Internal_Process;

   begin
      Community.Network.Scan_State_Nodes (Internal_Process'Access);
   end Scan_State_Nodes;

   ------------------
   -- Set_Location --
   ------------------

   overriding procedure Set_Location
     (Community : in out Root_Community_Type;
      Location  : Concorde.Locations.Object_Location)
   is
      pragma Unreferenced (Community, Location);
   begin
      null;
   end Set_Location;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (Community : in out Root_Community_Type'Class;
      Faction   : not null access constant
        Concorde.Factions.Root_Faction_Type'Class)
   is
   begin
      Community.Owner := Concorde.Factions.Faction_Type (Faction);
   end Set_Owner;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Community_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Concorde.People.Communities;
