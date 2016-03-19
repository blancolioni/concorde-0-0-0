with WL.Random;

with Concorde.AI;
with Concorde.Empires;

package body Concorde.Galaxy is

   -----------------
   -- Add_Systems --
   -----------------

   procedure Add_Systems
     (To           : in out Star_System_Set;
      Start        : Concorde.Systems.Star_System_Type;
      Max_Distance : Non_Negative_Real)
   is
      Sub : Concorde.Systems.Graphs.Sub_Graph;
   begin
      Galaxy_Graph.Breadth_First_Search
        (Start.Index, Max_Distance, Sub);
      Concorde.Systems.Graphs.Append (To.Collection, Sub);
   end Add_Systems;

   -----------------
   -- Find_System --
   -----------------

   function Find_System
     (OK : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Boolean)
      return Concorde.Systems.Star_System_Type
   is
   begin
      for I in 1 .. Galaxy_Graph.Last_Vertex_Index loop
         declare
            System : constant Concorde.Systems.Star_System_Type :=
                       Galaxy_Graph.Vertex (I);
         begin
            if OK (System) then
               return System;
            end if;
         end;
      end loop;
      return null;
   end Find_System;

   ----------------
   -- Get_System --
   ----------------

   function Get_System
     (Index : Positive)
      return Concorde.Systems.Star_System_Type
   is
   begin
      return Galaxy_Graph.Vertex (Index);
   end Get_System;

   -----------------
   -- Get_Systems --
   -----------------

   function Get_Systems
     (OK : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Boolean)
      return Array_Of_Star_Systems
   is
      Result : Array_Of_Star_Systems (1 .. Galaxy_Graph.Last_Vertex_Index);
      Count  : Natural := 0;
   begin
      for I in Result'Range loop
         if OK (Galaxy_Graph.Vertex (I)) then
            Count := Count + 1;
            Result (Count) := Galaxy_Graph.Vertex (I);
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Systems;

   ----------------
   -- Is_Element --
   ----------------

   function Is_Element
     (Of_Set : Star_System_Set;
      System : Concorde.Systems.Star_System_Type)
      return Boolean
   is
   begin
      return Concorde.Systems.Graphs.Contains
        (Of_Set.Collection, System.Index);
   end Is_Element;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Filter : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Boolean;
      Process : not null access
        procedure (System : Concorde.Systems.Star_System_Type))
   is
   begin
      for I in 1 .. Galaxy_Graph.Last_Vertex_Index loop
         if Filter (Galaxy_Graph.Vertex (I)) then
            Process (Galaxy_Graph.Vertex (I));
         end if;
      end loop;

   end Iterate;

   -----------------
   -- Move_Fleets --
   -----------------

   procedure Move_Fleets
     (From, To : Concorde.Systems.Star_System_Type;
      Count    : Natural)
   is
      use type Concorde.Empires.Empire_Type;
      use Concorde.Systems;
      From_Index : constant Positive := From.Index;
      To_Index   : constant Positive := To.Index;
      F          : constant Star_System_Access :=
                     Galaxy_Vector.Element (From_Index);
      T          : constant Star_System_Access :=
                     Galaxy_Vector.Element (To_Index);
      Old_Owner  : constant Concorde.Empires.Empire_Type := T.Owner;
   begin
      F.Set_Fleets (F.Fleets - Count);
      if Count > 0 and then T.Fleets = 0
        and then (T.Owner = null or else T.Owner /= F.Owner)
      then
         T.Set_Owner (F.Owner);
         if Old_Owner /= null then
            Old_Owner.AI.System_Lost (To, F.Owner);
         end if;
         F.Owner.AI.System_Acquired (To, Old_Owner);
      end if;

      if T.Owner = F.Owner then
         T.Set_Fleets (T.Fleets + Count);
      else
         declare
            Defenders : Natural := T.Fleets;
            Attackers : Natural := Count;
         begin

            if Defenders > 0 and then Attackers > 0 then
               T.Attacked (From);
            end if;

            for I in 1 .. Attackers + Defenders loop
               exit when Defenders = 0 or else Attackers = 0;
               if WL.Random.Random_Number (1, 100) <= 60 then
                  Attackers := Attackers - 1;
               else
                  Defenders := Defenders - 1;
               end if;
            end loop;

            if Attackers > 0 then
               if Defenders > 0 then
                  F.Set_Fleets (F.Fleets + Attackers);
               else
                  if T.Owner /= null then
                     T.Owner.AI.System_Lost (To, F.Owner);
                  end if;
                  T.Set_Owner (F.Owner);
                  F.Owner.AI.System_Acquired (To, Old_Owner);
                  T.Set_Fleets (Attackers);
               end if;
            end if;
         end;
      end if;

   end Move_Fleets;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (System : Concorde.Systems.Star_System_Type)
      return Array_Of_Star_Systems
   is
   begin
      return Neighbours (System.Index);
   end Neighbours;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (System_Index : Positive)
      return Array_Of_Star_Systems

   is
      use Concorde.Systems;
      Result : Array_Of_Star_Systems (1 .. 20);
      Count  : Natural := 0;

      procedure Add_System
        (Index : Positive;
         Cost  : Non_Negative_Real);

      ----------------
      -- Add_System --
      ----------------

      procedure Add_System
        (Index : Positive;
         Cost  : Non_Negative_Real)
      is
         pragma Unreferenced (Cost);
      begin
         Count := Count + 1;
         Result (Count) := Galaxy_Graph.Vertex (Index);
      end Add_System;

   begin
      Galaxy_Graph.Iterate_Edges
        (System_Index,
         Add_System'Access);
      return Result (1 .. Count);
   end Neighbours;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (System_1, System_2 : Concorde.Systems.Star_System_Type)
      return Boolean
   is
   begin
      return Galaxy_Graph.Connected (System_1, System_2);
   end Neighbours;

   ----------------------------
   -- Shortest_Path_Distance --
   ----------------------------

   function Shortest_Path_Distance
     (System_1, System_2 : Concorde.Systems.Star_System_Type)
      return Non_Negative_Real
   is
      Path : constant Concorde.Systems.Graphs.Path :=
               Galaxy_Graph.Shortest_Path
                 (System_1.Index, System_2.Index);
   begin
      return Concorde.Systems.Graphs.Cost (Path);
   end Shortest_Path_Distance;

   ------------------
   -- System_Count --
   ------------------

   function System_Count return Natural is
   begin
      return Galaxy_Graph.Last_Vertex_Index;
   end System_Count;

   -------------------
   -- Update_System --
   -------------------

   procedure Update_System
     (System : Concorde.Systems.Star_System_Type;
      Update : not null access
        procedure (System : in out Systems.Root_Star_System_Type'Class))
   is
      Index : constant Natural := System.Index;
   begin
      Update (Galaxy_Vector.Element (Index).all);
   end Update_System;

end Concorde.Galaxy;
