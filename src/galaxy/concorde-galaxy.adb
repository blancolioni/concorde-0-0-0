with Ada.Containers.Indefinite_Holders;

with Concorde.Scenarios;

package body Concorde.Galaxy is

   Local_Capital_World    : Concorde.Worlds.World_Type;

   package Path_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Systems.Graphs.Array_Of_Vertices,
        Concorde.Systems.Graphs."=");

   type Cached_System_Relation is
      record
         Path_Length : Non_Negative_Real;
         Path        : Path_Holders.Holder;
      end record;

   type Cached_System_Relation_Array is
     array (Positive range <>, Positive range <>) of Cached_System_Relation;

   Cached_System_Relations : access Cached_System_Relation_Array;

   function Get_System_Relation
     (From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Cached_System_Relation;

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

   -------------------
   -- Capital_World --
   -------------------

   function Capital_World return Concorde.Worlds.World_Type is
   begin
      return Local_Capital_World;
   end Capital_World;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (System_1, System_2 : Positive)
   is
   begin
      Galaxy_Graph.Connect (System_1, System_2, 1.0);
      Galaxy_Graph.Connect (System_2, System_1, 1.0);
   end Connect;

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
      if Concorde.Scenarios.Imperial_Centre then
         if OK (Galaxy_Graph.Vertex (1)) then
            return Galaxy_Graph.Vertex (1);
         else
            declare
               Index : constant Natural :=
                         Galaxy_Graph.Breadth_First_Search
                           (1, OK);
            begin
               if Index = 0 then
                  return null;
               else
                  return Galaxy_Graph.Vertex (Index);
               end if;
            end;
         end if;
      else
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
      end if;
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

   -------------------------
   -- Get_System_Relation --
   -------------------------

   function Get_System_Relation
     (From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Cached_System_Relation
   is
   begin
      if Cached_System_Relations = null then
         Cached_System_Relations :=
           new Cached_System_Relation_Array
             (1 .. Galaxy_Graph.Last_Vertex_Index,
              1 .. Galaxy_Graph.Last_Vertex_Index);
      end if;

      declare
         Item : Cached_System_Relation renames
                  Cached_System_Relations (From.Index, To.Index);
      begin
         if Item.Path.Is_Empty then
            declare
               Path : constant Concorde.Systems.Graphs.Path :=
                        Galaxy_Graph.Shortest_Path
                          (From.Index, To.Index);
            begin
               Item.Path.Replace_Element
                 (Concorde.Systems.Graphs.Get_Path (Path));
               Item.Path_Length :=
                 Concorde.Systems.Graphs.Cost (Path);
            end;
         end if;

         return Item;
      end;
   end Get_System_Relation;

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

   -------------
   -- Maximum --
   -------------

   function Maximum
     (Score : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Natural)
      return Concorde.Systems.Star_System_Type
   is
      Max_System : Concorde.Systems.Star_System_Type := null;
      Max_Value  : Natural := 0;
   begin
      for I in 1 .. Galaxy_Graph.Last_Vertex_Index loop
         declare
            System : constant Concorde.Systems.Star_System_Type :=
                       Galaxy_Graph.Vertex (I);
            Value  : constant Natural := Score (System);
         begin
            if Value > Max_Value then
               Max_Value := Value;
               Max_System := System;
            end if;
         end;
      end loop;
      return Max_System;
   end Maximum;

   -------------
   -- Minimum --
   -------------

   function Minimum
     (Score : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Natural)
      return Concorde.Systems.Star_System_Type
   is
      function Local_Score (System : Concorde.Systems.Star_System_Type)
                            return Natural
      is (Natural'Last - Score (System));
   begin
      return Maximum (Local_Score'Access);
   end Minimum;

   -----------------
   -- Move_Fleets --
   -----------------

--     procedure Move_Fleets
--       (From, To : Concorde.Systems.Star_System_Type;
--        Count    : Natural)
--     is
--        use type Concorde.Factions.Faction_Type;
--        use Concorde.Systems;
--        From_Index : constant Positive := From.Index;
--        To_Index   : constant Positive := To.Index;
--        F          : constant Star_System_Access :=
--                       Galaxy_Vector.Element (From_Index);
--        T          : constant Star_System_Access :=
--                       Galaxy_Vector.Element (To_Index);
--        Old_Owner  : constant Concorde.Factions.Faction_Type := T.Owner;
--     begin
--        F.Set_Fleets (F.Fleets - Count);
--        if Count > 0 and then T.Fleets = 0
--          and then (T.Owner = null or else T.Owner /= F.Owner)
--        then
--           T.Set_Owner (F.Owner);
--           if Old_Owner /= null then
--              Old_Owner.AI.System_Lost (To, F.Owner);
--           end if;
--           F.Owner.System_Acquired (To);
--           F.Owner.AI.System_Acquired (To, Old_Owner);
--        end if;
--
--        if T.Owner = F.Owner then
--           T.Set_Fleets (T.Fleets + Count);
--        else
--           declare
--              Defenders : Natural := T.Fleets;
--              Attackers : Natural := Count;
--           begin
--
--              if Defenders > 0 and then Attackers > 0 then
--                 T.Attacked (From);
--              end if;
--
--              for I in 1 .. Attackers + Defenders loop
--                 exit when Defenders = 0 or else Attackers = 0;
--                 if WL.Random.Random_Number (1, 100) <= 60 then
--                    Attackers := Attackers - 1;
--                 else
--                    Defenders := Defenders - 1;
--                 end if;
--              end loop;
--
--              if Attackers > 0 then
--                 if Defenders > 0 then
--                    F.Set_Fleets (F.Fleets + Attackers);
--                 else
--                    if T.Owner /= null then
--                       T.Owner.System_Lost (To);
--                       T.Owner.AI.System_Lost (To, F.Owner);
--                    end if;
--                    T.Set_Owner (F.Owner);
--                    F.Owner.System_Acquired (To);
--                    F.Owner.AI.System_Acquired (To, Old_Owner);
--                    T.Set_Fleets (Attackers);
--                 end if;
--              end if;
--           end;
--        end if;
--
--     end Move_Fleets;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Array_Of_Star_Systems
   is
   begin
      return Neighbours (System.Index);
   end Neighbours;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (System : Concorde.Systems.Root_Star_System_Type'Class)
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
      Result : Array_Of_Star_Systems (1 .. 40);
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
      return Galaxy_Graph.Connected (System_1.Index, System_2.Index);
   end Neighbours;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (System_1, System_2 : Positive)
      return Boolean
   is
   begin
      return Galaxy_Graph.Connected (System_1, System_2);
   end Neighbours;

   -----------------------
   -- Set_Capital_World --
   -----------------------

   procedure Set_Capital_World
     (World : Concorde.Worlds.World_Type)
   is
   begin
      Local_Capital_World := World;
   end Set_Capital_World;

   -------------------
   -- Shortest_Path --
   -------------------

   function Shortest_Path
     (From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      OK       : access function (System : Concorde.Systems.Star_System_Type)
      return Boolean := null)
      return Concorde.Systems.Graphs.Array_Of_Vertices
   is
      use Concorde.Systems;
   begin
      if OK = null then
         return Get_System_Relation (From, To).Path.Element;
      else
         declare
            Path : constant Concorde.Systems.Graphs.Path :=
                     Galaxy_Graph.Shortest_Path (From.Index, To.Index, OK);
         begin
            return Concorde.Systems.Graphs.Get_Path (Path);
         end;
      end if;
   end Shortest_Path;

   ----------------------------
   -- Shortest_Path_Distance --
   ----------------------------

   function Shortest_Path_Distance
     (System_1, System_2 : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Get_System_Relation (System_1, System_2).Path_Length;
--
--        Path : constant Concorde.Systems.Graphs.Path :=
--                 Galaxy_Graph.Shortest_Path
--                   (System_1.Index, System_2.Index);
--     begin
--        return Concorde.Systems.Graphs.Cost (Path);
   end Shortest_Path_Distance;

   ------------------
   -- System_Count --
   ------------------

   function System_Count return Natural is
   begin
      return Galaxy_Graph.Last_Vertex_Index;
   end System_Count;

end Concorde.Galaxy;
