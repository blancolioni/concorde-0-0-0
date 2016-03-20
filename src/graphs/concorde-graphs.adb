package body Concorde.Graphs is

   ------------
   -- Append --
   ------------

   procedure Append
     (Container : in out Graph;
      Vertex    : Vertex_Type)
   is
   begin
      Container.Vertices.Append
        ((Vertex => Vertex,
          Index  => Container.Vertices.Last_Index + 1,
          Edges  => Edge_Lists.Empty_List));
      Container.Vs.Append (Vertex);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Sub   : in out Sub_Graph;
      Index : Index_Type)
   is
   begin
      Sub.Vertex_List.Append (Index);
      Sub.Vertex_Flags.Replace_Element (Index, True);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Collection : in out Sub_Graph_Collection;
      Sub        : Sub_Graph)
   is
   begin
      Collection.List.Append (Sub);
   end Append;

   --------------------------
   -- Breadth_First_Search --
   --------------------------

   procedure Breadth_First_Search
     (Container : Graph;
      Start     : Index_Type;
      Max       : Cost_Type;
      Result    : out Sub_Graph)
   is
      type Partial is
         record
            Index : Index_Type;
            Cost  : Cost_Type;
         end record;

      package Queue_Of_Partials is
         new Ada.Containers.Doubly_Linked_Lists (Partial);
      Queue : Queue_Of_Partials.List;
   begin
      Container.Create (Result);
      Queue.Append ((Start, 0.0));
      while not Queue.Is_Empty loop
         declare
            P    : constant Partial := Queue.First_Element;
            Ix   : constant Index_Type := P.Index;
            Cost : constant Cost_Type := P.Cost;
         begin
            Queue.Delete_First;
            if not Contains (Result, Ix) then
               Append (Result, Ix);
               for Edge of Container.Vertices.Element (Ix).Edges loop
                  declare
                     New_Cost : constant Cost_Type := Cost + Edge.Cost;
                  begin
                     if New_Cost <= Max then
                        Queue.Append ((Edge.To, New_Cost));
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;

   end Breadth_First_Search;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Container : in out Graph'Class;
      From, To  : in     Index_Type;
      Cost      : in     Cost_Type := Default_Cost)
   is
   begin
      Container.Vertices (From).Edges.Append ((To, Cost));
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Container : in out Graph'Class;
      From, To  : in     Vertex_Type;
      Cost      : in     Cost_Type := Default_Cost)
   is
   begin
      Container.Connect (Container.Index_Of (From),
                         Container.Index_Of (To),
                         Cost);
   end Connect;

   ---------------
   -- Connected --
   ---------------

   function Connected
     (Container    : Graph;
      From, To     : Index_Type)
      return Boolean
   is
   begin
      for Edge of Container.Vertices.Element (From).Edges loop
         if Edge.To = To then
            return True;
         end if;
      end loop;
      return False;
   end Connected;

   ---------------
   -- Connected --
   ---------------

   function Connected
     (Container    : Graph;
      From, To     : Vertex_Type)
      return Boolean
   is
   begin
      return Container.Connected (Container.Index_Of (From),
                                  Container.Index_Of (To));
   end Connected;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Graph;
      Vertex    : Vertex_Type)
      return Boolean
   is
   begin
      return Container.Index_Of (Vertex) in Index_Type'Range;
   end Contains;

   --------------
   -- Contains --
   --------------

   function Contains
     (Sub : Sub_Graph;
      Index : Index_Type)
      return Boolean
   is
   begin
      return Sub.Vertex_Flags.Element (Index);
   end Contains;

   --------------
   -- Contains --
   --------------

   function Contains
     (Collection : Sub_Graph_Collection;
      Index      : Index_Type)
      return Boolean
   is
   begin
      for Sub of Collection.List loop
         if Contains (Sub, Index) then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   ----------
   -- Cost --
   ----------

   function Cost (P : Path) return Cost_Type is
   begin
      return P.Cost;
   end Cost;

   ------------
   -- Create --
   ------------

   procedure Create
     (Container : Graph'Class;
      Sub       : out Sub_Graph)
   is
   begin
      Sub.Main_Graph := Container'Unchecked_Access;
      Sub.Vertex_List.Clear;
      Sub.Vertex_Flags.Clear;
      for I in 1 .. Container.Last_Vertex_Index loop
         Sub.Vertex_Flags.Append (False);
      end loop;
   end Create;

   ------------------------
   -- Depth_First_Search --
   ------------------------

   procedure Depth_First_Search
     (Container : Graph;
      Start     : Index_Type;
      Max       : Cost_Type;
      Result    : out Sub_Graph)
   is
      pragma Unreferenced (Max);
      Stack : Index_Lists.List;
   begin
      Container.Create (Result);
      Stack.Append (Start);
      while not Stack.Is_Empty loop
         declare
            Ix : constant Index_Type := Stack.First_Element;
         begin
            Stack.Delete_First;
            if not Contains (Result, Ix) then
               Append (Result, Ix);
               for Edge of Container.Vertices.Element (Ix).Edges loop
                  Stack.Insert (Stack.First, Edge.To);
               end loop;
            end if;
         end;
      end loop;

   end Depth_First_Search;

   ---------------
   -- Edge_Cost --
   ---------------

   function Edge_Cost
     (Container    : Graph;
      From, To     : in     Index_Type)
      return Cost_Type
   is
   begin
      for Edge of Container.Vertices.Element (From).Edges loop
         if Edge.To = To then
            return Edge.Cost;
         end if;
      end loop;
      --  can't happen because of precondition
      raise Program_Error with "invalid call to edge_cost";
   end Edge_Cost;

   ------------------------------
   -- Get_Connected_Components --
   ------------------------------

   procedure Get_Connected_Components
     (Container : Graph'Class;
      Result    : out Sub_Graph_Collection)
   is
   begin
      Result.List.Clear;
      for Index in 1 .. Container.Vertices.Last_Index loop
         if not Contains (Result, Index) then
            declare
               Sub : Sub_Graph;
            begin
               Container.Depth_First_Search (Index, Cost_Type'Last, Sub);
               Append (Result, Sub);
            end;
         end if;
      end loop;
   end Get_Connected_Components;

   --------------
   -- Index_Of --
   --------------

   function Index_Of
     (Container : Graph;
      Vertex    : Vertex_Type)
      return Extended_Index
   is
   begin
      for I in Index_Type'First .. Container.Vertices.Last_Index loop
         if Container.Vertices.Element (I).Vertex = Vertex then
            return I;
         end if;
      end loop;
      return Extended_Index'First;
   end Index_Of;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Collection : in out Sub_Graph_Collection;
      Sub        : Sub_Graph)
   is
   begin
      Collection.List.Append (Sub);
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Graph;
      Process   : not null access procedure (Position : Cursor))
   is
      procedure Local_Process (Position : Vertex_Info_Vectors.Cursor);

      -------------------
      -- Local_Process --
      -------------------

      procedure Local_Process (Position : Vertex_Info_Vectors.Cursor) is
      begin
         Process (Cursor (Position));
      end Local_Process;

   begin
      Container.Vertices.Iterate (Local_Process'Access);
   end Iterate;

   -------------------
   -- Iterate_Edges --
   -------------------

   procedure Iterate_Edges
     (Container : Graph;
      From      : Index_Type;
      Process   : not null access procedure (To : Index_Type;
                                             Cost : Cost_Type))
   is
   begin
      for Edge of Container.Vertices.Element (From).Edges loop
         Process (Edge.To, Edge.Cost);
      end loop;
   end Iterate_Edges;

   -----------------------
   -- Last_Vertex_Index --
   -----------------------

   function Last_Vertex_Index
     (Container : Graph)
      return Extended_Index
   is
   begin
      return Container.Vertices.Last_Index;
   end Last_Vertex_Index;

   ----------
   -- Next --
   ----------

   function Next (Container : Graph;
                  P         : Path)
                  return Vertex_Type
   is
   begin
      return Container.Vs (P.Edges.First_Element.To);
   end Next;

   --------------------
   -- Same_Sub_Graph --
   --------------------

   function Same_Sub_Graph
     (Collection : Sub_Graph_Collection;
      V1, V2     : Index_Type)
      return Boolean
   is
   begin
      for Sub_Graph of Collection.List loop
         if Contains (Sub_Graph, V1) then
            return Contains (Sub_Graph, V2);
         elsif Contains (Sub_Graph, V2) then
            return False;
         end if;
      end loop;
      return False;
   end Same_Sub_Graph;

   -------------------
   -- Shortest_Path --
   -------------------

   function Shortest_Path
     (Container : Graph'Class;
      From, To  : Index_Type)
      return Path
   is
      function OK (Vertex : Vertex_Type) return Boolean
      is (True);
   begin
      return Container.Shortest_Path (From, To, OK'Access);
   end Shortest_Path;

   -------------------
   -- Shortest_Path --
   -------------------

   function Shortest_Path
     (Container : Graph'Class;
      From, To  : Index_Type;
      Test_Vertex : not null access
        function (Vertex : Vertex_Type) return Boolean)
      return Path
   is
      package Queue_Of_Partials is
         new Ada.Containers.Doubly_Linked_Lists (Path);
      Queue : Queue_Of_Partials.List;
      Tried : Sub_Graph;
      Result : Path := (From, 0.0, Edge_Lists.Empty_List);
   begin
      Container.Create (Tried);
      Queue.Append ((From, 0.0, Edge_Lists.Empty_List));

      while not Queue.Is_Empty loop
         declare
            P    : constant Path := Queue.First_Element;
         begin
            Queue.Delete_First;
            if P.Start = To then
               declare
                  V      : Index_Type := P.Start;
               begin
                  for Edge of reverse P.Edges loop
                     Result.Edges.Append ((V, Edge.Cost));
                     V := Edge.To;
                  end loop;
                  Result.Start := V;
                  Result.Cost := P.Cost;
                  exit;
               end;
            end if;
            if not Contains (Tried, P.Start) then
               Append (Tried, P.Start);
               for Edge of Container.Vertices.Element (P.Start).Edges loop
                  if Test_Vertex (Container.Vs (Edge.To)) then
                     declare
                        New_Path : Path :=
                                     (Edge.To, P.Cost + Edge.Cost, P.Edges);
                     begin
                        New_Path.Edges.Append ((P.Start, Edge.Cost));
                        Queue.Append (New_Path);
                     end;
                  end if;
               end loop;
            end if;
         end;
      end loop;

      return Result;

   end Shortest_Path;

   ---------------------
   -- Sub_Graph_Count --
   ---------------------

   function Sub_Graph_Count
     (Collection : Sub_Graph_Collection)
      return Natural
   is
   begin
      return Natural (Collection.List.Length);
   end Sub_Graph_Count;

   ------------
   -- Vertex --
   ------------

   function Vertex
     (Container : Graph;
      Index     : Index_Type)
      return Vertex_Type
   is
   begin
      return Container.Vs (Index);
   end Vertex;

   ------------------
   -- Vertex_Count --
   ------------------

   function Vertex_Count (P : Path) return Index_Type is
   begin
      return Extended_Index (P.Edges.Length) + 1;
   end Vertex_Count;

end Concorde.Graphs;
