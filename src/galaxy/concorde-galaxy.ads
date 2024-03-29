with Concorde.Systems.Graphs;

with Concorde.Worlds;

package Concorde.Galaxy is

   function Find_System
     (OK : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Boolean)
      return Concorde.Systems.Star_System_Type;

   type Array_Of_Star_Systems is
     array (Positive range <>) of Concorde.Systems.Star_System_Type;

   function Get_Systems
     (OK : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Boolean)
      return Array_Of_Star_Systems;

   function Minimum
     (Score : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Natural)
      return Concorde.Systems.Star_System_Type;
   --  find the system which minimises Score.
   --  return null if no system scores less than Natural'Last

   function Maximum
     (Score : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Natural)
      return Concorde.Systems.Star_System_Type;
   --  find the system which maximises Score.
   --  return null if no system scores more than zero

   function Neighbours
     (System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Array_Of_Star_Systems;

   function Neighbours
     (System : Concorde.Systems.Root_Star_System_Type'Class)
      return Array_Of_Star_Systems;

   function Neighbours
     (System_Index : Positive)
      return Array_Of_Star_Systems;

   function Neighbours
     (System_1, System_2 : Concorde.Systems.Star_System_Type)
      return Boolean;

   function Neighbours
     (System_1, System_2 : Positive)
      return Boolean;

   procedure Connect
     (System_1, System_2 : Positive);

   function Shortest_Path_Distance
     (System_1, System_2 : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Non_Negative_Real;

   function Shortest_Path
     (From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      OK       : access function (System : Concorde.Systems.Star_System_Type)
      return Boolean := null)
      return Concorde.Systems.Graphs.Array_Of_Vertices;

   procedure Iterate
     (Filter : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Boolean;
      Process : not null access
        procedure (System : Concorde.Systems.Star_System_Type));

   function System_Count return Natural;

   function Get_System
     (Index : Positive)
      return Concorde.Systems.Star_System_Type;

   type Star_System_Set is private;

   function Is_Element
     (Of_Set : Star_System_Set;
      System : Concorde.Systems.Star_System_Type)
      return Boolean;

   procedure Add_Systems
     (To           : in out Star_System_Set;
      Start        : Concorde.Systems.Star_System_Type;
      Max_Distance : Non_Negative_Real);

   function Capital_World return Concorde.Worlds.World_Type;

   procedure Set_Capital_World
     (World : Concorde.Worlds.World_Type);

private

   Galaxy_Graph   : Concorde.Systems.Graphs.Graph;

   type Star_System_Set is
      record
         Collection : Concorde.Systems.Graphs.Sub_Graph_Collection;
      end record;

end Concorde.Galaxy;
