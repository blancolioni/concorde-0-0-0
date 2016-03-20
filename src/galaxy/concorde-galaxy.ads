private with Ada.Containers.Vectors;

with Concorde.Systems.Graphs;

package Concorde.Galaxy is

   function Find_System
     (OK : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Boolean)
      return Concorde.Systems.Star_System_Type;

   procedure Update_System
     (System : Concorde.Systems.Star_System_Type;
      Update : not null access
        procedure (System : in out Systems.Root_Star_System_Type'Class));

--     procedure Move_Fleets
--       (From, To : Concorde.Systems.Star_System_Type;
--        Count    : Natural);

   type Array_Of_Star_Systems is
     array (Positive range <>) of Concorde.Systems.Star_System_Type;

   function Get_Systems
     (OK : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Boolean)
      return Array_Of_Star_Systems;

   function Neighbours
     (System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Array_Of_Star_Systems;

   function Neighbours
     (System_Index : Positive)
      return Array_Of_Star_Systems;

   function Neighbours
     (System_1, System_2 : Concorde.Systems.Star_System_Type)
      return Boolean;

   function Shortest_Path_Distance
     (System_1, System_2 : Concorde.Systems.Star_System_Type)
      return Non_Negative_Real;

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

private

   package Star_System_Vectors is
     new Ada.Containers.Vectors
       (Positive, Concorde.Systems.Star_System_Access,
        Concorde.Systems."=");

   Galaxy_Vector : Star_System_Vectors.Vector;
   Galaxy_Graph : Concorde.Systems.Graphs.Graph;

   type Star_System_Set is
      record
         Collection : Concorde.Systems.Graphs.Sub_Graph_Collection;
      end record;

end Concorde.Galaxy;
