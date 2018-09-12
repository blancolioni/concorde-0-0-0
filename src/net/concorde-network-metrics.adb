with Concorde.Network.State;

package body Concorde.Network.Metrics is

   type Money_Metric_Type is new Root_Metric_Type with null record;

   overriding function Create_State
     (Metric        : not null access constant Money_Metric_Type;
      Initial_Value : Real)
      return Concorde.Network.Node_State_Access;

   type Rating_Metric_Type is new Root_Metric_Type with null record;

   overriding function Create_State
     (Metric        : not null access constant Rating_Metric_Type;
      Initial_Value : Real)
      return Concorde.Network.Node_State_Access;

   type Root_Metric_State_Type is
     abstract new Concorde.Network.State.Root_Node_State_Type with
      record
         Actual_Value : Real := 0.0;
         Base_Value   : Real := 0.0;
      end record;

   type Root_Money_Metric_State_Type is
     new Root_Metric_State_Type with null record;

   type Root_Rating_Metric_State_Type is
     new Root_Metric_State_Type with null record;

   ------------------
   -- Create_State --
   ------------------

   overriding function Create_State
     (Metric        : not null access constant Money_Metric_Type;
      Initial_Value : Real)
      return Concorde.Network.Node_State_Access
   is
      State : Root_Money_Metric_State_Type;
   begin
      State.Initialize_State (Metric, Initial_Value);
      return new Root_Money_Metric_State_Type'(State);
   end Create_State;

   ------------------
   -- Create_State --
   ------------------

   overriding function Create_State
     (Metric        : not null access constant Rating_Metric_Type;
      Initial_Value : Real)
      return Concorde.Network.Node_State_Access
   is
      State : Root_Rating_Metric_State_Type;
   begin
      State.Initialize_State (Metric, Initial_Value);
      return new Root_Rating_Metric_State_Type'(State);
   end Create_State;

   ----------------------
   -- New_Money_Metric --
   ----------------------

   function New_Money_Metric
     (Id : String)
      return Concorde.Network.Nodes.Node_Type
   is
      Metric : Money_Metric_Type;
   begin
      Metric.Initialise (Id);
      return new Money_Metric_Type'(Metric);
   end New_Money_Metric;

   -----------------------
   -- New_Rating_Metric --
   -----------------------

   function New_Rating_Metric
     (Id : String)
      return Concorde.Network.Nodes.Node_Type
   is
      Metric : Rating_Metric_Type;
   begin
      Metric.Initialise (Id);
      return new Rating_Metric_Type'(Metric);
   end New_Rating_Metric;

end Concorde.Network.Metrics;
