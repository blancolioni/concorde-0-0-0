with Tropos;

with Concorde.Objects;
with Concorde.People.Attributes;
with Concorde.People.Groups;
with Concorde.Worlds;

private package Concorde.Powers.Execution is

   function Daily_Work
     (Power : Power_Type;
      World : not null access constant Concorde.Worlds.Root_World_Type'Class)
      return Duration;
   --  How many person-seconds required
   --  to execute this power on the given world today

   function Execution_Work
     (Power  : Power_Type;
      Target : access constant
        Concorde.Objects.Root_Object_Type'Class)
      return Duration;
   --  How many person-seconds required
   --  to execute this power on the given target

   function Pop_Group_Count
     (Power : Power_Type)
      return Natural;
   --  Number of pop group categories that can execute this power

   function Pop_Group
     (Power : Power_Type;
      Index : Positive)
      return Concorde.People.Groups.Pop_Group
     with Pre => Pop_Group_Count (Power) >= Index,
     Post => Concorde.People.Groups."/=" (Pop_Group'Result, null);

   function Pop_Group_Effect
     (Power : Power_Type;
      Index : Positive)
      return Duration
     with Pre => Pop_Group_Count (Power) >= Index;
   --  Number of seconds per day contributed by each
   --  person in this pop group

   function Attribute_Count
     (Power : Power_Type)
      return Natural;

   function Attribute
     (Power : Power_Type;
      Index : Positive)
      return Concorde.People.Attributes.Attribute_Reference
     with Pre => Attribute_Count (Power) >= Index;
   --  Attributes reduce the daily work required to execute this power
   --  They are contributed by individuals executing the power.
   --  Effect is reduced for each degree of separation from execution.
   --  E.g. an individual directly executing a power gets full effect
   --  Individual who manages a ministry which executes the power gets half
   --  That individual's boss gets less, depending on how much time they
   --  can contribute

   procedure Configure_Power_Execution
     (Config : Tropos.Configuration);

end Concorde.Powers.Execution;
