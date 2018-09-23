with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Concorde.Objects;
with Concorde.People.Attributes;

limited with Concorde.People.Communities;
limited with Concorde.People.Groups;

package Concorde.Powers is

   type Root_Power_Type is abstract tagged private;

   subtype Power_Type is Root_Power_Type'Class;

   function Class_Identifier (Power : Root_Power_Type) return String
                              is abstract;

   function Identifier (Power : Root_Power_Type) return String;

   function Show (Power : Root_Power_Type) return String;

   function Daily_Execution
     (Power     : Root_Power_Type)
      return Boolean;

   function Daily_Work
     (Power     : Root_Power_Type'Class;
      Community : not null access constant
        Concorde.People.Communities.Root_Community_Type'Class)
      return Duration;
   --  How many person-seconds required
   --  to execute this power in the given community today

   function Execution_Work
     (Power  : Root_Power_Type'Class;
      Target : access constant
        Concorde.Objects.Root_Object_Type'Class)
      return Duration;
   --  How many person-seconds required
   --  to execute this power on the given target

   function Pop_Group_Count
     (Power : Root_Power_Type'Class)
      return Natural;
   --  Number of pop group categories that can execute this power

   function Pop_Group
     (Power : Root_Power_Type'Class;
      Index : Positive)
      return not null access constant
     Concorde.People.Groups.Root_Pop_Group'Class
       with Pre => Pop_Group_Count (Power) >= Index;

   function Pop_Group_Effect
     (Power : Root_Power_Type'Class;
      Index : Positive)
      return Duration
     with Pre => Pop_Group_Count (Power) >= Index;
   --  Number of seconds per day contributed by each
   --  person in this pop group

   function Attribute_Count
     (Power : Root_Power_Type'Class)
      return Natural;

   function Attribute
     (Power : Root_Power_Type'Class;
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

   type Powered_Interface is limited interface;

   function Has_Power
     (Container : Powered_Interface;
      Power     : Power_Type)
      return Boolean
      is abstract;

   procedure Add_Power
     (Container : in out Powered_Interface;
      Power     : Power_Type)
   is abstract;

   procedure Remove_Power
     (Container : in out Powered_Interface;
      Power     : Power_Type)
   is abstract;

   procedure Scan_Powers
     (Container : Powered_Interface;
      Process   : not null access
        procedure (Power : Power_Type))
   is abstract;

   function Check_Powers
     (Container : Powered_Interface;
      Test      : not null access
        function (Power : Power_Type) return Boolean)
      return Boolean
      is abstract;

   type Power_Set is new Powered_Interface with private;

   function No_Powers return Power_Set;

   overriding function Has_Power
     (Container : Power_Set;
      Power     : Power_Type)
      return Boolean;

   overriding procedure Add_Power
     (Container : in out Power_Set;
      Power     : Power_Type);

   overriding procedure Remove_Power
     (Container : in out Power_Set;
      Power     : Power_Type);

   overriding function Check_Powers
     (Container : Power_Set;
      Test      : not null access
        function (Power : Power_Type) return Boolean)
      return Boolean;

   overriding procedure Scan_Powers
     (Container : Power_Set;
      Process   : not null access
        procedure (Power : Power_Type));

private

   type Root_Power_Type is abstract tagged
      record
         null;
      end record;

   function Identifier (Power : Root_Power_Type) return String
   is (Root_Power_Type'Class (Power).Class_Identifier);

   package Power_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Power_Type);

   type Power_Set is new Powered_Interface with
      record
         Set : Power_Lists.List;
      end record;

   function No_Powers return Power_Set is (Set => <>);

end Concorde.Powers;
