with WL.Localisation;

with Concorde.Powers.Execution;

package body Concorde.Powers is

   ---------------
   -- Add_Power --
   ---------------

   overriding procedure Add_Power
     (Container : in out Power_Set;
      Power     : Power_Type)
   is
   begin
      Container.Set.Append (Power);
   end Add_Power;

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Power : Root_Power_Type'Class;
      Index : Positive)
      return Concorde.People.Attributes.Attribute_Reference
   is
   begin
      return Concorde.Powers.Execution.Attribute (Power, Index);
   end Attribute;

   ---------------------
   -- Attribute_Count --
   ---------------------

   function Attribute_Count
     (Power : Root_Power_Type'Class)
      return Natural
   is
   begin
      return Concorde.Powers.Execution.Attribute_Count (Power);
   end Attribute_Count;

   ------------------
   -- Check_Powers --
   ------------------

   overriding function Check_Powers
     (Container : Power_Set;
      Test      : not null access
        function (Power : Power_Type) return Boolean)
      return Boolean
   is
   begin
      return (for all Power of Container.Set => Test (Power));
   end Check_Powers;

   ---------------------
   -- Daily_Execution --
   ---------------------

   function Daily_Execution
     (Power     : Root_Power_Type)
      return Boolean
   is
      pragma Unreferenced (Power);
   begin
      return False;
   end Daily_Execution;

   ----------------
   -- Daily_Work --
   ----------------

   function Daily_Work
     (Power : Root_Power_Type'Class;
      Community : not null access constant
        Concorde.People.Communities.Root_Community_Type'Class)
      return Duration
   is
   begin
      return Concorde.Powers.Execution.Daily_Work
        (Power, Community);
   end Daily_Work;

   --------------------
   -- Execution_Work --
   --------------------

   function Execution_Work
     (Power  : Root_Power_Type'Class;
      Target : access constant
        Concorde.Objects.Root_Object_Type'Class)
      return Duration
   is
   begin
      return Concorde.Powers.Execution.Execution_Work (Power, Target);
   end Execution_Work;

   ---------------
   -- Has_Power --
   ---------------

   overriding function Has_Power
     (Container : Power_Set;
      Power     : Power_Type)
      return Boolean
   is
   begin
      return Container.Set.Contains (Power);
   end Has_Power;

   ---------------
   -- Pop_Group --
   ---------------

   function Pop_Group
     (Power : Root_Power_Type'Class;
      Index : Positive)
      return not null access constant
     Concorde.People.Groups.Root_Pop_Group'Class
   is
   begin
      return Concorde.Powers.Execution.Pop_Group (Power, Index);
   end Pop_Group;

   ---------------------
   -- Pop_Group_Count --
   ---------------------

   function Pop_Group_Count
     (Power : Root_Power_Type'Class)
      return Natural
   is
   begin
      return Concorde.Powers.Execution.Pop_Group_Count (Power);
   end Pop_Group_Count;

   ----------------------
   -- Pop_Group_Effect --
   ----------------------

   function Pop_Group_Effect
     (Power : Root_Power_Type'Class;
      Index : Positive)
      return Duration
   is
   begin
      return Concorde.Powers.Execution.Pop_Group_Effect (Power, Index);
   end Pop_Group_Effect;

   ------------------
   -- Remove_Power --
   ------------------

   overriding procedure Remove_Power
     (Container : in out Power_Set;
      Power     : Power_Type)
   is
      Position : Power_Lists.Cursor := Container.Set.Find (Power);
   begin
      if not Power_Lists.Has_Element (Position) then
         raise Constraint_Error with
           "power not present in list: " & Power.Show;
      end if;
      Container.Set.Delete (Position);
   end Remove_Power;

   -----------------
   -- Scan_Powers --
   -----------------

   overriding procedure Scan_Powers
     (Container : Power_Set;
      Process   : not null access
        procedure (Power : Power_Type))
   is
   begin
      for Power of Container.Set loop
         Process (Power);
      end loop;
   end Scan_Powers;

   ----------
   -- Show --
   ----------

   function Show (Power : Root_Power_Type) return String is
   begin
      return WL.Localisation.Local_Text
        (Root_Power_Type'Class (Power).Class_Identifier);
   end Show;

end Concorde.Powers;
