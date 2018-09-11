private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Memor.Database;

with Memor;

with Concorde.Commodities;

with Concorde.Network.Expressions;
with Concorde.Network.Nodes;
with Concorde.Politics;

package Concorde.People.Groups is

   type Root_Pop_Group is
     new Concorde.Network.Nodes.Root_Node_Type
     and Concorde.Politics.Political_Interface
   with private;

   type Pop_Group is access constant Root_Pop_Group'Class;

   type Array_Of_Pop_Groups is
     array (Positive range <>) of Pop_Group;

   function All_Groups return Array_Of_Pop_Groups;
   function Wealth_Groups return Array_Of_Pop_Groups;
   function Political_Groups return Array_Of_Pop_Groups;
   function Everybody return Pop_Group;

   function Is_Political_Left
     (Group : Root_Pop_Group'Class)
      return Boolean;

   function Is_Political_Right
     (Group : Root_Pop_Group'Class)
      return Boolean;

   function Has_Wealth_Proportion
     (Group : Root_Pop_Group'Class)
      return Boolean;

   function Wealth_Proportion
     (Group : Root_Pop_Group'Class)
      return Concorde.Network.Expressions.Expression_Type
     with Pre => Group.Has_Wealth_Proportion;

   function Default_Proportion
     (Group : Root_Pop_Group'Class)
      return Unit_Real;

   function Influence
     (Group : Root_Pop_Group'Class;
      Other : Pop_Group)
      return Signed_Unit_Real;

   function Get (Name : String) return Pop_Group;

private

   type Group_Influence is
      record
         Group     : Pop_Group;
         Influence : Signed_Unit_Real;
      end record;

   package Group_Influence_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Group_Influence);

   type Root_Pop_Group is
     new Concorde.Network.Nodes.Root_Node_Type
     and Concorde.Politics.Political_Interface with
      record
         Default_Politics      : Concorde.Politics.Political_Record;
         Default_Proportion    : Unit_Real := 0.0;
         Expression_Proportion : Concorde.Network.Expressions.Expression_Type;
         Political_Wing        : Boolean := False;
         Left_Bias             : Boolean := False;
         Wealth_Group          : Boolean := False;
         Wealth_Proportion     : Boolean := False;
         Influences            : Group_Influence_Lists.List;
      end record;

   overriding function Object_Database
     (Item : Root_Pop_Group)
      return Memor.Memor_Database;

   overriding function Position
     (Group : Root_Pop_Group;
      Axis  : Concorde.Politics.Political_Axis)
      return Unit_Real
   is (Group.Default_Politics.Position (Axis));

   overriding function Strength
     (Group : Root_Pop_Group;
      Axis  : Concorde.Politics.Political_Axis)
      return Unit_Real
   is (Group.Default_Politics.Strength (Axis));

   function Is_Wealth_Group
     (Group : Root_Pop_Group'Class)
      return Boolean
   is (Group.Wealth_Group);

   function Default_Proportion
     (Group : Root_Pop_Group'Class)
      return Unit_Real
   is (Group.Default_Proportion);

   function Has_Wealth_Proportion
     (Group : Root_Pop_Group'Class)
      return Boolean
   is (Group.Wealth_Proportion);

   function Wealth_Proportion
     (Group : Root_Pop_Group'Class)
      return Concorde.Network.Expressions.Expression_Type
   is (Group.Expression_Proportion);

   function Is_Political_Left
     (Group : Root_Pop_Group'Class)
      return Boolean
   is (Group.Political_Wing and then Group.Left_Bias);

   function Is_Political_Right
     (Group : Root_Pop_Group'Class)
      return Boolean
   is (Group.Political_Wing and then not Group.Left_Bias);

   package Db is
     new Memor.Database
       ("pop-group", Root_Pop_Group, Pop_Group);

   overriding function Object_Database
     (Item : Root_Pop_Group)
      return Memor.Memor_Database
   is (Db.Get_Database);

   package Pop_Group_Vectors is
     new Ada.Containers.Vectors (Positive, Pop_Group);

   Vector : Pop_Group_Vectors.Vector;

end Concorde.People.Groups;
