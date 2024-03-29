with Tropos;

package Concorde.People.Genetics is

   type Genome is private;

   function Merge
     (A, B       : Genome;
      Error_Rate : Unit_Real)
      return Genome;

   function Random_Genome return Genome;

   type Gene_Expression is range 1 .. 16;

   type Gene_Type (<>) is private;

   function Get_Gene (Id : Positive) return Gene_Type;
   function Get_Gene (Name : String) return Gene_Type;

   function Express
     (From_Genome : Genome;
      Gene        : Gene_Type)
      return Gene_Expression;

   procedure Configure_Genes
     (Gene_Config : Tropos.Configuration);

private

   type Base_Value is range 0 .. 3;

   Number_Of_Bases : constant := 80;

   type Base_Count is range 0 .. Number_Of_Bases;
   subtype Base_Index is Base_Count range 1 .. Base_Count'Last;

   type Array_Of_Bases is array (Base_Index) of Base_Value;

   type Genome is
      record
         Left, Right : Array_Of_Bases;
      end record;

   type Expressed_Base_Operation is
     (Highest, Lowest, Average, Left, Right, Open_Average);

   type Expressed_Base is
      record
         Base      : Base_Index;
         Operation : Expressed_Base_Operation;
      end record;

   type Expressed_Base_Array is array (Positive range <>) of Expressed_Base;

   type Gene_Type (Expressed_Base_Count : Natural) is
      record
         Expressed_Bases : Expressed_Base_Array (1 .. Expressed_Base_Count);
      end record;

end Concorde.People.Genetics;
