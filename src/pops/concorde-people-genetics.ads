package Concorde.People.Genetics is

   type Genome is private;

   function Merge
     (A, B       : Genome;
      Error_Rate : Unit_Real)
      return Genome;

   function Random_Genome return Genome;

   type Gene_Expression is range 1 .. 16;

   type Gene_Type (<>) is private;

   function Express
     (From_Genome : Genome;
      Gene        : Gene_Type)
      return Gene_Expression;

   Cheeks : aliased constant Gene_Type;
   Chin   : aliased constant Gene_Type;
   Ears   : aliased constant Gene_Type;
   Eyes   : aliased constant Gene_Type;
   Mouth  : aliased constant Gene_Type;
   Neck   : aliased constant Gene_Type;
   Nose   : aliased constant Gene_Type;

private

   type Base_Value is range 0 .. 3;

   Number_Of_Bases : constant := 30;

   type Base_Count is range 0 .. Number_Of_Bases;
   subtype Base_Index is Base_Count range 1 .. Base_Count'Last;

   type Array_Of_Bases is array (Base_Index) of Base_Value;

   type Genome is
      record
         Left, Right : Array_Of_Bases;
      end record;

   type Expressed_Base_Operation is (Highest, Lowest, Average, Left, Right);

   type Expressed_Base is
      record
         Base      : Base_Index;
         Operation : Expressed_Base_Operation;
      end record;

   type Expressed_Base_Array is array (Positive range <>) of Expressed_Base;

   type Gene_Type (Expressed_Base_Count : Positive) is
      record
         Expressed_Bases : Expressed_Base_Array (1 .. Expressed_Base_Count);
      end record;

   Cheeks : aliased constant Gene_Type :=
              Gene_Type'
                (Expressed_Base_Count => 4,
                 Expressed_Bases      =>
                   ((1, Left), (2, Right), (3, Left), (4, Right)));

   Chin   : aliased constant Gene_Type :=
              Gene_Type'
                (Expressed_Base_Count => 4,
                 Expressed_Bases      =>
                   ((5, Left), (6, Right), (7, Left), (8, Right)));

   Ears   : aliased constant Gene_Type :=
              Gene_Type'
                (Expressed_Base_Count => 4,
                 Expressed_Bases      =>
                   ((9, Left), (10, Right), (11, Left), (12, Right)));

   Eyes   : aliased constant Gene_Type :=
              Gene_Type'
                (Expressed_Base_Count => 4,
                 Expressed_Bases      =>
                   ((1, Left), (2, Right), (3, Left), (4, Right)));

   Mouth  : aliased constant Gene_Type :=
              Gene_Type'
                (Expressed_Base_Count => 4,
                 Expressed_Bases      =>
                   ((13, Left), (14, Right), (15, Left), (16, Right)));

   Neck   : aliased constant Gene_Type :=
              Gene_Type'
                (Expressed_Base_Count => 4,
                 Expressed_Bases      =>
                   ((17, Left), (18, Right), (19, Left), (20, Right)));

   Nose   : aliased constant Gene_Type :=
              Gene_Type'
                (Expressed_Base_Count => 4,
                 Expressed_Bases      =>
                   ((21, Left), (22, Right), (23, Left), (24, Right)));

end Concorde.People.Genetics;
