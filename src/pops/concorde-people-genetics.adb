with WL.Random;
with Concorde.Random;

package body Concorde.People.Genetics is

   function Random_Base_Value return Base_Value
   is (Base_Value (WL.Random.Random_Number (0, Natural (Base_Value'Last))));

   -------------
   -- Express --
   -------------

   function Express
     (From_Genome : Genome;
      Gene        : Gene_Type)
      return Gene_Expression
   is
      Acc : Natural := 0;
   begin
      for Expr of Gene.Expressed_Bases loop
         declare
            X : constant Base_Value := From_Genome.Left (Expr.Base);
            Y : constant Base_Value := From_Genome.Right (Expr.Base);
            Z : constant Base_Value :=
                  (case Expr.Operation is
                      when Highest => Base_Value'Max (X, Y),
                      when Lowest  => Base_Value'Min (X, Y),
                      when Average =>
                         Base_Value ((Natural (X) + Natural (Y)) / 2));
         begin
            Acc := Acc + Natural (Z);
         end;
      end loop;

      return Gene_Expression (Acc * 16 / Gene.Expressed_Base_Count / 4 + 1);

   end Express;

   -----------
   -- Merge --
   -----------

   function Merge
     (A, B       : Genome;
      Error_Rate : Unit_Real)
      return Genome
   is
      function Merge (X, Y : Array_Of_Bases) return Array_Of_Bases;

      -----------
      -- Merge --
      -----------

      function Merge (X, Y : Array_Of_Bases) return Array_Of_Bases is
      begin
         return Z : Array_Of_Bases do
            for I in Z'Range loop
               if Concorde.Random.Unit_Random < Error_Rate then
                  Z (I) := Random_Base_Value;
               elsif WL.Random.Random_Number (1, 2) = 1 then
                  Z (I) := X (I);
               else
                  Z (I) := Y (I);
               end if;
            end loop;
         end return;
      end Merge;

   begin
      return (Merge (A.Left, B.Left), Merge (A.Right, B.Right));
   end Merge;

   -------------------
   -- Random_Genome --
   -------------------

   function Random_Genome return Genome is
      function Random_Bases return Array_Of_Bases;

      ------------------
      -- Random_Bases --
      ------------------

      function Random_Bases return Array_Of_Bases is
      begin
         return Result : Array_Of_Bases do
            for X of Result loop
               X := Random_Base_Value;
            end loop;
         end return;
      end Random_Bases;

   begin
      return Result : Genome do
         Result.Left := Random_Bases;
         Result.Right := Random_Bases;
      end return;
   end Random_Genome;

end Concorde.People.Genetics;
