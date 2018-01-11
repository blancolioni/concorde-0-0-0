with Ada.Containers.Indefinite_Vectors;

with WL.Random;
with WL.String_Maps;

with Concorde.Random;

package body Concorde.People.Genetics is

   Empty_Gene : Gene_Type (0);

   function Random_Base_Value return Base_Value
   is (Base_Value (WL.Random.Random_Number (0, Natural (Base_Value'Last))));

   package Gene_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Gene_Type);

   package Gene_Maps is
     new WL.String_Maps (Gene_Type);

   Gene_Vector : Gene_Vectors.Vector;
   Gene_Map    : Gene_Maps.Map;

   ---------------------
   -- Configure_Genes --
   ---------------------

   procedure Configure_Genes
     (Gene_Config : Tropos.Configuration)
   is

      function Configure_Expression
        (Index       : Positive;
         Expr_Config : Tropos.Configuration)
         return Expressed_Base;

      --------------------------
      -- Configure_Expression --
      --------------------------

      function Configure_Expression
        (Index       : Positive;
         Expr_Config : Tropos.Configuration)
         return Expressed_Base
      is
         Name : constant String := Expr_Config.Config_Name;
      begin
         return (Base_Index (Index), Expressed_Base_Operation'Value (Name));
      end Configure_Expression;

   begin
      for Config of Gene_Config loop
         declare
            Id     : constant Positive := Config.Get ("id");
            Name   : constant String   := Config.Get ("name");
            Start  : constant Positive := Config.Get ("start");
            Coding : constant Tropos.Configuration :=
                       Config.Child ("coding");
            Count  : constant Natural := Coding.Child_Count;
            Gene   : Gene_Type (Count);
         begin

            for I in 1 .. Count loop
               Gene.Expressed_Bases (I) :=
                 Configure_Expression (Start + I - 1, Coding.Child (I));
            end loop;

            while Gene_Vector.Last_Index < Id loop
               Gene_Vector.Append (Empty_Gene);
            end loop;
            Gene_Vector.Replace_Element (Id, Gene);
            Gene_Map.Insert (Name, Gene);
         end;
      end loop;
   end Configure_Genes;

   -------------
   -- Express --
   -------------

   function Express
     (From_Genome : Genome;
      Gene        : Gene_Type)
      return Gene_Expression
   is
      Acc   : Natural := 0;
      Count : Natural := 0;
   begin
      for Expr of Gene.Expressed_Bases loop
         declare
            X : constant Base_Value := From_Genome.Left (Expr.Base);
            Y : constant Base_Value := From_Genome.Right (Expr.Base);
            Z : constant Base_Value :=
                  (case Expr.Operation is
                      when Highest => Base_Value'Max (X, Y),
                      when Lowest  => Base_Value'Min (X, Y),
                      when Average | Open_Average =>
                         Base_Value ((Natural (X) + Natural (Y)) / 2),
                      when Left    => X,
                      when Right   => Y);
         begin
            Acc := Acc + Natural (Z);
            Count := Count + 1;
            if Expr.Operation = Open_Average
              and then Z < Base_Value'Last
            then
               exit;
            end if;
         end;
      end loop;

      return Gene_Expression (Acc * 16 / Gene.Expressed_Base_Count / 4 + 1);

   end Express;

   --------------
   -- Get_Gene --
   --------------

   function Get_Gene (Id : Positive) return Gene_Type is
   begin
      if Gene_Vector (Id).Expressed_Base_Count = 0 then
         raise Constraint_Error with
           "no gene for id" & Id'Img;
      end if;
      return Gene_Vector.Element (Id);
   end Get_Gene;

   --------------
   -- Get_Gene --
   --------------

   function Get_Gene (Name : String) return Gene_Type is
   begin
      if not Gene_Map.Contains (Name) then
         raise Constraint_Error with
           "no such gene: " & Name;
      end if;
      return Gene_Map.Element (Name);
   end Get_Gene;

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
         Remaining : Natural := 0;
         Use_X     : Boolean := True;
      begin
         return Z : Array_Of_Bases do
            for I in Z'Range loop
               if Remaining = 0 then
                  Remaining := 4;
                  Use_X := WL.Random.Random_Number (1, 2) = 1;
               end if;

               Remaining := Remaining - 1;
               if Concorde.Random.Unit_Random < Error_Rate then
                  Z (I) := Random_Base_Value;
               elsif Use_X then
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
