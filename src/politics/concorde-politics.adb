with Concorde.Random;

package body Concorde.Politics is

   ---------------
   -- Add_Noise --
   ---------------

   procedure Add_Noise
     (Rec     : in out Political_Record'Class;
      Std_Dev : Non_Negative_Real)
   is

      function F (X : Unit_Real) return Unit_Real
      is (Unit_Clamp (X + Concorde.Random.Normal_Random (Std_Dev)));

   begin
      for Item of Rec.Axis loop
         Item.Position := F (Item.Position);
         Item.Strength := F (Item.Strength);
      end loop;
   end Add_Noise;

   ------------
   -- Choose --
   ------------

   function Choose
     (Political : Political_Interface'Class;
      Random    : Unit_Real;
      Count     : Positive;
      Get       : not null access
        function (Index : Positive) return Political_Interface'Class)
      return Positive
   is
      Total : Non_Negative_Real := 0.0;
      Match : array (1 .. Count) of Unit_Real;
   begin
      for I in 1 .. Count loop
         Match (I) := Political.Match (Get (I));
         Total := Total + Match (I);
      end loop;

      declare
         Pick   : Non_Negative_Real := Random * Total;
         Result : Natural := 0;
      begin
         for I in 1 .. Count loop
            if Pick < Match (I) then
               Result := I;
               exit;
            end if;
            Pick := Pick - Match (I);
         end loop;
         return Result;
      end;
   end Choose;

   ----------
   -- Join --
   ----------

   function Join
     (Left, Right : Political_Interface'Class)
      return Political_Record
   is
      Result : Political_Record;
   begin
      Result.Join (Left);
      Result.Join (Right);
      return Result;
   end Join;

   ----------
   -- Join --
   ----------

   procedure Join
     (Left  : in out Political_Record'Class;
      Right : Political_Interface'Class)
   is
   begin
      for Axis in Political_Axis loop
         declare
            L_Strength : constant Unit_Real := Left.Strength (Axis);
            R_Strength : constant Unit_Real := Right.Strength (Axis);
            L_Position : constant Unit_Real := Left.Position (Axis);
            R_Position : constant Unit_Real := Right.Position (Axis);
            Position   : constant Unit_Real :=
                           (if L_Strength > 0.0 or else R_Strength > 0.0
                            then (L_Position * L_Strength
                              + R_Position * R_Strength)
                            / (L_Strength + R_Strength)
                            else 0.0);
            Strength   : constant Unit_Real :=
                           (if L_Strength = 0.0 then R_Strength
                            elsif R_Strength = 0.0 then L_Strength
                            else (L_Strength + R_Strength) / 2.0);
         begin
            Left.Axis (Axis) := (Position, Strength);
         end;
      end loop;
   end Join;

   -----------
   -- Match --
   -----------

   function Match
     (Political : Political_Interface'Class;
      Other     : Political_Interface'Class)
      return Unit_Real
   is
      Total : Non_Negative_Real := 0.0;
   begin
      for Axis in Political_Axis loop
         declare
            D : constant Unit_Real :=
                  abs (Political.Position (Axis)
                       - Other.Position (Axis));
         begin
            Total := Total
              + D * Political.Strength (Axis) * Other.Strength (Axis);
         end;
      end loop;
      if Total > 1.0 then
         return 0.0;
      else
         return 1.0 - Total;
      end if;
   end Match;

   --------------
   -- Set_Axis --
   --------------

   procedure Set_Axis
     (Rec      : in out Political_Record'Class;
      Axis     : Political_Axis;
      Position : Unit_Real;
      Strength : Unit_Real)
   is
   begin
      Rec.Axis (Axis) := (Position, Strength);
   end Set_Axis;

end Concorde.Politics;
