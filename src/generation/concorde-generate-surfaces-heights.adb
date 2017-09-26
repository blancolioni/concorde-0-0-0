with Ada.Unchecked_Deallocation;

with WL.Random;

package body Concorde.Generate.Surfaces.Heights is

   -------------------------
   -- Create_Height_Array --
   -------------------------

   procedure Create_Height_Array
     (Heights           : out Surface_Array;
      Smoothing         :     Natural;
      Min, Max          : Integer;
      Random            : Boolean;
      Initial_Heights   : access
        function (X, Y : Positive) return Real)
   is
      type Work_Array is
        array (Boolean, Heights'Range (1), Heights'Range (2)) of Integer;
      type Work_Array_Access is access Work_Array;
      procedure Free is
        new Ada.Unchecked_Deallocation (Work_Array, Work_Array_Access);
      Work : Work_Array_Access := new Work_Array;
      Current         : Boolean := False;
   begin
      for X in Heights'Range (1) loop
         for Y in Heights'Range (2) loop
            if Initial_Heights = null then
               Work (Current, X, Y) :=
                 WL.Random.Random_Number (Min, Max);
            else
               declare
                  D : constant Real :=
                        Initial_Heights (X, Y);
               begin
                  if Random then
                     declare
                        Init : constant Unit_Real :=
                                 Initial_Heights (X, Y);
                        R    : constant Natural :=
                                 WL.Random.Random_Number
                                   (Min, Max);
                        H    : constant Natural :=
                                 Natural (Init * Real (R - Min))
                                 + Min;
                     begin
                        Work (Current, X, Y) :=
                          Natural'Min (Max, H);
                     end;
                  else
                     Work (Current, X, Y) :=
                       Natural'Min
                         (Max,
                          Min +
                            Natural
                              (Real (Max - Min) * D));
                  end if;
               end;
            end if;
         end loop;
      end loop;

      for I in 1 .. Smoothing loop
         for X in Heights'Range (1) loop
            for Y in Heights'Range (2) loop
               declare
                  Total : Natural := 0;
                  Count : Natural := 0;
               begin
                  for DX in -1 .. 1 loop
                     for DY in -1 .. 1 loop
                        if Y + DY in Heights'Range (2) then
                           declare
                              New_X : Integer := X + DX;
                           begin
                              if New_X < Heights'First (1) then
                                 New_X := Heights'Last (1)
                                   + New_X + (Heights'First (1) - 1);
                              elsif New_X > Heights'Last (1) then
                                 New_X := Heights'First (1) + New_X
                                   - (Heights'Last (1) + 1);
                              end if;

                              Total := Total +
                                Work (Current, New_X, Y + DY);
                              Count := Count + 1;
                           end;
                        end if;
                     end loop;
                  end loop;
                  Work (not Current, X, Y) := Total / Count;
               end;
            end loop;
         end loop;
         Current := not Current;
      end loop;

      for X in Heights'Range (1) loop
         for Y in Heights'Range (2) loop
            Heights (X, Y) := Work (Current, X, Y);
         end loop;
      end loop;

      Free (Work);

   end Create_Height_Array;

end Concorde.Generate.Surfaces.Heights;
