with WL.Random;

package body Concorde.Maps.Height_Maps is

   Raw_Height_Min : constant := 0;
   Raw_Height_Max : constant := 255;

   type Raw_Height is range Raw_Height_Min .. Raw_Height_Max
     with Size => 8;

   type Height_Array is array (Positive range <>) of Raw_Height
     with Component_Size => 8;

   procedure Copy_Height_Map
     (Layout    : in out Tile_Layout_Interface'Class;
      Heights   : Height_Array;
      Frequency : Frequency_Array);

   procedure Copy_Height_Map
     (Layout    : in out Tile_Layout_Interface'Class;
      Heights   : Height_Array;
      Frequency : Frequency_Array)
   is
      Height_Occurances : array (Raw_Height) of Natural :=
                            (others => 0);
      Total_Tiles      : array (Frequency'Range) of Natural :=
                            (others => 0);
      Height_Map        : array (Raw_Height) of Positive;
   begin
      for I in Heights'Range loop
         declare
            T : Natural renames Height_Occurances (Heights (I));
         begin
            T := T + 1;
         end;
      end loop;

      for I in Total_Tiles'Range loop
         Total_Tiles (I) := Natural (Frequency (I) * Real (Heights'Length));
      end loop;

      declare
         Cum : Natural := 0;
         Index : Positive := Frequency'First;
      begin
         for Ht in Raw_Height loop
            Height_Map (Ht) := Index;
            Cum := Cum + Height_Occurances (Ht);
            if Index < Frequency'Last
              and then Cum > Total_Tiles (Index)
            then
               Cum := Cum - Total_Tiles (Index);
               Index := Index + 1;
            end if;
         end loop;
      end;

      for I in 1 .. Layout.Tile_Count loop
         Layout.Set_Height (I, Height_Map (Heights (I)));
      end loop;

   end Copy_Height_Map;

   -----------------------
   -- Create_Height_Map --
   -----------------------

   procedure Create_Height_Map
     (Layout    : in out Tile_Layout_Interface'Class;
      Frequency : Frequency_Array;
      Smoothing : Positive)
   is
      Work : array (0 .. 1) of Height_Array (1 .. Layout.Tile_Count);
      Current : Natural := 0;
   begin
      for I in Work (Current)'Range loop
         Work (Current) (I) :=
           Raw_Height
             (WL.Random.Random_Number (Raw_Height_Min, Raw_Height_Max));
      end loop;

      for I in 1 .. Smoothing loop
         for J in Work (Current)'Range loop
            declare
               Total : Natural := Natural (Work (Current) (J));
               Count : constant Natural :=
                         Layout.Neighbour_Count (J);
            begin
               for N in 1 .. Count loop
                  Total := Total
                    + Natural (Work (Current) (Layout.Neighbour (J, N)));
               end loop;
               Work (1 - Current) (J) := Raw_Height (Total / (Count + 1));
            end;
         end loop;
         Current := 1 - Current;
      end loop;

      Copy_Height_Map (Layout, Work (Current), Frequency);

   end Create_Height_Map;

end Concorde.Maps.Height_Maps;
