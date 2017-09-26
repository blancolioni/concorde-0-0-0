package body Concorde.Maps is

   -------------------
   -- Expand_Border --
   -------------------

   procedure Expand_Border
     (Layout : Tile_Layout_Interface'Class;
      Tiles  : in out List_Of_Tiles.List)
   is
      Border : List_Of_Tiles.List;
   begin
      Layout.External_Border (Tiles, Border);
      for Index of Border loop
         Tiles.Append (Index);
      end loop;
   end Expand_Border;

   -------------------
   -- Expand_Border --
   -------------------

   procedure Expand_Border
     (Layout : Tile_Layout_Interface'Class;
      Tiles  : in out List_Of_Tiles.List;
      Match  : not null access
        function (Tile_Index : Positive) return Boolean)
   is
      Border : List_Of_Tiles.List;
   begin
      Layout.External_Border (Tiles, Border);
      for Index of Border loop
         if Match (Index) then
            Tiles.Append (Index);
         end if;
      end loop;
   end Expand_Border;

   ---------------------
   -- External_Border --
   ---------------------

   procedure External_Border
     (Layout : Tile_Layout_Interface'Class;
      Tiles  : List_Of_Tiles.List;
      Border : out List_Of_Tiles.List)
   is
   begin
      Border.Clear;
      for Tile of Tiles loop
         for Neighbour_Index in 1 .. Layout.Neighbour_Count (Tile) loop
            declare
               Neighbour : constant Positive :=
                             Layout.Neighbour (Tile, Neighbour_Index);
            begin
               if not Border.Contains (Neighbour)
                 and then not Tiles.Contains (Neighbour)
               then
                  Border.Append (Neighbour);
               end if;
            end;
         end loop;
      end loop;
   end External_Border;

   ---------------------
   -- Internal_Border --
   ---------------------

   procedure Internal_Border
     (Layout : Tile_Layout_Interface'Class;
      Tiles  : List_Of_Tiles.List;
      Border : out List_Of_Tiles.List)
   is
   begin
      Border.Clear;
      for Tile of Tiles loop
         for Neighbour_Index in 1 .. Layout.Neighbour_Count (Tile) loop
            declare
               Neighbour : constant Positive :=
                             Layout.Neighbour (Tile, Neighbour_Index);
            begin
               if Border.Contains (Neighbour)
                 and then not Tiles.Contains (Neighbour)
               then
                  Border.Append (Tile);
                  exit;
               end if;
            end;
         end loop;
      end loop;
   end Internal_Border;

   ----------------
   -- Scan_Tiles --
   ----------------

   procedure Scan_Tiles
     (Layout : Tile_Layout_Interface'Class;
      Match  : not null access
        function (Tile_Index : Positive)
      return Boolean;
      Result : out List_Of_Tiles.List)
   is
   begin
      Result.Clear;
      for I in 1 .. Layout.Tile_Count loop
         if Match (I) then
            Result.Append (I);
         end if;
      end loop;
   end Scan_Tiles;

end Concorde.Maps;
