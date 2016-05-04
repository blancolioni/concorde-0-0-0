with Ada.Unchecked_Deallocation;

with WL.Random;

with Concorde.Elementary_Functions;

with Concorde.Generate.Surfaces.Heights;

package body Concorde.Generate.Surfaces is

   Height_Min : constant := 0;
   Height_Max : constant := 255;

   procedure Create_Height_Map
     (Surface           : in out Surface_Type;
      Smoothing         : in  Natural;
      Frequency       : Surface_Frequency;
      Initial_Heights   : access
        function (X, Y : Positive) return Unit_Real);

   procedure Copy_To_Coarse
     (Surface : in out Surface_Type'Class;
      Frequency       : Surface_Frequency);

   -------------------
   -- Coarse_Height --
   -------------------

   function Coarse_Height
     (Surface : Surface_Type'Class;
      X, Y    : Positive)
      return Positive
   is
   begin
      return Surface.Coarse (X, Y);
   end Coarse_Height;

   -------------------
   -- Coarse_Height --
   -------------------

   function Coarse_Height
     (Surface : Surface_Type'Class;
      X1, X2  : Positive;
      Y1, Y2  : Positive)
      return Positive
   is
      Count : constant Positive := (X2 - X1 + 1) * (Y2 - Y1 + 1);
      Total : Natural := 0;
   begin
      for X in X1 .. X2 loop
         for Y in Y1 .. Y2 loop
            Total := Total + Surface.Coarse (X, Y);
         end loop;
      end loop;
      return (Total + Count / 2) / Count;
   end Coarse_Height;

   --------------------
   -- Copy_To_Coarse --
   --------------------

   procedure Copy_To_Coarse
     (Surface : in out Surface_Type'Class;
      Frequency       : Surface_Frequency)
   is
      Height_Freq : array (Height_Min .. Height_Max) of Natural :=
                      (others => 0);
   begin

      for X in Surface.Coarse'Range (1) loop
         for Y in Surface.Coarse'Range (2) loop
            declare
               Total : Integer := 0;
               Count : Natural := 0;
            begin
               for DX in 1 .. Surface.KX loop
                  for DY in 1 .. Surface.KY loop
                     Total := Total
                       + Surface.Detail ((X - 1) * Surface.KX + DX,
                                         (Y - 1) * Surface.KY + DY);
                     Count := Count + 1;
                  end loop;
               end loop;
               Surface.Coarse (X, Y) := Total / Count;
            end;
         end loop;
      end loop;

      for X in Surface.Coarse'Range (1) loop
         for Y in Surface.Coarse'Range (2) loop
            declare
               F : Natural renames Height_Freq (Surface.Coarse_Height (X, Y));
            begin
               F := F + 1;
            end;
         end loop;
      end loop;

      declare
         Tile_Count : array (Frequency'Range) of Natural :=
                        (others => 0);
         Total_Tiles : constant Real :=
                         Real (Surface.Coarse'Length (1)
                               * Surface.Coarse'Length (2));
         Step        : Positive := Frequency'First;
         Current_Count : Natural := 0;
         Map           : Height_Map (Height_Min .. Height_Max);
      begin
         for I in Tile_Count'Range loop
            Tile_Count (I) := Natural (Frequency (I) * Total_Tiles);
         end loop;

         for I in Height_Freq'Range loop
            Current_Count := Current_Count + Height_Freq (I);
            if Step <= Frequency'Last
              and then Current_Count > Tile_Count (Step)
            then
               Step := Step + 1;
               Current_Count := 0;
            end if;
            Map (I) := Step;
         end loop;

         for X in Surface.Coarse'Range (1) loop
            for Y in Surface.Coarse'Range (2) loop
               Surface.Coarse (X, Y) :=
                 Map (Surface.Coarse (X, Y));
            end loop;
         end loop;

         Surface.Map := new Height_Map'(Map);

      end;

   end Copy_To_Coarse;

   ------------
   -- Create --
   ------------

   procedure Create
     (Surface       : in out Surface_Type'Class;
      Detail_Width  : Natural;
      Detail_Height : Natural;
      Coarse_Width  : Natural;
      Coarse_Height : Natural;
      Seed          : Integer)
   is
   begin
      Surface.Detail :=
        new Surface_Array (1 .. Detail_Width, 1 .. Detail_Height);
      Surface.Coarse :=
        new Surface_Array (1 .. Coarse_Width, 1 .. Coarse_Height);

      Surface.KX := Detail_Width / Coarse_Width;
      Surface.KY := Detail_Height / Coarse_Height;

      Surface.Seed := Seed;

   end Create;

   -----------------------
   -- Create_Height_Map --
   -----------------------

   procedure Create_Height_Map
     (Surface           : in out Surface_Type;
      Smoothing         : in  Natural;
      Frequency       : Surface_Frequency;
      Initial_Heights   : access
        function (X, Y : Positive) return Unit_Real)
   is
   begin

      Surfaces.Heights.Create_Height_Array
        (Heights         => Surface.Detail.all,
         Smoothing       => Smoothing,
         Min             => Height_Min,
         Max             => Height_Max,
         Random          => True,
         Initial_Heights => Initial_Heights);

      Copy_To_Coarse (Surface, Frequency);

   end Create_Height_Map;

   -------------------
   -- Detail_Across --
   -------------------

   function Detail_Across
     (Surface : Surface_Type'Class)
      return Natural
   is
   begin
      return Surface.Detail'Length (1);
   end Detail_Across;

   -----------------
   -- Detail_Down --
   -----------------

   function Detail_Down
     (Surface : Surface_Type'Class)
      return Natural
   is
   begin
      return Surface.Detail'Length (2);
   end Detail_Down;

   -------------------
   -- Detail_Height --
   -------------------

   function Detail_Height
     (Surface : Surface_Type'Class;
      X, Y    : Positive)
      return Integer
   is
   begin
      return Surface.Detail (X, Y);
   end Detail_Height;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Surface : in out Surface_Type) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Surface_Array, Surface_Array_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (Height_Map, Height_Map_Access);
   begin
      if Surface.Detail /= null then
         Free (Surface.Detail);
      end if;
      if Surface.Coarse /= null then
         Free (Surface.Coarse);
      end if;
      if Surface.Map /= null then
         Free (Surface.Map);
      end if;
   end Finalize;

   ----------------------------------
   -- Generate_Continental_Surface --
   ----------------------------------

   procedure Generate_Continental_Surface
     (Surface      : out Surface_Type;
      Frequency    : in  Surface_Frequency;
      Smoothing    : in  Natural)
   is

      use Concorde.Elementary_Functions;

      type Point is
         record
            X, Y : Integer;
         end record;

      type Line is
         record
            P1, P2 : Point;
         end record;

      function Distance (P : Point;
                         L : Line)
                         return Real;

      function Distance (X1, Y1, X2, Y2 : Real)
                         return Real;

      Spines : array (1 .. 4) of Line;

      Width : constant Positive := Surface.Detail'Length (1);
      Height : constant Positive := Surface.Detail'Length (2);

      Max_D : constant Real :=
                Sqrt (Real (Width / (Spines'Length + 1)) ** 2
                      + Real (Height / (Spines'Length + 1)) ** 2);

      function Continent_Heights (X, Y : Positive) return Unit_Real;

      -----------------------
      -- Continent_Heights --
      -----------------------

      function Continent_Heights (X, Y : Positive) return Unit_Real is
         Ds : array (Spines'Range) of Real;
         Min : Real := Real'Last;
      begin
         for I in Ds'Range loop
            Ds (I) := Distance ((X, Y), Spines (I));
            if Ds (I) < Min then
               Min := Ds (I);
            end if;
         end loop;

         if Min >= Max_D then
            return 0.1;
         else
            return (Max_D - Min) / Max_D * 0.89 + 0.1;
         end if;
      end Continent_Heights;

      --------------
      -- Distance --
      --------------

      function Distance (X1, Y1, X2, Y2 : Real)
                         return Real
      is
      begin
         return Sqrt ((X1 - X2) ** 2 + (Y1 - Y2) ** 2);
      end Distance;

      --------------
      -- Distance --
      --------------

      function Distance (P : Point;
                         L : Line)
                         return Real
      is
         PX : constant Real := Real (P.X);
         PY : constant Real := Real (P.Y);
         VX : constant Real := Real (L.P1.X);
         VY : constant Real := Real (L.P1.Y);
         WX : constant Real := Real (L.P2.X);
         WY : constant Real := Real (L.P2.Y);
         Length_2 : constant Real :=
                      (VX - WX) ** 2 + (VY - WY) ** 2;

      begin
         if Length_2 = 0.0 then
            return Distance (PX, PY, VX, VY);
         end if;

         declare
            T : constant Real := ((PX - VX) * (WX - VX)
                                   + (PY - VY) * (WY - VY))
                  / Length_2;
         begin
            if T < 0.0 then
               return Distance (PX, PY, VX, VY);
            elsif T > 1.0 then
               return Distance (PX, PY, WX, WY);
            else
               declare
                  DX   : constant Real := WX - VX;
                  DY   : constant Real := WY - VY;
                  Proj_X : constant Real := VX + T * DX;
                  Proj_Y : constant Real := VY + T * DY;
               begin
                  return Distance (PX, PY, Proj_X, Proj_Y);
               end;
            end if;
         end;
      end Distance;

      Pangaea : constant Boolean :=
                  WL.Random.Random_Number (1, 4) = 1;
      Left_Factor : constant Positive :=
                      (if Pangaea then 4 else 1);
      Right_Factor : constant Positive :=
                       (if Pangaea then 2 else 4);
   begin
      Spines (1) :=
        ((Width / 6,
         WL.Random.Random_Number (Height / 4, Height / 2)),
         (2 * Left_Factor * Width / 6,
          WL.Random.Random_Number (Height / 4, Height / 2)));
      Spines (2) :=
        ((WL.Random.Random_Number (1 * Width / 6, 2 * Width / 6),
         Height / 2),
         (WL.Random.Random_Number (1 * Width / 6, 2 * Width / 6),
          3 * Height / 4));
      Spines (3) :=
        ((WL.Random.Random_Number (Right_Factor * Width / 6, 5 * Width / 6),
         WL.Random.Random_Number (Height / 4, Height / 2)),
         (WL.Random.Random_Number (4 * Width / 6, 5 * Width / 6),
          WL.Random.Random_Number (Height / 4, Height / 2)));
      Spines (4) :=
        ((WL.Random.Random_Number (4 * Width / 6, 5 * Width / 6),
         WL.Random.Random_Number (Height / 2, 3 * Height / 4)),
         (WL.Random.Random_Number (4 * Width / 6, 5 * Width / 6),
          WL.Random.Random_Number (Height / 2, 3 * Height / 4)));

      Create_Height_Map (Surface, Smoothing, Frequency,
                         Continent_Heights'Access);
   end Generate_Continental_Surface;

   ----------------------
   -- Generate_Surface --
   ----------------------

   procedure Generate_Surface
     (Surface      : out Surface_Type;
      Frequency    : in  Surface_Frequency;
      Smoothing    : in  Natural)
   is
      type Work_Array is
        array (Positive range <>, Positive range <>) of Natural;
      type Work_Array_Access is access Work_Array;
      procedure Free is
        new Ada.Unchecked_Deallocation (Work_Array, Work_Array_Access);

      Points_Across : constant Natural := Surface.Detail'Length (1);
      Points_Down   : constant Natural := Surface.Detail'Length (2);

      Work : array (Boolean) of Work_Array_Access :=
               (new Work_Array (1 .. Points_Across, 1 .. Points_Down),
                new Work_Array (1 .. Points_Across, 1 .. Points_Down));
      Current : Boolean := False;
   begin
      for X in Work (Current)'Range (1) loop
         for Y in Work (Current)'Range (2) loop
            Work (Current) (X, Y) :=
              WL.Random.Random_Number (Height_Min, Height_Max);
         end loop;
      end loop;

      for I in 1 .. Smoothing loop

         declare
            W : constant Work_Array_Access := Work (Current);
         begin
            for X in W'Range (1) loop
               for Y in W'Range (2) loop
                  declare
                     Total : Natural := 0;
                     Count : Natural := 0;
                  begin
                     for DX in -1 .. 1 loop
                        for DY in -1 .. 1 loop
                           declare
                              WX : Integer := X + DX;
                              WY : constant Integer := Y + DY;
                           begin
                              if WX = 0 then
                                 WX := W'Last (1);
                              elsif WX = W'Last (1) + 1 then
                                 WX := 1;
                              end if;
                              if WY in W'Range (2) then
                                 Count := Count + 1;
                                 Total := Total + W (WX, WY);
                              end if;
                           end;
                        end loop;
                     end loop;
                     Work (not Current) (X, Y) := Total / Count;
                  end;
               end loop;
            end loop;
         end;
         Current := not Current;
      end loop;

      for X in Surface.Detail'Range (1) loop
         for Y in Surface.Detail'Range (2) loop
            Surface.Detail (X, Y) := Work (Current) (X, Y);
         end loop;
      end loop;

      Copy_To_Coarse (Surface, Frequency);

      Free (Work (False));
      Free (Work (True));

   end Generate_Surface;

   ----------------
   -- Map_Height --
   ----------------

   function Map_Height
     (Surface : Surface_Type'Class;
      Height  : Natural)
      return Natural
   is
   begin
      return Surface.Map (Height);
   end Map_Height;

end Concorde.Generate.Surfaces;
