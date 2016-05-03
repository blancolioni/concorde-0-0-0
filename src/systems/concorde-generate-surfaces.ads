with Ada.Finalization;

package Concorde.Generate.Surfaces is

   type Surface_Type is tagged limited private;

   procedure Create
     (Surface       : in out Surface_Type'Class;
      Detail_Width  : Natural;
      Detail_Height : Natural;
      Coarse_Width  : Natural;
      Coarse_Height : Natural;
      Seed          : Integer);

   function Detail_Across
     (Surface : Surface_Type'Class)
      return Natural;

   function Detail_Down
     (Surface : Surface_Type'Class)
      return Natural;

   function Detail_Height
     (Surface : Surface_Type'Class;
      X, Y    : Positive)
      return Integer;

   function Coarse_Height
     (Surface : Surface_Type'Class;
      X, Y    : Positive)
      return Positive;

   function Map_Height
     (Surface : Surface_Type'Class;
      Height  : Natural)
      return Natural;

   type Surface_Frequency is array (Positive range <>) of Unit_Real;

   procedure Generate_Surface
     (Surface      : out Surface_Type;
      Frequency    : in  Surface_Frequency;
      Smoothing    : in  Natural);

   procedure Generate_Continental_Surface
     (Surface      : out Surface_Type;
      Frequency    : in  Surface_Frequency;
      Smoothing    : in  Natural);

private

   type Surface_Array is
     array (Positive range <>, Positive range <>) of Integer;

   type Surface_Array_Access is access Surface_Array;

   type Height_Map is
     array (Natural range <>) of Natural;

   type Height_Map_Access is access Height_Map;

   type Surface_Type is
   limited new Ada.Finalization.Limited_Controlled with
      record
         Detail : Surface_Array_Access;
         Coarse : Surface_Array_Access;
         Map    : Height_Map_Access;
         KX, KY : Positive;
         Seed   : Integer;
      end record;

   overriding procedure Finalize (Surface : in out Surface_Type);

end Concorde.Generate.Surfaces;
