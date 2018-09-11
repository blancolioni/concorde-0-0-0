with Concorde.Objects;

package Concorde.Politics is

   type Political_Axis is
     (Control,
      Corporate,
      Democracy,
      Equality,
      Freedom,
      Individual,
      Military,
      Order,
      Religion,
      Society,
      Spending,
      State);

   type Political_Interface is limited interface;

   function Position
     (Political : Political_Interface;
      Axis      : Political_Axis)
      return Unit_Real
      is abstract;

   function Strength
     (Political : Political_Interface;
      Axis      : Political_Axis)
      return Unit_Real
      is abstract;

   function Match
     (Political : Political_Interface'Class;
      Other     : Political_Interface'Class)
      return Unit_Real;

   function Choose
     (Political : Political_Interface'Class;
      Random    : Unit_Real;
      Count     : Positive;
      Get       : not null access
        function (Index : Positive) return Political_Interface'Class)
      return Positive;

   type Political_Record is
     new Political_Interface with private;

   procedure Set_Axis
     (Rec      : in out Political_Record'Class;
      Axis     : Political_Axis;
      Position : Unit_Real;
      Strength : Unit_Real);

   function Join
     (Left, Right : Political_Interface'Class)
      return Political_Record;

   procedure Join
     (Left  : in out Political_Record'Class;
      Right : Political_Interface'Class);

   procedure Add_Noise
     (Rec     : in out Political_Record'Class;
      Std_Dev : Non_Negative_Real);

   type Named_Polical_Record is
     abstract new Political_Record
     and Concorde.Objects.Named_Object_Interface
   with private;

private

   type Axis_Item is
      record
         Position : Unit_Real;
         Strength : Unit_Real;
      end record;

   type Political_Axis_Array is
     array (Political_Axis) of Axis_Item;

   type Political_Record is
     new Political_Interface with
      record
         Axis : Political_Axis_Array :=
                  (others => (0.0, 0.0));
      end record;

   overriding function Position
     (Political : Political_Record;
      Axis      : Political_Axis)
      return Unit_Real
   is (Political.Axis (Axis).Position);

   overriding function Strength
     (Political : Political_Record;
      Axis      : Political_Axis)
      return Unit_Real
   is (Political.Axis (Axis).Strength);

   type Named_Polical_Record is
     abstract new Political_Record
     and Concorde.Objects.Named_Object_Interface
   with null record;

end Concorde.Politics;
