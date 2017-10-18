private with Ada.Strings.Unbounded;

with Xi.Color;

package Concorde.Xi_UI.Outliner is

   type Outliner_Element is private;

   function Image_Element (Resource_Name : String) return Outliner_Element;

   function Text_Element
     (Text  : String)
      return Outliner_Element;

   function Color_Text_Element
     (Text  : String;
      Color : Xi.Color.Xi_Color)
      return Outliner_Element;

   type Array_Of_Elements is array (Positive range <>) of Outliner_Element;

   function No_Elements return Array_Of_Elements;

   procedure Add_Item
     (Category : String;
      Identity : String;
      Element  : Outliner_Element;
      Tooltip  : Array_Of_Elements);

   procedure Add_Item
     (Category : String;
      Identity : String;
      Elements : Array_Of_Elements;
      Tooltip  : Array_Of_Elements);

   procedure Update_Item
     (Category : String;
      Identity : String;
      Element  : Outliner_Element;
      Tooltip  : Array_Of_Elements);

   procedure Update_Item
     (Category : String;
      Identity : String;
      Elements : Array_Of_Elements;
      Tooltip  : Array_Of_Elements);

   procedure Remove_Item
     (Category : String;
      Identity : String);

   procedure Expand (Category : String);
   procedure Collapse (Category : String);
   procedure Hide (Category : String);
   procedure Show (Category : String);

   procedure Render;

private

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   type Outliner_Element_Class is (Image_Element, Text_Element);

   type Outliner_Element is
      record
         Class     : Outliner_Element_Class;
         Text      : Ada.Strings.Unbounded.Unbounded_String;
         Color     : Xi.Color.Xi_Color;
      end record;

   Default_Color : constant Xi.Color.Xi_Color := (1.0, 1.0, 1.0, 1.0);

   function Image_Element
     (Resource_Name : String)
      return Outliner_Element
   is (Image_Element, +Resource_Name, Default_Color);

   function Text_Element
     (Text  : String)
      return Outliner_Element
   is (Text_Element, +Text, Default_Color);

   function Color_Text_Element
     (Text  : String;
      Color : Xi.Color.Xi_Color)
      return Outliner_Element
   is (Text_Element, +Text, Color);

   No_Element_Object : Array_Of_Elements (1 .. 0);

   function No_Elements return Array_Of_Elements
   is (No_Element_Object);

end Concorde.Xi_UI.Outliner;
