with Concorde.Xi_UI.Portraits;

package body Concorde.Xi_UI.Individuals is

   type Portrait_Renderer_Record is
     new Xtk.Values.Renderers.Xtk_Value_Renderer_Interface with null record;

   overriding function Render
     (Renderer : Portrait_Renderer_Record;
      Value    : Xtk.Values.Xtk_Value_Interface'Class)
      return Xtk.Widget.Xtk_Widget;

   overriding procedure Update
     (Renderer : Portrait_Renderer_Record;
      Widget   : not null access
        Xtk.Widget.Xtk_Widget_Record'Class;
      Value    : Xtk.Values.Xtk_Value_Interface'Class);

   -----------------------
   -- Portrait_Renderer --
   -----------------------

   function Portrait_Renderer
     return Xtk.Values.Renderers.Xtk_Value_Renderer
   is
   begin
      return Renderer : Portrait_Renderer_Record;
   end Portrait_Renderer;

   ------------
   -- Render --
   ------------

   overriding function Render
     (Renderer : Portrait_Renderer_Record;
      Value    : Xtk.Values.Xtk_Value_Interface'Class)
      return Xtk.Widget.Xtk_Widget
   is
      pragma Unreferenced (Renderer);
      Portrait : Concorde.Xi_UI.Portraits.Xtk_Portrait;
   begin
      Concorde.Xi_UI.Portraits.Xtk_New (Portrait);
      Portrait.Set_Attribute ("class", "portrait-medium");
      if Value in Individual_Value'Class then
         Portrait.Set_Portrait
           (Individual => Individual_Value'Class (Value).Individual);
      end if;
      return Xtk.Widget.Xtk_Widget (Portrait);
   end Render;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Value : Individual_Value)
      return String
   is
      use type Concorde.People.Individuals.Individual_Type;
   begin
      if Value.Individual = null then
         return "";
      else
         return Value.Individual.Full_Name;
      end if;
   end To_String;

   --------------
   -- To_Value --
   --------------

   function To_Value
     (Individual : access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
      return Xtk.Values.Xtk_Value_Interface'Class
   is
   begin
      return Individual_Value'
        (Individual =>
           Concorde.People.Individuals.Individual_Type (Individual));
   end To_Value;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Renderer : Portrait_Renderer_Record;
      Widget   : not null access
        Xtk.Widget.Xtk_Widget_Record'Class;
      Value    : Xtk.Values.Xtk_Value_Interface'Class)
   is
      pragma Unreferenced (Renderer);
      Portrait : constant Concorde.Xi_UI.Portraits.Xtk_Portrait :=
        Concorde.Xi_UI.Portraits.Xtk_Portrait (Widget);
   begin
      if Value in Individual_Value'Class then
         Portrait.Set_Portrait
           (Individual => Individual_Value'Class (Value).Individual);
      end if;
   end Update;

end Concorde.Xi_UI.Individuals;
