with Xtk.Values.Renderers;
with Xtk.Widget;

with Concorde.People.Individuals;

package Concorde.Xi_UI.Individuals is

   type Individual_Value is
     new Xtk.Values.Xtk_Value_Interface with private;

   overriding function To_String
     (Value : Individual_Value)
      return String;

   function To_Value
     (Individual : access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
      return Xtk.Values.Xtk_Value_Interface'Class;

   function Portrait_Renderer
     return Xtk.Values.Renderers.Xtk_Value_Renderer;

private

   type Individual_Value is
     new Xtk.Values.Xtk_Value_Interface with
      record
         Individual : Concorde.People.Individuals.Individual_Type;
      end record;

end Concorde.Xi_UI.Individuals;
