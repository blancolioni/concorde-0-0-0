package body Concorde.Xi_UI.Individuals is

   -----------------------
   -- Portrait_Renderer --
   -----------------------

   function Portrait_Renderer
     return Xtk.Values.Renderers.Xtk_Value_Renderer
   is
   begin
      return Xtk.Values.Renderers.Text_Renderer;
   end Portrait_Renderer;

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

end Concorde.Xi_UI.Individuals;
