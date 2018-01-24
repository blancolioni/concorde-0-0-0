with Cairo;

with Xtk.Div_Element;

with Concorde.People.Individuals;

package Concorde.Xi_UI.Portraits is

   type Xtk_Portrait_Div_Record is
     new Xtk.Div_Element.Xtk_Div_Element_Record with private;

   type Xtk_Portrait is access all Xtk_Portrait_Div_Record'Class;

   procedure Set_Portrait
     (Widget     : in out Xtk_Portrait_Div_Record'Class;
      Individual : Concorde.People.Individuals.Individual_Type);

   procedure Register;

private

   type Xtk_Portrait_Div_Record is
     new Xtk.Div_Element.Xtk_Div_Element_Record with
      record
         Individual : Concorde.People.Individuals.Individual_Type;
         Surface    : Cairo.Cairo_Surface;
      end record;

end Concorde.Xi_UI.Portraits;
