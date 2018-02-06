with Cairo.Image_Surface;

with Xtk.Events;

with Concorde.People.Individuals.Portraits;

package body Concorde.Xi_UI.Portraits is

   function Create_Surface
     (Individual : Concorde.People.Individuals.Individual_Type)
      return Cairo.Cairo_Surface;

   function Draw_Portrait
     (Widget    : not null access Xtk.Widget.Xtk_Widget_Record'Class;
      Context   : Cairo.Cairo_Context)
      return Xtk.Events.Event_Response;

   --------------------
   -- Create_Surface --
   --------------------

   function Create_Surface
     (Individual : Concorde.People.Individuals.Individual_Type)
      return Cairo.Cairo_Surface
   is
      use Concorde.People.Individuals;

      Surface     : constant Cairo.Cairo_Surface :=
                      Cairo.Image_Surface.Create
                        (Cairo.Image_Surface.Cairo_Format_ARGB32,
                         102, 102);
      Cr          : constant Cairo.Cairo_Context :=
                      Cairo.Create (Surface);

   begin
      Cairo.Save (Cr);
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Clear);
      Cairo.Paint (Cr);
      Cairo.Restore (Cr);

      if Individual /= null then
         Concorde.People.Individuals.Portraits.Draw_Portrait
           (Cr, Individual, 102, 102);
      end if;

      Cairo.Destroy (Cr);

      return Surface;

   end Create_Surface;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Portrait : in out Xtk_Portrait) is
   begin
      Xtk.Widget.Destroy (Xtk.Widget.Xtk_Widget (Portrait));
   end Destroy;

   -------------------
   -- Draw_Portrait --
   -------------------

   function Draw_Portrait
     (Widget    : not null access Xtk.Widget.Xtk_Widget_Record'Class;
      Context   : Cairo.Cairo_Context)
      return Xtk.Events.Event_Response
   is
      use Cairo;
      Portrait : constant Xtk_Portrait := Xtk_Portrait (Widget);
   begin
      if Portrait.Surface /= Null_Surface then
         Cairo.Set_Source_Surface (Context, Portrait.Surface, 0.0, 0.0);
         Cairo.Paint (Context);
      end if;
      return Xtk.Events.Propagate_Event;
   end Draw_Portrait;

   --------------
   -- On_Click --
   --------------

   procedure On_Click
     (Portrait : Xtk_Portrait_Div_Record'Class;
      Handler  : not null access
        procedure (Individual : Concorde.People.Individuals.Individual_Type))
   is
   begin
      Handler (Portrait.Individual);
   end On_Click;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      null;
   end Register;

   ------------------
   -- Set_Portrait --
   ------------------

   procedure Set_Portrait
     (Portrait   : in out Xtk_Portrait_Div_Record'Class;
      Individual : Concorde.People.Individuals.Individual_Type)
   is
      use Cairo;
   begin
      if Portrait.Surface /= Null_Surface then
         Cairo.Surface_Destroy (Portrait.Surface);
      end if;

      Portrait.Individual := Individual;
      Portrait.Surface := Create_Surface (Individual);

   end Set_Portrait;

   -------------
   -- Xtk_New --
   -------------

   procedure Xtk_New (Portrait : in out Xtk_Portrait) is
   begin
      Portrait := new Xtk_Portrait_Div_Record;
      Portrait.Surface := Cairo.Null_Surface;
      Portrait.Individual := null;
      Portrait.On_Draw (Draw_Portrait'Access);
   end Xtk_New;

end Concorde.Xi_UI.Portraits;
