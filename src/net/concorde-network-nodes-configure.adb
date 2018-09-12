package body Concorde.Network.Nodes.Configure is

   ----------------
   -- Add_Output --
   ----------------

   procedure Add_Effect
     (From       : in out Root_Node_Type'Class;
      To         : String;
      Expression : Expressions.Expression_Type;
      Wait       : Duration)
   is
   begin
      From.Effects.Append
        (Effect_Record'
           (Target       => new String'(To),
            Effect_Delay => Wait,
            Expression   => Expression));
   end Add_Effect;

end Concorde.Network.Nodes.Configure;
