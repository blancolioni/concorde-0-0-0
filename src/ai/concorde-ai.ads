with Concorde.Empires;
with Concorde.Systems;

package Concorde.AI is

   type Root_AI_Type is abstract tagged private;

   procedure Start
     (AI     : in out Root_AI_Type;
      Empire : Concorde.Empires.Empire_Type);

   procedure Update_Focus
     (AI : in out Root_AI_Type);

   procedure System_Acquired
     (AI           : in out Root_AI_Type;
      System       : Concorde.Systems.Star_System_Type;
      Former_Owner : Concorde.Empires.Empire_Type);

   procedure System_Lost
     (AI        : in out Root_AI_Type;
      System    : Concorde.Systems.Star_System_Type;
      New_Owner : Concorde.Empires.Empire_Type);

   function Minimum_Attack_Factor
     (AI : Root_AI_Type)
      return Non_Negative_Real;

   type AI_Type is access all Root_AI_Type'Class;

private

   type Root_AI_Type is abstract tagged
      record
         Empire        : Concorde.Empires.Empire_Type;
         Defensiveness : Non_Negative_Real := 1.2;
      end record;

   procedure Update_Defensiveness
     (AI           : in out Root_AI_Type'Class;
      Enemy        : Concorde.Empires.Empire_Type;
      Can_Increase : Boolean := True;
      Can_Decrease : Boolean := True);

end Concorde.AI;
