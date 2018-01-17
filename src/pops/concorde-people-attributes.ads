private with Ada.Containers.Indefinite_Vectors;

with Concorde.People.Abilities;
with Concorde.People.Proficiencies;
with Concorde.People.Skills;

package Concorde.People.Attributes is

   type Attribute_Reference (<>) is private;

   function Ability_Reference
     (Ability : Concorde.People.Abilities.Ability_Type)
      return Attribute_Reference;

   function Skill_Reference
     (Skill : Concorde.People.Skills.Skill_Type)
      return Attribute_Reference;

   function Proficiency_Reference
     (Proficiency : Concorde.People.Proficiencies.Proficiency_Type)
      return Attribute_Reference;

   function Ability_Reference
     (Ability : Concorde.People.Abilities.Ability_Type;
      Score   : Concorde.People.Abilities.Ability_Score_Range)
      return Attribute_Reference;

   function Skill_Reference
     (Skill : Concorde.People.Skills.Skill_Type;
      Score : Concorde.People.Skills.Skill_Level_Range)
      return Attribute_Reference;

   function Proficiency_Reference
     (Proficiency : Concorde.People.Proficiencies.Proficiency_Type;
      Score       : Concorde.People.Proficiencies.Proficiency_Score_Range)
      return Attribute_Reference;

   type Attribute_Container is tagged private;

   function Is_Empty
     (Container : Attribute_Container'Class)
      return Boolean;

   procedure Insert
     (Container : in out Attribute_Container'Class;
      Attribute : Attribute_Reference);

   procedure Iterate
     (Container : Attribute_Container'Class;
      Process   : not null access
        procedure (Attribute : Attribute_Reference));

   function Random_Choice
     (Container : Attribute_Container'Class)
      return Attribute_Reference
     with Pre => not Container.Is_Empty;

   type Has_Attributes is limited interface;

   function Ability_Score
     (Attrs   : Has_Attributes;
      Ability : Concorde.People.Abilities.Ability_Type)
      return Concorde.People.Abilities.Ability_Score_Range
      is abstract;

   function Skill_Level
     (Attrs : Has_Attributes;
      Skill : Concorde.People.Skills.Skill_Type)
      return Concorde.People.Skills.Skill_Level_Range
      is abstract;

   function Proficiency_Level
     (Attrs       : Has_Attributes;
      Proficiency : Concorde.People.Proficiencies.Proficiency_Type)
      return Concorde.People.Proficiencies.Proficiency_Score_Range
      is abstract;

   procedure Set_Ability_Score
     (Attrs   : in out Has_Attributes;
      Ability : Concorde.People.Abilities.Ability_Type;
      Score   : Concorde.People.Abilities.Ability_Score_Range)
   is abstract;

   procedure Set_Skill_Level
     (Attrs : in out Has_Attributes;
      Skill : Concorde.People.Skills.Skill_Type;
      Level : Concorde.People.Skills.Skill_Level_Range)
      is abstract;

   procedure Set_Proficiency_Level
     (Attrs       : in out Has_Attributes;
      Proficiency : Concorde.People.Proficiencies.Proficiency_Type;
      Score       : Concorde.People.Proficiencies.Proficiency_Score_Range)
      is abstract;

   procedure Improve
     (Attrs     : in out Has_Attributes'Class;
      Reference : Attribute_Reference);

   function Qualifies
     (Container : Attribute_Container'Class;
      Attrs     : Has_Attributes'Class)
      return Boolean;

   function Chance
     (Container : Attribute_Container'Class;
      Attrs     : Has_Attributes'Class)
      return Unit_Real;

private

   type Attribute_Type is
     (Ability_Attribute, Skill_Attribute, Proficiency_Attribute);

   type Attribute_Reference (Attribute : Attribute_Type) is
      record
         Has_Value : Boolean;
         Value     : Natural;
         case Attribute is
            when Ability_Attribute =>
               Ability : Concorde.People.Abilities.Ability_Type;
            when Skill_Attribute =>
               Skill   : Concorde.People.Skills.Skill_Type;
            when Proficiency_Attribute =>
               Proficiency : Concorde.People.Proficiencies.Proficiency_Type;
         end case;
      end record;

   function Ability_Reference
     (Ability : Concorde.People.Abilities.Ability_Type)
      return Attribute_Reference
   is (Ability_Attribute, False, 0, Ability);

   function Skill_Reference
     (Skill : Concorde.People.Skills.Skill_Type)
      return Attribute_Reference
   is (Skill_Attribute, False, 0, Skill);

   function Proficiency_Reference
     (Proficiency : Concorde.People.Proficiencies.Proficiency_Type)
      return Attribute_Reference
   is (Proficiency_Attribute, False, 0, Proficiency);

   function Ability_Reference
     (Ability : Concorde.People.Abilities.Ability_Type;
      Score   : Concorde.People.Abilities.Ability_Score_Range)
      return Attribute_Reference
   is (Ability_Attribute, True, Natural (Score), Ability);

   function Skill_Reference
     (Skill : Concorde.People.Skills.Skill_Type;
      Score : Concorde.People.Skills.Skill_Level_Range)
      return Attribute_Reference
   is (Skill_Attribute, True, Natural (Score), Skill);

   function Proficiency_Reference
     (Proficiency : Concorde.People.Proficiencies.Proficiency_Type;
      Score       : Concorde.People.Proficiencies.Proficiency_Score_Range)
      return Attribute_Reference
   is (Proficiency_Attribute, True, Natural (Score), Proficiency);

   package Attribute_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Attribute_Reference);

   type Attribute_Container is tagged
      record
         Vector : Attribute_Vectors.Vector;
      end record;

   function Is_Empty
     (Container : Attribute_Container'Class)
      return Boolean
   is (Container.Vector.Is_Empty);

end Concorde.People.Attributes;
