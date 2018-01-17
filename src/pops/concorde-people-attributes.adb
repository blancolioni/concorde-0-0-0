with WL.Random;

package body Concorde.People.Attributes is

   ------------
   -- Chance --
   ------------

   function Chance
     (Container : Attribute_Container'Class;
      Attrs     : Has_Attributes'Class)
      return Unit_Real
   is
      use Concorde.People.Abilities;
      use Concorde.People.Proficiencies;
      use Concorde.People.Skills;
      Score  : Natural := 0;
      Target : Natural := 0;
      Chance : constant array (0 .. 6) of Unit_Real :=
                 (0.1, 0.3, 0.5, 0.6, 0.7, 0.8, 0.9);
   begin
      for Reference of Container.Vector loop
         case Reference.Attribute is
            when Ability_Attribute =>
               if Reference.Has_Value then
                  Target := Target + Reference.Value;
               end if;
               Score := Score
                 + Natural (Attrs.Ability_Score (Reference.Ability));
            when Proficiency_Attribute =>
               if Reference.Has_Value then
                  Target := Target + Reference.Value;
               end if;
               Score := Score
                 + Natural (Attrs.Proficiency_Level (Reference.Proficiency));
            when Skill_Attribute =>
               if Reference.Has_Value then
                  Target := Target + Reference.Value;
               end if;
               Score := Score
                 + Natural (Attrs.Skill_Level (Reference.Skill));
         end case;
      end loop;

      return Result : constant Unit_Real :=
        (if Target > Score
         then 0.0
         elsif Score - Target > Chance'Last
         then 0.95
         else Chance (Score - Target));
   end Chance;

   -------------
   -- Improve --
   -------------

   procedure Improve
     (Attrs     : in out Has_Attributes'Class;
      Reference : Attribute_Reference)
   is
      use Concorde.People.Abilities;
      use Concorde.People.Proficiencies;
      use Concorde.People.Skills;
   begin
      case Reference.Attribute is
         when Ability_Attribute =>
            if Reference.Has_Value then
               if Ability_Score_Range (Reference.Value)
                 > Attrs.Ability_Score (Reference.Ability)
               then
                  Attrs.Set_Ability_Score
                    (Reference.Ability,
                     Ability_Score_Range (Reference.Value));
               end if;
            else
               Attrs.Set_Ability_Score
                 (Reference.Ability,
                  Attrs.Ability_Score (Reference.Ability) + 1);
            end if;
         when Proficiency_Attribute =>
            if Reference.Has_Value then
               if Proficiency_Score_Range (Reference.Value)
                 > Attrs.Proficiency_Level (Reference.Proficiency)
               then
                  Attrs.Set_Proficiency_Level
                    (Reference.Proficiency,
                     Proficiency_Score_Range (Reference.Value));
               end if;
            else
               Attrs.Set_Proficiency_Level
                 (Reference.Proficiency,
                  Attrs.Proficiency_Level (Reference.Proficiency) + 1);
            end if;
         when Skill_Attribute =>
            if Reference.Has_Value then
               if Skill_Level_Range (Reference.Value)
                 > Attrs.Skill_Level (Reference.Skill)
               then
                  Attrs.Set_Skill_Level
                    (Reference.Skill,
                     Skill_Level_Range (Reference.Value));
               end if;
            else
               Attrs.Set_Skill_Level
                 (Reference.Skill,
                  Attrs.Skill_Level (Reference.Skill) + 1);
            end if;
      end case;

   end Improve;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Attribute_Container'Class;
      Attribute : Attribute_Reference)
   is
   begin
      Container.Vector.Append (Attribute);
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Attribute_Container'Class;
      Process   : not null access
        procedure (Attribute : Attribute_Reference))
   is
   begin
      for Attribute of Container.Vector loop
         Process (Attribute);
      end loop;
   end Iterate;

   ---------------
   -- Qualifies --
   ---------------

   function Qualifies
     (Container : Attribute_Container'Class;
      Attrs     : Has_Attributes'Class)
      return Boolean
   is
      use Concorde.People.Abilities;
      use Concorde.People.Proficiencies;
      use Concorde.People.Skills;
   begin
      for Ref of Container.Vector loop
         case Ref.Attribute is
            when Ability_Attribute =>
               if Attrs.Ability_Score (Ref.Ability)
                 < Ability_Score_Range (Ref.Value)
               then
                  return False;
               end if;
            when Proficiency_Attribute =>
               if Attrs.Proficiency_Level (Ref.Proficiency)
                 < Proficiency_Score_Range (Ref.Value)
               then
                  return False;
               end if;
            when Skill_Attribute =>
               if Attrs.Skill_Level (Ref.Skill)
                 < Skill_Level_Range (Ref.Value)
               then
                  return False;
               end if;
         end case;
      end loop;
      return True;
   end Qualifies;

   -------------------
   -- Random_Choice --
   -------------------

   function Random_Choice
     (Container : Attribute_Container'Class)
      return Attribute_Reference
   is
      Index : constant Positive :=
                WL.Random.Random_Number (1, Container.Vector.Last_Index);
   begin
      return Container.Vector.Element (Index);
   end Random_Choice;

end Concorde.People.Attributes;
