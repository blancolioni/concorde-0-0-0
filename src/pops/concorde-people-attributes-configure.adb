package body Concorde.People.Attributes.Configure is

   --------------------------
   -- Configure_Attributes --
   --------------------------

   procedure Configure_Attributes
     (Container : in out Attribute_Container'Class;
      Config    : Tropos.Configuration)
   is
   begin
      for Attr_Config of Config loop
         declare
            Name      : constant String := Attr_Config.Config_Name;
            Has_Value : constant Boolean := Attr_Config.Child_Count > 0;
            Value     : constant Natural :=
                          (if Has_Value then Attr_Config.Value else 0);
         begin
            if Concorde.People.Abilities.Exists (Name) then
               Container.Vector.Append
                 (Attribute_Reference'
                    (Attribute   => Ability_Attribute,
                     Has_Value   => Has_Value,
                     Value       => Value,
                     Ability     =>
                       Concorde.People.Abilities.Get (Name)));
            elsif Concorde.People.Proficiencies.Exists (Name) then
               Container.Vector.Append
                 (Attribute_Reference'
                    (Attribute   => Proficiency_Attribute,
                     Has_Value   => Has_Value,
                     Value       => Value,
                     Proficiency =>
                       Concorde.People.Proficiencies.Get (Name)));
            elsif Concorde.People.Skills.Exists (Name) then
               Container.Vector.Append
                 (Attribute_Reference'
                    (Attribute   => Skill_Attribute,
                     Has_Value   => Has_Value,
                     Value       => Value,
                     Skill       =>
                       Concorde.People.Skills.Get (Name)));
            else
               raise Constraint_Error with
                 "no such attribute: " & Name;
            end if;
         end;
      end loop;
   end Configure_Attributes;

end Concorde.People.Attributes.Configure;
