with WL.Localisation;

package body Concorde.Powers.Armies is

   type Appoint_General_Power is
     new Root_Power_Type with null record;

   overriding function Class_Identifier
     (Power : Appoint_General_Power)
      return String
   is ("appoint_general");

   type Command_Army_Power is
     new Root_Power_Type with
      record
         Army : Concorde.Armies.Army_Type;
      end record;

   overriding function Class_Identifier
     (Power : Command_Army_Power)
      return String
   is ("command_army");

   overriding function Identifier
     (Power : Command_Army_Power)
      return String
   is ("command_army_" & Power.Army.Identifier);

   overriding function Show
     (Power : Command_Army_Power)
      return String
   is (WL.Localisation.Local_Text
       ("command_army", Power.Army.Name));

   ---------------------
   -- Appoint_General --
   ---------------------

   function Appoint_General return Power_Type is
   begin
      return Power : Appoint_General_Power;
   end Appoint_General;

   ------------------
   -- Command_Army --
   ------------------

   function Command_Army
     (Army : not null access constant
        Concorde.Armies.Root_Army_Type'Class)
      return Power_Type
   is
   begin
      return Command_Army_Power'
        (Army => Concorde.Armies.Army_Type (Army));
   end Command_Army;

end Concorde.Powers.Armies;
