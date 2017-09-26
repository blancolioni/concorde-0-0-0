with Ada.Text_IO;

with Tropos.Reader;
with Concorde.Paths;

with Concorde.Options;

with Concorde.Empires.Create;

with Lui.Colours;

with Concorde.Worlds.Tests;

package body Concorde.Empires.Configure is

   procedure Create_Test_Battle
     (Attacker, Defender : Empire_Type);

   --------------------
   -- Create_Empires --
   --------------------

   procedure Create_Empires (Count : Natural) is
      Empire_Config : constant Tropos.Configuration :=
                        Tropos.Reader.Read_Config
                          (Concorde.Paths.Config_File ("empires.txt"));
      Current       : Natural := 0;
      E1, E2        : Empire_Type := null;
   begin
      for Config of Empire_Config loop
         exit when Current >= Count;
         Ada.Text_IO.Put_Line ("New empire: " & Config.Config_Name);
         declare
            Name : constant String :=
                     Config.Get ("name", Config.Config_Name);
            Capital : constant String :=
                        Config.Get ("capital", Name);
            Colour  : constant Tropos.Configuration :=
                        Config.Child ("colour");
            Red     : constant Natural := Colour.Get (1);
            Green   : constant Natural := Colour.Get (2);
            Blue    : constant Natural := Colour.Get (3);
            E       : Empire_Type;
         begin
            E :=
              Concorde.Empires.Create.New_Empire
                (Name    => Name,
                 Capital => Capital,
                 Colour  =>
                   Lui.Colours.To_Colour
                     (Lui.Colours.Colour_Byte (Red),
                      Lui.Colours.Colour_Byte (Green),
                      Lui.Colours.Colour_Byte (Blue)),
                 Default_Ship_Design =>
                   Config.Get ("design", "defender"));
            if E1 = null then
               E1 := E;
            elsif E2 = null then
               E2 := E;
            end if;
         end;

         Current := Current + 1;
      end loop;

      if Concorde.Options.Test_Battle then
         Create_Test_Battle (E1, E2);
      end if;

   end Create_Empires;

   ------------------------
   -- Create_Test_Battle --
   ------------------------

   procedure Create_Test_Battle
     (Attacker, Defender : Empire_Type)
   is

      procedure Add_Test_Ships
        (World : Concorde.Worlds.Updateable_Reference);

      procedure Bad_Relationship
        (Empire : in out Root_Empire_Type'Class);

      --------------------
      -- Add_Test_Ships --
      --------------------

      procedure Add_Test_Ships
        (World : Concorde.Worlds.Updateable_Reference)
      is
      begin
         for I in 1 .. 8 loop
            Concorde.Worlds.Tests.New_Test_Ship
              (Defender, World.Item.all, Design => "defender");
         end loop;

         for I in 1 .. 8 loop
            Concorde.Worlds.Tests.New_Test_Ship
              (Attacker, World, Design => "dromon");
         end loop;
      end Add_Test_Ships;

      ----------------------
      -- Bad_Relationship --
      ----------------------

      procedure Bad_Relationship
        (Empire : in out Root_Empire_Type'Class)
      is
      begin
         if Empire.Name = Attacker.Name then
            Empire.Set_Relationship (Defender.all, -100);
         else
            Empire.Set_Relationship (Attacker.all, -100);
         end if;
      end Bad_Relationship;

   begin
      Add_Test_Ships (Defender.Capital_World.Update);

      Db.Update (Attacker.Reference, Bad_Relationship'Access);
      Db.Update (Defender.Reference, Bad_Relationship'Access);
   end Create_Test_Battle;

end Concorde.Empires.Configure;
