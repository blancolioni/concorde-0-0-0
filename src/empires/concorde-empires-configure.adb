with Tropos.Reader;
with Concorde.Paths;

with Concorde.Options;

with Concorde.Players.Registry;

with Concorde.Empires.Create;

with Lui.Colours;

with Concorde.Systems.Tests;

with Concorde.Empires.Db;
with Concorde.Systems.Db;

package body Concorde.Empires.Configure is

   Imperial_Centre : constant Boolean := True;

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
            Player  : constant String :=
                        (if Imperial_Centre
                         and then Current = 0
                         then "null"
                         else Config.Get ("player", "simple"));
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
                   Config.Get ("design", "defender"),
                 Player              =>
                   Concorde.Players.Registry.Get (Player));
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
        (System : in out Concorde.Systems.Root_Star_System_Type'Class);

      procedure Bad_Relationship
        (Empire : in out Root_Empire_Type'Class);

      --------------------
      -- Add_Test_Ships --
      --------------------

      procedure Add_Test_Ships
        (System : in out Concorde.Systems.Root_Star_System_Type'Class)
      is
      begin
         for I in 1 .. 8 loop
            Concorde.Systems.Tests.New_Test_Ship
              (Defender, System, Design => "defender");
         end loop;

         for I in 1 .. 8 loop
            Concorde.Systems.Tests.New_Test_Ship
              (Attacker, System, Design => "dromon");
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
      Concorde.Systems.Db.Update
        (Defender.Capital.Reference, Add_Test_Ships'Access);

      Db.Update (Attacker.Reference, Bad_Relationship'Access);
      Db.Update (Defender.Reference, Bad_Relationship'Access);
   end Create_Test_Battle;

end Concorde.Empires.Configure;
