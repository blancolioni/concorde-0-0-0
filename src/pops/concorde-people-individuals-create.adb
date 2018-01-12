with Ada.Text_IO;

with WL.Random;

with WL.Money;
with WL.Quantities;

with Concorde.Names;
with Concorde.Random;

with Concorde.Government;
with Concorde.Markets;
with Concorde.Worlds;

with Concorde.People.Individuals.Portraits;
with Concorde.People.Individuals.Report;

package body Concorde.People.Individuals.Create is

   procedure Random_Features
     (Individual : in out Root_Individual_Type'Class);

   ------------------
   -- Create_Child --
   ------------------

   function Create_Child
     (Parent_1, Parent_2 : not null access constant Root_Individual_Type'Class;
      Location           : Concorde.Locations.Object_Location;
      Date_Of_Birth      : Concorde.Calendar.Time)
      return Individual_Type
   is
      procedure Create (Item : in out Root_Individual_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Item : in out Root_Individual_Type'Class) is
         Market : constant Concorde.Markets.Market_Type :=
                    (if Concorde.Locations.Is_World_Location (Location)
                     then Concorde.Locations.Current_World (Location).Market
                     else null);
      begin
         Item.Last_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Parent_1.Faction.Name);
         Item.DNA := Genetics.Merge (Parent_1.DNA, Parent_2.DNA, 0.01);
         Random_Features (Item);
         Item.Faction := Parent_1.Faction;
         Item.Citizenship := Parent_1.Faction;
         Item.Loyalty := 1.0;
         Item.Birth := Date_Of_Birth;
         Item.Alive := True;
         Item.New_Agent
           (Location, Concorde.Government.Get_Government (Location),
            Market, WL.Money.Zero,
            WL.Quantities.To_Quantity (1000.0));

      end Create;

   begin
      return Individual : constant Individual_Type :=
        Db.Create (Create'Access)
      do
         if True then
            Concorde.People.Individuals.Portraits.Save_Portrait
              (Individual,
               "portraits/"
               & Ada.Strings.Unbounded.To_String (Individual.Last_Name)
               & "-"
               & Ada.Strings.Unbounded.To_String (Individual.First_Name)
               & ".png");
         end if;
      end return;
   end Create_Child;

   --------------------------
   -- Create_Family_Member --
   --------------------------

   function Create_Family_Member
     (Faction       : Concorde.Factions.Faction_Type;
      Location      : Concorde.Locations.Object_Location;
      Date_Of_Birth : Concorde.Calendar.Time)
      return Individual_Type
   is
      procedure Create (Item : in out Root_Individual_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Item : in out Root_Individual_Type'Class) is
         Market : constant Concorde.Markets.Market_Type :=
                    (if Concorde.Locations.Is_World_Location (Location)
                     then Concorde.Locations.Current_World (Location).Market
                     else null);
      begin
         Item.Last_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Faction.Name);
         Item.DNA := Genetics.Random_Genome;
         Random_Features (Item);
         Item.Faction := Faction;
         Item.Citizenship := Faction;
         Item.Loyalty := 1.0;
         Item.Birth := Date_Of_Birth;
         Item.Alive := True;
         Item.New_Agent
           (Location, Concorde.Government.Get_Government (Location),
            Market, WL.Money.To_Money (1_000.0),
            WL.Quantities.To_Quantity (1000.0));
      end Create;

   begin
      return Individual : constant Individual_Type :=
        Db.Create (Create'Access)
      do
         if True then
            Concorde.People.Individuals.Portraits.Save_Portrait
              (Individual,
               "portraits/"
               & Ada.Strings.Unbounded.To_String (Individual.Last_Name)
               & "-"
               & Ada.Strings.Unbounded.To_String (Individual.First_Name)
               & ".png");
         end if;
      end return;
   end Create_Family_Member;

   ------------------------
   -- Create_Family_Tree --
   ------------------------

   procedure Create_Family_Tree
     (Faction    : Concorde.Factions.Faction_Type;
      Location   : Concorde.Locations.Object_Location)
   is
      use Concorde.Calendar;

      Now : constant Time := Clock;

      function Random_Years (Start_Year, End_Year : Natural) return Duration;

      function Random_Ancestor_Past_Duration return Duration;

      procedure Create_Generation
        (Parent_1, Parent_2    : Individual_Type;
         Remaining_Generations : Natural);

      -----------------------
      -- Create_Generation --
      -----------------------

      procedure Create_Generation
        (Parent_1, Parent_2    : Individual_Type;
         Remaining_Generations : Natural)
      is
         Recent_DOB : constant Time :=
                        (if Parent_1.Birth > Parent_2.Birth
                         then Parent_1.Birth else Parent_2.Birth);
         Start      : Time :=
                        Recent_DOB + Random_Years (20, 30);
         Child_Count : Natural := 0;
      begin
         while WL.Random.Random_Number (0, 8) > Child_Count loop
            Child_Count := Child_Count + 1;
         end loop;

         Child_Count := Child_Count + Remaining_Generations;

         declare
            Children : array (1 .. Child_Count) of Individual_Type;
         begin
            for I in 1 .. Child_Count loop

               if Start > Now then
                  Child_Count := I - 1;
                  exit;
               end if;

               declare
                  Child : constant Individual_Type :=
                            Create_Child (Parent_1, Parent_2, Location, Start);
               begin
                  Children (I) := Child;
                  Start := Start + Random_Years (1, 2);
               end;
            end loop;

            Ada.Text_IO.Put_Line
              (Parent_1.Full_Name
               & " (age" & Natural'Image (Parent_1.Age)
               & " empathy" & Parent_1.Abilities (Empathy)'Img
               & ")"
               & " and " & Parent_2.Full_Name
               & " (age" & Natural'Image (Parent_2.Age)
               & " empathy" & Parent_2.Abilities (Empathy)'Img
               & ")"
               & " have"
               & (if Child_Count = 0 then " no children"
                 elsif Child_Count = 1 then " one child"
                 else Child_Count'Img & " children"));

            for Child of Children (1 .. Child_Count) loop
               Report.Report (Child);
            end loop;

            if Remaining_Generations > 0 then
               for Child of Children (1 .. Child_Count) loop
                  if Child.Age > 20 then
                     declare
                        Spouse : constant Individual_Type :=
                                   Create_Family_Member
                                     (Faction, Location, Child.Birth);
                     begin
                        Create_Generation (Child, Spouse,
                                           Remaining_Generations - 1);
                     end;
                  end if;
               end loop;
            end if;
         end;

      end Create_Generation;

      -----------------------------------
      -- Random_Ancestor_Past_Duration --
      -----------------------------------

      function Random_Ancestor_Past_Duration return Duration is
         Min_Years : constant Natural := 60;
         Max_Years : constant Natural := 80;
      begin
         return Random_Years (Min_Years, Max_Years);
      end Random_Ancestor_Past_Duration;

      ------------------
      -- Random_Years --
      ------------------

      function Random_Years
        (Start_Year, End_Year : Natural)
         return Duration
      is
         Duration_Range : constant Natural :=
                            (End_Year - Start_Year)
                            * 84_600 * 365;
         Seconds        : constant Positive :=
                            WL.Random.Random_Number (1, Duration_Range);
      begin
         return Duration (Seconds)
           + Duration (Start_Year) * Duration (86_400.0 * 365.0);
      end Random_Years;

      X_DOB : constant Time :=
                Clock - Random_Ancestor_Past_Duration;
      Y_DOB : constant Time :=
                Clock - Random_Ancestor_Past_Duration;

      X : constant Individual_Type :=
            Create_Family_Member
              (Faction, Location, X_DOB);
      Y : constant Individual_Type :=
            Create_Family_Member
              (Faction, Location, Y_DOB);
   begin
      Report.Report (X);
      Report.Report (Y);
      Create_Generation (X, Y, 2);
   end Create_Family_Tree;

   ------------------------------
   -- Create_Random_Individual --
   ------------------------------

   procedure Create_Random_Individual
     (Loyalty       : Concorde.Factions.Faction_Type;
      Location      : Concorde.Locations.Object_Location;
      Date_Of_Birth : Concorde.Calendar.Time)
   is
      procedure Create (Item : in out Root_Individual_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Item : in out Root_Individual_Type'Class) is
         Market : constant Concorde.Markets.Market_Type :=
                    (if Concorde.Locations.Is_World_Location (Location)
                     then Concorde.Locations.Current_World (Location).Market
                     else null);
      begin
         Item.Last_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Concorde.Names.Random_Last_Name);
         Item.DNA := Genetics.Random_Genome;
         Random_Features (Item);
         Item.Faction := Loyalty;
         Item.Citizenship := Loyalty;
         Item.Loyalty := Concorde.Random.Unit_Random;
         Item.Birth := Date_Of_Birth;
         Item.New_Agent
           (Location, Concorde.Government.Get_Government (Location),
            Market, WL.Money.To_Money (100.0),
            WL.Quantities.To_Quantity (1000.0));
      end Create;

   begin
      Db.Create (Create'Access);
   end Create_Random_Individual;

   ---------------------
   -- Random_Features --
   ---------------------

   procedure Random_Features
     (Individual : in out Root_Individual_Type'Class)
   is
   begin
      for Ability in Individual.Abilities'Range loop
         declare
            Gene : constant Genetics.Gene_Type :=
                     Genetics.Get_Gene (Ability_Type'Image (Ability));
         begin
            Individual.Abilities (Ability) :=
              Ability_Score_Range
                (Genetics.Express (Individual.DNA, Gene));
         end;
      end loop;

      Individual.Gender :=
        (if WL.Random.Random_Number (1, 2) = 1 then Female else Male);
      Individual.Set_Name
        (if Individual.Gender = Female
         then Concorde.Names.Random_Female_First_Name
         else Concorde.Names.Random_Male_First_Name);
   end Random_Features;

end Concorde.People.Individuals.Create;
