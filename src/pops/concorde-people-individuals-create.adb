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

with Concorde.Options;

package body Concorde.People.Individuals.Create is

   procedure Random_Features
     (Individual : in out Root_Individual_Type'Class);

   procedure Create_Career
     (Individual : in out Root_Individual_Type'Class);

   function Choose_Career
     (Individual : Root_Individual_Type'Class)
      return Concorde.People.Careers.Career_Type;

   -------------------
   -- Choose_Career --
   -------------------

   function Choose_Career
     (Individual : Root_Individual_Type'Class)
      return Concorde.People.Careers.Career_Type
   is
      use Concorde.People.Careers;
      Best : Career_Type := null;

      procedure Check_Career (Career : Career_Type);

      ------------------
      -- Check_Career --
      ------------------

      procedure Check_Career (Career : Career_Type) is
      begin
         if not Individual.Has_Career (Career)
           and then Individual.Qualified (Career)
         then
            if Best = null or else Best.Prestige < Career.Prestige then
               Best := Career;
            end if;
         end if;
      end Check_Career;

   begin

      Concorde.People.Careers.Scan_Careers (Check_Career'Access);
      return Best;
   end Choose_Career;

   -------------------
   -- Create_Career --
   -------------------

   procedure Create_Career
     (Individual : in out Root_Individual_Type'Class)
   is
      use Concorde.Calendar;
      use Concorde.People.Careers;
      Date : Time := Individual.Birth + Days (18 * 360);
      Current_Career : Career_Type := null;
      Current_Rank   : Rank_Index := 1;
      Current_Start  : Time;
      Rounds_At_Rank : Natural := 0;
   begin
      while Date < Clock loop
         if Individual.Education < 5 then
            Individual.Education := Individual.Education + 1;
            Date := Date + Days (360);
         else
            if Current_Career = null then
               Current_Career := Choose_Career (Individual);

               if Current_Career = null then
                  exit;
               end if;

               Current_Rank := 1;
               Rounds_At_Rank := 0;
               Current_Start := Date;

               Ada.Text_IO.Put_Line
                 (Image (Date)
                  & ": " & Individual.Full_Name & ": new career: "
                  & Current_Career.Name
                  & " rank " & Current_Career.Rank_Name (Current_Rank));
            else
               declare
                  Chance : constant Unit_Real :=
                             Current_Career.Promotion_Chance
                               (Current_Rank, Individual);
               begin
                  if Concorde.Random.Unit_Random < Chance then
                     Current_Rank := Current_Rank + 1;
                     Ada.Text_IO.Put_Line
                       (Image (Date)
                        & ": " & Individual.Full_Name & ": promoted to "
                        & Current_Career.Rank_Name (Current_Rank));
                     Rounds_At_Rank := 0;
                  else
                     Rounds_At_Rank := Rounds_At_Rank + 1;
                     if Rounds_At_Rank > 5 then
                        Ada.Text_IO.Put_Line
                          (Image (Date)
                           & ": " & Individual.Full_Name & " resigns");
                        exit;
                     end if;
                  end if;
               end;
            end if;

            if Current_Career /= null then
               for Skill of Current_Career.Rank_Skills (Current_Rank) loop
                  Individual.Improve_Skill (Skill);
               end loop;
               Individual.Education := Individual.Education + 1;
            end if;

            Date := Date + Days (4 * 360);
         end if;
      end loop;

      if Current_Career /= null then
         Individual.Career.Append
           (Career_Record'
              (Career => Current_Career,
               Start  => Current_Start,
               Finish => Date,
               Rank   => Current_Rank));
         if Current_Career.Titles then
            Individual.Title :=
              Ada.Strings.Unbounded.To_Unbounded_String
                (Current_Career.Rank_Name (Current_Rank));
         end if;
      end if;

   end Create_Career;

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

         Create_Career (Item);

      end Create;

   begin
      return Individual : constant Individual_Type :=
        Db.Create (Create'Access)
      do
         if Concorde.Options.Write_Character_Portraits then
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

         Create_Career (Item);

      end Create;

   begin
      return Individual : constant Individual_Type :=
        Db.Create (Create'Access)
      do
         if Concorde.Options.Write_Character_Portraits then
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
               & ")"
               & " and " & Parent_2.Full_Name
               & " (age" & Natural'Image (Parent_2.Age)
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

         Create_Career (Item);

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
                     Genetics.Get_Gene
                       (Concorde.People.Abilities.Ability_Type'Image
                          (Ability));
         begin
            Individual.Abilities (Ability) :=
              Concorde.People.Abilities.Ability_Score_Range
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
