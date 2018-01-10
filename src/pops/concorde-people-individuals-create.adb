with WL.Random;

with Concorde.Names;
with Concorde.Random;

with WL.Money;
with WL.Quantities;

with Concorde.Government;
with Concorde.Markets;
with Concorde.Worlds;

with Concorde.People.Individuals.Portraits;
with Ada.Text_IO;

package body Concorde.People.Individuals.Create is

   procedure Random_Features
     (Individual : in out Root_Individual_Type'Class);

   ------------------
   -- Create_Child --
   ------------------

   function Create_Child
     (Parent_1, Parent_2 : not null access constant Root_Individual_Type'Class;
      Location           : Concorde.Locations.Object_Location)
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
         Random_Features (Item);
         Item.Last_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Parent_1.Faction.Name);
         Item.DNA := Genetics.Merge (Parent_1.DNA, Parent_2.DNA, 0.01);
         Item.Faction := Parent_1.Faction;
         Item.Citizenship := Parent_1.Faction;
         Item.Loyalty := 1.0;
         Item.New_Agent
           (Location, Concorde.Government.Get_Government (Location),
            Market, WL.Money.Zero,
            WL.Quantities.To_Quantity (1000.0));
      end Create;

   begin
      return Db.Create (Create'Access);
   end Create_Child;

   --------------------------
   -- Create_Family_Member --
   --------------------------

   function Create_Family_Member
     (Faction    : Concorde.Factions.Faction_Type;
      Location   : Concorde.Locations.Object_Location)
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
         Random_Features (Item);
         Item.Last_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Faction.Name);
         Item.DNA := Genetics.Random_Genome;
         Item.Faction := Faction;
         Item.Citizenship := Faction;
         Item.Loyalty := 1.0;
         Item.New_Agent
           (Location, Concorde.Government.Get_Government (Location),
            Market, WL.Money.To_Money (1_000.0),
            WL.Quantities.To_Quantity (1000.0));
         Ada.Text_IO.Put_Line
           (Full_Name (Item) & ": eyes"
            & Genetics.Gene_Expression'Image
              (Genetics.Express (Item.DNA, Genetics.Eyes)));
      end Create;

   begin
      return Individual : constant Individual_Type :=
        Db.Create (Create'Access)
      do
         Concorde.People.Individuals.Portraits.Save_Portrait
           (Individual,
            "portraits/"
            & Ada.Strings.Unbounded.To_String (Individual.Last_Name)
            & "-"
            & Ada.Strings.Unbounded.To_String (Individual.First_Name)
            & ".png");
      end return;

   end Create_Family_Member;

   ------------------------------
   -- Create_Random_Individual --
   ------------------------------

   procedure Create_Random_Individual
     (Loyalty  : Concorde.Factions.Faction_Type;
      Location : Concorde.Locations.Object_Location)
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
         Random_Features (Item);
         Item.Last_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Concorde.Names.Random_Last_Name);
         Item.DNA := Genetics.Random_Genome;
         Item.Faction := Loyalty;
         Item.Citizenship := Loyalty;
         Item.Loyalty := Concorde.Random.Unit_Random;
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
      for Score of Individual.Scores loop
         Score := Score_Range (WL.Random.Random_Number (1, 3));
         for I in 1 .. 3 loop
            exit when WL.Random.Random_Number (1, 10) < 10;
            Score := Score + Score_Range (WL.Random.Random_Number (1, 3)) - 1;
         end loop;
      end loop;
      Individual.Gender :=
        (if WL.Random.Random_Number (1, 2) = 1 then Female else Male);
      Individual.Set_Name
        (if Individual.Gender = Female
         then Concorde.Names.Random_Female_First_Name
         else Concorde.Names.Random_Male_First_Name);
   end Random_Features;

end Concorde.People.Individuals.Create;
