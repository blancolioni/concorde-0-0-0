with WL.Random;

with Concorde.Names;
with Concorde.Random;

with WL.Money;
with WL.Quantities;

with Concorde.Markets;
with Concorde.Worlds;

package body Concorde.People.Individuals.Create is

   procedure Random_Features
     (Individual : in out Root_Individual_Type'Class);

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
         Item.Faction := Faction;
         Item.Loyalty := 1.0;
         Item.Set_Cash (WL.Money.To_Money (1_000.0));
         Item.New_Agent
           (Location, Market, WL.Quantities.To_Quantity (1000.0));
      end Create;

   begin
      return Db.Create (Create'Access);
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
         Item.Faction := Loyalty;
         Item.Loyalty := Concorde.Random.Unit_Random;
         Item.Set_Cash (WL.Money.To_Money (100.0));
         Item.New_Agent
           (Location, Market, WL.Quantities.To_Quantity (1000.0));
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
      Individual.First_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (if Individual.Gender = Female
           then Concorde.Names.Random_Female_First_Name
           else Concorde.Names.Random_Male_First_Name);
   end Random_Features;

end Concorde.People.Individuals.Create;
