with Concorde.Powers.Execution;
with Concorde.Worlds;

package body Concorde.People.Individuals.Work is

   type Appoint_Minister_Work_Item is
     new Root_Individual_Work_Item with
      record
         Ministry : Concorde.Ministries.Ministry_Type;
      end record;

   overriding function Show (Item : Appoint_Minister_Work_Item) return String
   is ("appoint minister to " & Item.Ministry.Name);

   overriding function Cost (Item : Appoint_Minister_Work_Item) return Duration
   is (Concorde.Calendar.Days (5));

   overriding procedure Execute
     (Work       : Appoint_Minister_Work_Item;
      Individual : Individual_Type);

   ----------------------
   -- Appoint_Minister --
   ----------------------

   function Appoint_Minister
     (Ministry : not null access constant
        Concorde.Ministries.Root_Ministry_Type'Class)
      return Concorde.Work.Work_Item
   is
   begin
      return new Appoint_Minister_Work_Item'
        (Concorde.Work.Root_Work_Item with
           Ministry => Concorde.Ministries.Ministry_Type (Ministry));
   end Appoint_Minister;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Work       : Appoint_Minister_Work_Item;
      Individual : Individual_Type)
   is

      Best_Score      : Natural := 0;
      Best_Individual : Concorde.People.Individuals.Individual_Type;

      procedure Score_Individual
        (Candidate : Concorde.People.Individuals.Individual_Type);

      ----------------------
      -- Score_Individual --
      ----------------------

      procedure Score_Individual
        (Candidate : Concorde.People.Individuals.Individual_Type)
      is
         use type Concorde.Factions.Faction_Type;
      begin
         if Candidate.Faction /= Individual.Faction then
            return;
         end if;

         if Candidate.Has_Office then
            return;
         end if;

         declare
            Score : Natural := 0;

            procedure Score_Power (Power : Concorde.Powers.Power_Type);

            -----------------
            -- Score_Power --
            -----------------

            procedure Score_Power (Power : Concorde.Powers.Power_Type) is
               use Concorde.Powers.Execution;
            begin
               for I in 1 .. Attribute_Count (Power) loop
                  Score := Score
                    + Concorde.People.Attributes.Attribute_Score
                    (Candidate.all, Attribute (Power, I));
               end loop;
            end Score_Power;

         begin
            Work.Ministry.Scan_Powers (Score_Power'Access);

            if Score > 0 then
               if Score > Best_Score
                 or else (Score = Best_Score
                          and then Candidate.Loyalty >
                            Best_Individual.Loyalty)
               then
                  Best_Score := Score;
                  Best_Individual := Candidate;
               end if;
            end if;
         end;

      end Score_Individual;

   begin

      Individual.Faction.Capital_World.Scan_Individuals
        (Score_Individual'Access);

      if Best_Score > 0 then
         Individual.Log_Government
           ("appointing "
            & Best_Individual.Full_Name
            & " to run "
            & Work.Ministry.Name);
         Individual.Faction.Update.Set_Minister
           (Work.Ministry, Best_Individual);
      end if;

   end Execute;

   ------------------
   -- Perform_Work --
   ------------------

   procedure Perform_Work
     (Individual : Individual_Type;
      Work       : Concorde.Work.Work_Item)
   is
   begin
      Root_Individual_Work_Item'Class (Work.all).Execute (Individual);
   end Perform_Work;

end Concorde.People.Individuals.Work;
