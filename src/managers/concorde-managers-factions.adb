with Concorde.Objects.Queues;
with Concorde.Signals.Standard;

with Concorde.Commodities;
with Concorde.Facilities;
with Concorde.Offices;
with Concorde.Trades;
with Concorde.Worlds;

with Concorde.People.Individuals;

package body Concorde.Managers.Factions is

   ------------
   -- Create --
   ------------

   procedure Create
     (Manager : not null access Root_Faction_Manager'Class;
      Faction     : Concorde.Factions.Faction_Type)
   is
   begin
      Manager.Object := Faction;
      Manager.Faction := Faction;
      Faction.Update.Add_Handler
        (Concorde.Signals.Standard.Object_Activated,
         new Object_Activated_Handler'(Manager => Manager_Type (Manager)));
   end Create;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Faction : Concorde.Factions.Faction_Type)
      return Faction_Manager
   is
   begin
      return Manager : constant Faction_Manager := new Root_Faction_Manager do
         Manager.Create (Faction);
      end return;
   end Create_Manager;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out Root_Faction_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is
      pragma Unreferenced (Object);
   begin
      Handler.Manager.Time := Event.Time_Stamp;
      Handler.Manager.On_Activated;
   end Handle;

   ------------------
   -- On_Activated --
   ------------------

   overriding procedure On_Activated
     (Manager : in out Root_Faction_Manager)
   is
      procedure Check_Minister (Office : Concorde.Offices.Office_Type);

      --------------------
      -- Check_Minister --
      --------------------

      procedure Check_Minister (Office : Concorde.Offices.Office_Type) is
         Best_Score : Natural := 0;
         Best_Individual : Concorde.People.Individuals.Individual_Type;

         procedure Score_Individual
           (Individual : Concorde.People.Individuals.Individual_Type);

         ----------------------
         -- Score_Individual --
         ----------------------

         procedure Score_Individual
           (Individual : Concorde.People.Individuals.Individual_Type)
         is
            use type Concorde.Factions.Faction_Type;
         begin
            if Individual.Faction /= Manager.Faction then
               return;
            end if;

            declare
               Score : constant Natural :=
                         Office.Score (Individual.all);
            begin
               if Score > 0 then
                  if Score > Best_Score
                    or else (Score = Best_Score
                             and then Individual.Loyalty >
                               Best_Individual.Loyalty)
                  then
                     Best_Score := Score;
                     Best_Individual := Individual;
                  end if;
               end if;
            end;

         end Score_Individual;

      begin
         if not Manager.Faction.Has_Minister (Office) then
            Manager.Faction.Capital_World.Scan_Individuals
              (Score_Individual'Access);
         end if;
         if Best_Score > 0 then
            Manager.Faction.Log_Government
              ("appointing "
               & Best_Individual.Full_Name
               & " as "
               & Office.Name);
            Manager.Faction.Update.Set_Minister (Office, Best_Individual);

            for Responsibility in Concorde.Offices.Responsibility_Type loop
               if Office.Has_Responsibility (Responsibility) then
                  Manager.Faction.Log_Government
                    (Best_Individual.Full_Name
                     & " portfolio size"
                     & Concorde.Offices.Portfolio_Size_Range'Image
                       (Manager.Faction.Portfolio_Size
                            (Responsibility))
                     & " effectiveness"
                     & Natural'Image
                       (Natural
                            (Manager.Faction.Current_Effectiveness
                                 (Responsibility)
                             * 100.0)) & "%");
               end if;
            end loop;
         else
            Manager.Faction.Log_Government
              (Office.Name & ": no suitable candidates");
         end if;
      end Check_Minister;

   begin

      Concorde.Offices.Scan_Offices
        (Check_Minister'Access);

      Manager.Faction.Log_Government
        ("activated: "
         & Manager.Faction.Leader.Full_Name
         & " leader portfolio size"
         & Concorde.Offices.Portfolio_Size_Range'Image
           (Manager.Faction.Portfolio_Size
                (Concorde.Offices.Leader))
         & " effectiveness"
         & Natural'Image
           (Natural
                (Manager.Faction.Current_Effectiveness
                     (Concorde.Offices.Leader)
                 * 100.0)) & "%");
      Concorde.Objects.Queues.Next_Event
        (Manager.Faction, Manager.Time, Delay_Days => 1);
   end On_Activated;

end Concorde.Managers.Factions;
