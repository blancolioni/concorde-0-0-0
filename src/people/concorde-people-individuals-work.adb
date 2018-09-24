with Concorde.Quantities;

with Concorde.Powers.Armies;
with Concorde.Powers.Ministries;
with Concorde.Powers.Ships;

with Concorde.People.Communities;

package body Concorde.People.Individuals.Work is

   type Appoint_Minister_Work_Item is
     new Root_Individual_Work_Item with
      record
         Ministry : Concorde.Ministries.Ministry_Type;
      end record;

   overriding function Show (Item : Appoint_Minister_Work_Item) return String
   is ("appoint minister to " & Item.Ministry.Name);

   overriding function Power
     (Item : Appoint_Minister_Work_Item)
      return Concorde.Powers.Power_Type
   is (Concorde.Powers.Ministries.Appoint_Minister);

   overriding procedure Execute
     (Work       : Appoint_Minister_Work_Item;
      Individual : Individual_Type);

   type Appoint_General_Work_Item is
     new Root_Individual_Work_Item with
      record
         Army : Concorde.Armies.Army_Type;
      end record;

   overriding function Show (Item : Appoint_General_Work_Item) return String
   is ("appoint general to " & Item.Army.Name);

   overriding function Power
     (Item : Appoint_General_Work_Item)
      return Concorde.Powers.Power_Type
   is (Concorde.Powers.Armies.Appoint_General);

   overriding procedure Execute
     (Work       : Appoint_General_Work_Item;
      Individual : Individual_Type);

   type Appoint_Trader_Captain_Work_Item is
     new Root_Individual_Work_Item with
      record
         Ship : Concorde.Ships.Ship_Type;
      end record;

   overriding function Show
     (Item : Appoint_Trader_Captain_Work_Item)
      return String
   is ("appoint captain for trader " & Item.Ship.Name);

   overriding function Power
     (Item : Appoint_Trader_Captain_Work_Item)
      return Concorde.Powers.Power_Type
   is (Concorde.Powers.Ships.Appoint_Trader_Captain);

   overriding procedure Execute
     (Work       : Appoint_Trader_Captain_Work_Item;
      Individual : Individual_Type);

   type Pay_Work_Item is
     new Root_Individual_Work_Item with
      record
         Pop  : Concorde.People.Pops.Pop_Type;
         Wage : Concorde.Money.Price_Type;
      end record;

   overriding function Show (Item : Pay_Work_Item) return String
   is ("pay wage " & Concorde.Money.Show (Item.Wage) & " to "
       & Concorde.Quantities.Show (Item.Pop.Size_Quantity)
       & " " & Item.Pop.Group.Name & "; total "
       & Concorde.Money.Show
         (Concorde.Money.Total (Item.Wage, Item.Pop.Size_Quantity)));

   overriding function Power
     (Item : Pay_Work_Item)
      return Concorde.Powers.Power_Type
   is (Concorde.Powers.Ministries.Pay (Item.Pop.Group));

   overriding procedure Execute
     (Work       : Pay_Work_Item;
      Individual : Individual_Type);

   function Best_Candidate
     (Community : Concorde.People.Communities.Community_Type;
      Powers    : Concorde.Powers.Powered_Interface'Class)
      return Concorde.People.Individuals.Individual_Type;

   ---------------------
   -- Appoint_General --
   ---------------------

   function Appoint_General
     (Army : not null access constant
        Concorde.Armies.Root_Army_Type'Class)
      return Concorde.Work.Work_Item
   is
   begin
      return new Appoint_General_Work_Item'
        (Concorde.Work.Root_Work_Item with
           Army => Concorde.Armies.Army_Type (Army));
   end Appoint_General;

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

   ----------------------------
   -- Appoint_Trader_Captain --
   ----------------------------

   function Appoint_Trader_Captain
     (Ship : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
      return Concorde.Work.Work_Item
   is
   begin
      return new Appoint_Trader_Captain_Work_Item'
        (Concorde.Work.Root_Work_Item with
           Ship => Concorde.Ships.Ship_Type (Ship));
   end Appoint_Trader_Captain;

   --------------------
   -- Best_Candidate --
   --------------------

   function Best_Candidate
     (Community : Concorde.People.Communities.Community_Type;
      Powers    : Concorde.Powers.Powered_Interface'Class)
      return Concorde.People.Individuals.Individual_Type
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
         if Candidate.Faction /= Community.Owner then
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
            begin
               for I in 1 .. Power.Attribute_Count loop
                  Score := Score
                    + Concorde.People.Attributes.Attribute_Score
                    (Candidate.all, Power.Attribute (I));
               end loop;
            end Score_Power;

         begin
            Powers.Scan_Powers (Score_Power'Access);

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
      Community.Scan_Individuals (Score_Individual'Access);

      return Best_Individual;
   end Best_Candidate;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Work       : Appoint_Minister_Work_Item;
      Individual : Individual_Type)
   is
      New_Minister : constant Concorde.People.Individuals.Individual_Type :=
                       Best_Candidate
                         (Community  => Individual.Faction.Capital_Community,
                          Powers     => Work.Ministry.all);
   begin
      if New_Minister = null then
         Individual.Log_Government
           ("no suitable candidates");
      else
         Individual.Log_Government
           ("appointing "
            & New_Minister.Full_Name
            & " to run "
            & Work.Ministry.Identifier
            & " "
            & Work.Ministry.Name);
         Individual.Faction.Update.Set_Minister
           (Work.Ministry, New_Minister);
      end if;

   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Work       : Appoint_General_Work_Item;
      Individual : Individual_Type)
   is
      Power : constant Concorde.Powers.Power_Type :=
                Concorde.Powers.Armies.Command_Army
                  (Army => Work.Army);
      Set : Concorde.Powers.Power_Set;
      General : Concorde.People.Individuals.Individual_Type;
   begin
      Set.Add_Power (Power);
      General :=
        Best_Candidate
          (Community => Individual.Faction.Capital_Community,
           Powers    => Set);

      if General /= null then
         Individual.Log_Government
           ("appointing "
            & General.Full_Name
            & " to command of "
            & Work.Army.Name);
      end if;

   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Work       : Appoint_Trader_Captain_Work_Item;
      Individual : Individual_Type)
   is
      Power   : constant Concorde.Powers.Power_Type :=
                  Concorde.Powers.Ships.Captain_Trader_Ship (Work.Ship);
      Set     : Concorde.Powers.Power_Set;
      Captain : Concorde.People.Individuals.Individual_Type;
   begin
      Set.Add_Power (Power);
      Captain :=
        Best_Candidate
          (Community => Individual.Faction.Capital_Community,
           Powers    => Set);

      if Captain /= null then
         Individual.Log_Government
           ("appointing "
            & Captain.Full_Name
            & " as captain of trade ship "
            & Work.Ship.Name);
      end if;

   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Work       : Pay_Work_Item;
      Individual : Individual_Type)
   is
      use Concorde.Money;
      Ministry : constant Concorde.Ministries.Ministry_Type :=
                   Concorde.Ministries.Ministry_Type
                     (Individual.Office);
      Wages    : constant Money_Type :=
                   Total (Work.Wage, Work.Pop.Size_Quantity);
   begin
      Ministry.Log
        ("pay " & Concorde.Quantities.Show (Work.Pop.Size_Quantity)
         & " " & Work.Pop.Group.Identifier
         & " " & Show (Wages));
      Ministry.Update.Remove_Cash (Wages);
      Work.Pop.Update.Add_Cash (Wages);
   end Execute;

   ------------------------
   -- Pay_State_Employee --
   ------------------------

   function Pay_State_Employee
     (Pop  : Concorde.People.Pops.Pop_Type;
      Wage : Concorde.Money.Price_Type)
      return Concorde.Work.Work_Item
   is
   begin
      return new Pay_Work_Item'
        (Concorde.Work.Root_Work_Item with
           Pop  => Pop,
           Wage => Wage);
   end Pay_State_Employee;

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
