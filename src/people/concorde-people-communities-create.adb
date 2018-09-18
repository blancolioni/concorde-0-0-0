with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with WL.Money;
with WL.String_Sets;

--  with Concorde.Real_Images;
with Concorde.Elementary_Functions;
with Concorde.Random;

with Concorde.Objects.Queues;

with Concorde.People.Groups;
with Concorde.People.Pops.Create;

with Concorde.Metrics;
with Concorde.Policies;
with Concorde.Government.Create;

with Concorde.Managers.Communities;

package body Concorde.People.Communities.Create is

   procedure Create_Initial_Pops
     (Community : in out Root_Community_Type'Class;
      Pop_Size  : Non_Negative_Real;
      Apathy    : Unit_Real;
      Gini      : Unit_Real;
      Total_Pop : Non_Negative_Real;
      Initial_Value : not null access
        function (Parameter_Name : String) return Real);

   procedure Create_Network_State
     (Community     : in out Root_Community_Type'Class;
      Initial_Value : not null access
        function (Parameter_Name : String) return Real);

   -------------------------
   -- Create_Initial_Pops --
   -------------------------

   procedure Create_Initial_Pops
     (Community : in out Root_Community_Type'Class;
      Pop_Size  : Non_Negative_Real;
      Apathy    : Unit_Real;
      Gini      : Unit_Real;
      Total_Pop : Non_Negative_Real;
      Initial_Value : not null access
        function (Parameter_Name : String) return Real)
   is
      use Concorde.Elementary_Functions;

      type Income_Group is
         record
            Group           : Concorde.People.Groups.Pop_Group;
            Income          : Non_Negative_Real;
            Relative_Income : Unit_Real;
            Proportion      : Unit_Real;
         end record;

      package Income_Group_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Income_Group);

      function Less_Than (Left, Right : Income_Group) return Boolean
      is (Left.Income < Right.Income);

      package Income_Group_Sorting is
        new Income_Group_Lists.Generic_Sorting (Less_Than);

      Income_Groups     : Income_Group_Lists.List;

      Total_Income     : Non_Negative_Real := 0.0;

      Everybody    : constant Concorde.People.Groups.Pop_Group :=
                       Concorde.People.Groups.Get ("everybody");

      procedure Calculate_Distribution;

      function Recalculate_Gini (Power : Non_Negative_Real) return Unit_Real;

      function Current_Gini return Unit_Real;

      function Default_Group_Proportion
        (Group : Concorde.People.Groups.Pop_Group)
         return Unit_Real;

      ----------------------------
      -- Calculate_Distribution --
      ----------------------------

      procedure Calculate_Distribution is
         Low  : Non_Negative_Real := 0.5;
         High : Non_Negative_Real := 1.5;
      begin
         loop
            declare
               Power : constant Non_Negative_Real := (High + Low) / 2.0;
               G     : constant Unit_Real :=
                         Recalculate_Gini (Power);
            begin
               if abs (G - Gini) < 0.005 then
                  Ada.Text_IO.Put_Line
                    ("power:" & Natural'Image (Natural (Power * 100.0)) & "%");
                  exit;
               elsif G < Gini then
                  High := Power;
               else
                  Low := Power;
               end if;
            end;
         end loop;

      end Calculate_Distribution;

      ------------------
      -- Current_Gini --
      ------------------

      function Current_Gini return Unit_Real is
         B            : Unit_Real := 0.0;
         Total_Income : Non_Negative_Real := 0.0;
      begin

         for IG of Income_Groups loop
            Total_Income := Total_Income
              + IG.Income * IG.Proportion * Total_Pop;
         end loop;

         for IG of Income_Groups loop
            B := B
              + IG.Proportion
              * (IG.Proportion * Total_Pop * IG.Income / Total_Income)
              * 0.7;
         end loop;

         return 1.0 - 2.0 * Unit_Real'Min (B, 0.5);
      end Current_Gini;

      ------------------------------
      -- Default_Group_Proportion --
      ------------------------------

      function Default_Group_Proportion
        (Group : Concorde.People.Groups.Pop_Group)
         return Unit_Real
      is
         P : constant Unit_Real := Initial_Value (Group.Identifier);
      begin
         if P = 0.0 then
            return Group.Default_Proportion;
         else
            return P;
         end if;
      end Default_Group_Proportion;

      ----------------------
      -- Recalculate_Gini --
      ----------------------

      function Recalculate_Gini
        (Power : Non_Negative_Real)
         return Unit_Real
      is

         procedure Distribute_Pops;

         ---------------------
         -- Distribute_Pops --
         ---------------------

         procedure Distribute_Pops is
            Total_Income     : Non_Negative_Real := 0.0;
            Total_Proportion : Non_Negative_Real := 0.0;
         begin

            for IG of Income_Groups loop
               Total_Income := Total_Income + IG.Income;
            end loop;

            for IG of Income_Groups loop
               Total_Proportion := Total_Proportion +
                 (Total_Income / IG.Income) ** Power;
            end loop;

            for IG of Income_Groups loop
               IG.Proportion :=
                 (Total_Income / IG.Income) ** Power
                 / Total_Proportion;
            end loop;
         end Distribute_Pops;

      begin
         Distribute_Pops;
         return Current_Gini;
      end Recalculate_Gini;

   begin
      Ada.Text_IO.Put_Line
        ("Total population:"
         & Natural'Image (Natural (Total_Pop)));

      Ada.Text_IO.Put_Line
        ("Pop size:"
         & Natural'Image (Natural (Pop_Size)));

      declare
         Min_Income : Non_Negative_Real := Non_Negative_Real'Last;
         Max_Income : Non_Negative_Real := Non_Negative_Real'First;
      begin
         for Group of Concorde.People.Groups.Wealth_Groups loop
            declare
               Income      : constant Non_Negative_Real :=
                               Initial_Value (Group.Identifier & "-income");
            begin
               if Income < Min_Income then
                  Min_Income := Income;
               end if;
               if Income > Max_Income then
                  Max_Income := Income;
               end if;
               Income_Groups.Append
                 (Income_Group'
                    (Group           => Group,
                     Income          => Income,
                     Relative_Income => 0.0,
                     Proportion      => 0.0));
               Total_Income := Total_Income + Income;
            end;
         end loop;

         for IG of Income_Groups loop
            IG.Relative_Income :=
              (IG.Income - Min_Income) / (Max_Income - Min_Income);
         end loop;
      end;

      Income_Group_Sorting.Sort (Income_Groups);

      Calculate_Distribution;

      declare
         Total_Income : Non_Negative_Real := 0.0;
      begin

         for IG of Income_Groups loop
            Total_Income := Total_Income
              + IG.Income * IG.Proportion * Total_Pop;
         end loop;

         Ada.Text_IO.Put ("GROUP");
         Ada.Text_IO.Set_Col (20);
         Ada.Text_IO.Put ("SIZE");
         Ada.Text_IO.Set_Col (40);
         Ada.Text_IO.Put ("%TOTAL");
         Ada.Text_IO.Set_Col (60);
         Ada.Text_IO.Put ("%INCOME");
         Ada.Text_IO.Set_Col (80);
         Ada.Text_IO.Put ("REL INCOME");
         Ada.Text_IO.Set_Col (100);
         Ada.Text_IO.Put ("#POPS");
         Ada.Text_IO.New_Line;

         for IG of Income_Groups loop
            Ada.Text_IO.Put (IG.Group.Identifier);
            Ada.Text_IO.Set_Col (20);
            Ada.Text_IO.Put
              (Natural'Image (Natural (Total_Pop * IG.Proportion)));
            Ada.Text_IO.Set_Col (40);
            Ada.Text_IO.Put
              (Natural'Image (Natural (IG.Proportion * 100.0)) & "%");
            Ada.Text_IO.Set_Col (60);
            Ada.Text_IO.Put
              (Natural'Image
                 (Natural
                      (IG.Proportion * Total_Pop * IG.Income / Total_Income
                       * 100.0)) & "%");
            Ada.Text_IO.Set_Col (80);
            Ada.Text_IO.Put
              (Natural'Image
                 (Natural (IG.Relative_Income * 100.0)) & "%");
            Ada.Text_IO.Set_Col (100);
            Ada.Text_IO.Put
              (Natural'Image
                 (Natural
                      (Real'Ceiling
                           (IG.Proportion * Total_Pop / Pop_Size))));
            Ada.Text_IO.New_Line;
         end loop;

      end;

      Ada.Text_IO.Put_Line
        ("gini:"
         & Natural'Image (Natural (Current_Gini * 100.0))
         & "%");

      for IG of Income_Groups loop
         declare
            Group_Size : constant Natural :=
                           Natural
                             (Real'Ceiling
                                (IG.Proportion * Total_Pop));
            Pop_Count  : constant Positive :=
                           Positive'Max
                             (20,
                              Positive
                                (Real'Ceiling
                                   (IG.Proportion * Total_Pop / Pop_Size)));
            Size       : constant Natural := Group_Size / Pop_Count;
         begin
            for I in 1 .. Pop_Count loop
               declare
                  use Concorde.People.Groups;
                  Pop_Groups   : constant Array_Of_Pop_Groups :=
                                   All_Groups;
                  Political    : constant Array_Of_Pop_Groups :=
                                   Political_Groups;
                  Group_Count  : Natural := 0;
                  Groups       : Array_Of_Pop_Groups (Pop_Groups'Range);
                  Left         : constant Unit_Real :=
                                   Concorde.Random.Unit_Random;
                  Considered_Groups : WL.String_Sets.Set;

                  procedure Add_Group
                    (G : Concorde.People.Groups.Pop_Group);

                  ---------------
                  -- Add_Group --
                  ---------------

                  procedure Add_Group
                    (G : Concorde.People.Groups.Pop_Group)
                  is
                  begin
                     Group_Count := Group_Count + 1;
                     Groups (Group_Count) := G;
                     Considered_Groups.Insert (G.Identifier);
                  end Add_Group;

               begin
                  Add_Group (Everybody);
                  Add_Group (IG.Group);
                  for G of Political loop
                     if G.Has_Wealth_Proportion then
                        declare
                           Bias : constant Real :=
                                    G.Wealth_Proportion.Evaluate
                                      (Community.Network,
                                       "relative-wealth", IG.Relative_Income);
                           R    : constant Real := Left * Bias;
                           P    : constant Unit_Real :=
                                    Default_Group_Proportion (G);
                        begin
                           if (G.Is_Political_Left
                               and then R < P)
                             or else
                               (G.Is_Political_Right
                                and then R > 1.0 - P)
                           then
                              Add_Group (G);
                              exit;
                           end if;
                        end;
                     end if;
                  end loop;

                  for G of Political loop
                     if G.Has_Wealth_Proportion
                       and then not Considered_Groups.Contains (G.Identifier)
                     then
                        Considered_Groups.Insert (G.Identifier);
                     end if;
                  end loop;

                  declare
                     Chances : array (All_Groups'Range) of Non_Negative_Real;
                     Gs      : array (All_Groups'Range) of Pop_Group;
                     Count   : Natural;
                     Total   : Non_Negative_Real;
                  begin
                     loop

                        Total := 0.0;
                        Count := 0;

--                          Ada.Text_IO.Put_Line
--                            ("checking groups ...");
                        for Group of All_Groups loop
                           if not Considered_Groups.Contains
                             (Group.Identifier)
                           then
                              declare
                                 Chance : Unit_Real :=
                                            Default_Group_Proportion (Group);
                              begin
                                 for I in 1 .. Group_Count loop
                                    Chance := Chance *
                                      (1.0 + Groups (I).Influence (Group));
                                 end loop;
                                 if Chance > 0.0 then
                                    Count := Count + 1;
                                    Chances (Count) := Chance;
                                    Gs (Count) := Group;
                                    Total := Total + Chance;
--                                      Ada.Text_IO.Put_Line
--                                        ("   " & Group.Identifier
--                                         & " "
--                                         & Concorde.Real_Images
--                                         .Approximate_Image (Chance));
                                 end if;
                              end;
                           end if;
                        end loop;

                        exit when Count < 2;

                        declare
                           R : Non_Negative_Real :=
                                 Concorde.Random.Unit_Random
                                   * (Real'Max (Total * 1.2, 1.0));
                           Assigned : Boolean := False;
                        begin
--                             Ada.Text_IO.Put_Line
--                               ("roll: "
--                                & Concorde.Real_Images.
--                                  Approximate_Image (R));
                           for I in 1 .. Count loop
                              if R <= Chances (I) then
--                                   Ada.Text_IO.Put_Line
--                                     ("adding group: "
--                                      & Gs (I).Identifier);
                                 Add_Group (Gs (I));
                                 Assigned := True;
                                 exit;
                              end if;
                              R := R - Chances (I);
                           end loop;
                           exit when not Assigned;
                        end;
                     end loop;
                  end;

--                    Ada.Text_IO.Put_Line ("done");
--                    for Group of All_Groups loop
--                       if not Considered_Groups.Contains
--                         (Group.Identifier)
--                       then
--                          Considered_Groups.Insert (Group.Identifier);
--                          declare
--                             Chance : Unit_Real :=
--                                        Default_Group_Proportion (Group);
--                          begin
--                             for I in 1 .. Group_Count loop
--                                Chance := Chance *
--                                  (1.0 + Groups (I).Influence (Group));
--                             end loop;
--
--                             if Concorde.Random.Unit_Random
--                               <= Chance
--                             then
--                                Add_Group (Group);
--                             end if;
--                          end;
--                       end if;
--                    end loop;

                  Community.Add_Pop
                    (Concorde.People.Pops.Create.New_Pop
                       (Market     => Community.Market,
                        Government => Community.Government,
                        Network    => Community,
                        Groups     => Groups (1 .. Group_Count),
                        Size       => Concorde.People.Pops.Pop_Size (Size),
                        Apathy     => Apathy));
               end;
            end loop;
         end;

      end loop;

      declare
         use WL.Quantities;
         All_Groups : constant Concorde.People.Groups.Array_Of_Pop_Groups :=
                        Concorde.People.Groups.All_Groups;
         Size       : array (All_Groups'Range) of Quantity_Type :=
                        (others => Zero);
      begin
         for Pop of Community.Pops loop
            for I in All_Groups'Range loop
               if Pop.Is_Member_Of (All_Groups (I)) then
                  Size (I) := Size (I) + Pop.Size_Quantity;
               end if;
            end loop;
         end loop;

         for I in All_Groups'Range loop
            Ada.Text_IO.Put (All_Groups (I).Identifier);
            Ada.Text_IO.Set_Col (30);
            Ada.Text_IO.Put (Show (Size (I)));
            Ada.Text_IO.New_Line;
         end loop;
      end;

   end Create_Initial_Pops;

   --------------------------
   -- Create_Network_State --
   --------------------------

   procedure Create_Network_State
     (Community     : in out Root_Community_Type'Class;
      Initial_Value : not null access
        function (Parameter_Name : String) return Real)
   is
      procedure Add_Metric_State
        (Metric : Concorde.Metrics.Metric_Type);

      procedure Add_Policy_State
        (Policy : Concorde.Policies.Policy_Type);

      ----------------------
      -- Add_Metric_State --
      ----------------------

      procedure Add_Metric_State
        (Metric : Concorde.Metrics.Metric_Type)
      is
      begin
         Community.Network.Add_Node
           (Metric.Metric_Node.Create_State
              (Initial_Value (Metric.Metric_Node.Identifier)));
      end Add_Metric_State;

      ----------------------
      -- Add_Policy_State --
      ----------------------

      procedure Add_Policy_State
        (Policy : Concorde.Policies.Policy_Type)
      is
      begin
         Community.Network.Add_Node
           (Policy.Policy_Node.Create_State
              (Initial_Value (Policy.Policy_Node.Identifier)));
      end Add_Policy_State;

   begin

      for Group of Concorde.People.Groups.All_Groups loop
         declare
            Happiness : constant Concorde.Network.Nodes.Node_Type :=
                          Group.Happiness_Node;
            Frequency : constant Concorde.Network.Nodes.Node_Type :=
                          Group.Frequency_Node;
            Income    : constant Concorde.Network.Nodes.Node_Type :=
                       Group.Income_Node;
         begin
            Community.Network.Add_Node
              (Happiness.Create_State (Initial_Value (Happiness.Identifier)));
            Community.Network.Add_Node
              (Frequency.Create_State (Initial_Value (Frequency.Identifier)));
            Community.Network.Add_Node
              (Income.Create_State (Initial_Value (Income.Identifier)));
         end;
      end loop;

      Concorde.Metrics.Scan_Metrics (Add_Metric_State'Access);
      Concorde.Policies.Scan_Policies (Add_Policy_State'Access);

   end Create_Network_State;

   -------------------
   -- New_Community --
   -------------------

   function New_Community
     (World         : not null access constant
        Concorde.Worlds.Root_World_Type'Class;
      Faction       : not null access constant
        Concorde.Factions.Root_Faction_Type'Class;
      Population    : WL.Quantities.Quantity_Type;
      Gini          : Unit_Real;
      Template      : Tropos.Configuration)
      return Community_Type
   is

      Init : constant Tropos.Configuration := Template.Child ("init");

      function Initial_Value
        (Name : String)
         return Real
      is (Real (Float'(Init.Get (Name, 0.0))));

      procedure Create (Community : in out Root_Community_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Community : in out Root_Community_Type'Class) is
         function Get_Land_Use (Item : Land_Use) return Unit_Real
         is (Real (Float'(Template.Get (Item'Image & "-land-use", 0.0))));

         Total_Land_Use : Non_Negative_Real := 0.0;

      begin
         Community.Set_Name (World.Name);
         Community.World := Concorde.Worlds.World_Type (World);
         Community.Owner := Concorde.Factions.Faction_Type (Faction);
         Community.Occupation :=
           Real (Float'(Template.Get ("occupation", 0.1)));

         Create_Network_State (Community, Initial_Value'Access);
         for Item in Land_Use loop
            declare
               Relative : constant Unit_Real := Get_Land_Use (Item);
            begin
               Community.Land_Use (Item).Relative := Relative;
               Total_Land_Use := Total_Land_Use + Relative;
            end;
         end loop;

         if Total_Land_Use < 1.0 then
            Community.Land_Use (Undeveloped).Relative :=
                 Community.Land_Use (Undeveloped).Relative
                 + 1.0 - Total_Land_Use;
         elsif Total_Land_Use > 1.0 then
            for Item of Community.Land_Use loop
               Item.Relative := Item.Relative / Total_Land_Use;
            end loop;
         end if;

         for Item of Community.Land_Use loop
            Item.Absolute :=
              Community.World.Surface_Area
              * (1.0 - Community.World.Hydrosphere)
              * Community.Occupation
              * Item.Relative;

         end loop;

      end Create;

      Community : constant Community_Type :=
                    Db.Create (Create'Access);
   begin

      Community.Update.Government :=
        Concorde.Government.Create.Create_Government
          (Governed          => Community,
           Location => Concorde.Locations.In_Community (Community),
           Cash              =>
             WL.Money.To_Money
               (WL.Quantities.To_Float (Population)),
           Owner             => Faction);

      declare
         procedure Update (Community : in out Root_Community_Type'Class);

         ------------
         -- Update --
         ------------

         procedure Update (Community : in out Root_Community_Type'Class) is
         begin
            Create_Initial_Pops
              (Community     => Community,
               Pop_Size      => 1.0E6,
               Apathy        => 0.2,
               Gini          => Gini,
               Total_Pop     =>
                 Non_Negative_Real (WL.Quantities.To_Float (Population)),
               Initial_Value => Initial_Value'Access);
         end Update;

      begin
         Db.Update (Community.Reference, Update'Access);
      end;

      Community.Update.Market :=
        Concorde.Markets.Create_Market
          (Identifier     => World.Identifier & "-market",
           Owner          => Community,
           Manager        => Community.Government,
           Enable_Logging => False);

      Concorde.Managers.Communities.Create_Manager (Community).Activate;

      declare
         use type Concorde.Calendar.Time;
      begin
         Concorde.Objects.Queues.Next_Event
           (Community,
            Concorde.Calendar.Clock
            + Duration (Concorde.Random.Unit_Random * 86_400.0));
      end;

      return Community;
   end New_Community;

end Concorde.People.Communities.Create;
