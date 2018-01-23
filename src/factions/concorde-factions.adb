with Ada.Containers.Vectors;

with Concorde.Factions.Events;
with Concorde.Factions.Logging;
with Concorde.Galaxy;
with Concorde.Systems.Graphs;

with Concorde.People.Individuals;

package body Concorde.Factions is

   function Image
     (Path : Concorde.Systems.Graphs.Array_Of_Vertices)
      return String;

   function All_Factions return Array_Of_Factions;

   -----------------
   -- All_Factions --
   -----------------

   function All_Factions return Array_Of_Factions is

      package Vectors is new Ada.Containers.Vectors (Positive, Faction_Type);
      V : Vectors.Vector;

      procedure Add (E : Faction_Type);

      ---------
      -- Add --
      ---------

      procedure Add (E : Faction_Type) is
      begin
         V.Append (E);
      end Add;

   begin
      Db.Scan (Add'Access);

      declare
         Result : Array_Of_Factions (1 .. V.Last_Index);
      begin
         for I in Result'Range loop
            Result (I) := V (I);
         end loop;
         return Result;
      end;

   end All_Factions;

   -------------
   -- Capital --
   -------------

   function Capital
     (Faction : Root_Faction_Type'Class)
      return access constant Concorde.Worlds.Root_World_Type'Class
   is
   begin
      return Faction.Capital_World;
   end Capital;

   -------------------------
   -- Change_Relationship --
   -------------------------

   procedure Change_Relationship
     (Faction  : in out Root_Faction_Type'Class;
      To      : Root_Faction_Type'Class;
      Change  : Faction_Relationship_Range)
   is
      Current_Value : constant Integer :=
                        Integer (Faction.Relationship (To));
      New_Value     : constant Integer :=
                        Current_Value + Integer (Change);
      New_Relationship : constant Faction_Relationship_Range :=
                           (if New_Value < Minimum_Relationship
                            then Minimum_Relationship
                            elsif New_Value > Maximum_Relationship
                            then Maximum_Relationship
                            else Faction_Relationship_Range (New_Value));
   begin
      Faction.Set_Relationship (To, New_Relationship);
   end Change_Relationship;

   ---------------------
   -- Change_Required --
   ---------------------

   procedure Change_Required
     (Faction   : in out Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Change   : Integer)
   is
      R : Integer renames Faction.System_Data (System.Index).Required;
   begin
      R := R + Change;
   end Change_Required;

   ------------------
   -- Change_Ships --
   ------------------

   procedure Change_Ships
     (Faction : in out Root_Faction_Type'Class;
      Change : Integer)
   is
   begin
      Faction.Current_Ships := Faction.Current_Ships + Change;
      if Change < 0 then
         Faction.Lost_Ships := Faction.Lost_Ships - Change;
      else
         Faction.Captured_Ships := Faction.Captured_Ships + Change;
      end if;
   end Change_Ships;

   -----------------
   -- Check_Cache --
   -----------------

   procedure Check_Cache
     (Faction   : Root_Faction_Type'Class;
      From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
   is
      Data : Faction_Star_System_Record renames
               Faction.System_Data (From.Index);
   begin
      if Data.Next_Node = null
        or else Data.Next_Node (To.Index).Next_Node = -1
      then
         declare
            function OK
              (System : Concorde.Systems.Star_System_Type)
               return Boolean
            is (True);
--              is (not System.Owned
--                  or else Faction.Owned_System (System)
--                  or else not Relations.At_War (Faction, System.Owner.all));

            Path : constant Concorde.Systems.Graphs.Array_Of_Vertices :=
                     Concorde.Galaxy.Shortest_Path
                       (From, To, OK'Access);

         begin

            if False and then Path'Length > 2 then
               Logging.Log (Faction,
                            "from" & From.Name & " to " & To.Name & ":"
                            & Image (Path));
            end if;

            for I in Path'First .. Path'Last loop
               declare
                  D : Faction_Star_System_Record renames
                        Faction.System_Data (Path (I));
               begin
                  if D.Next_Node = null then
                     D.Next_Node :=
                       new Destination_Next_Index (1 .. Galaxy.System_Count);
                     D.Next_Node.all := (others => (0, -1));
                  end if;

                  for J in I + 1 .. Path'Last loop
                     D.Next_Node (Path (J)) :=
                       (Path'Length - I, Path (I + 1));
                  end loop;
               end;
            end loop;

            if Path'Length <= 1 then
               Faction.System_Data (From.Index).Next_Node (To.Index) :=
                 (Natural'Last / 2, 0);
            end if;

         end;
      end if;
   end Check_Cache;

   ----------------------
   -- Check_Invariants --
   ----------------------

   procedure Check_Invariants is

      procedure Check (Faction : Root_Faction_Type'Class);

      -----------
      -- Check --
      -----------

      procedure Check (Faction : Root_Faction_Type'Class) is
         use type Memor.Database_Reference;
         use type Concorde.Systems.Star_System_Type;
      begin
         if Faction.Current_Systems > 0 then
            pragma Assert (Faction.Capital_World /= null,
                           Faction.Name & ": no capital");
            pragma Assert (Faction.Owned_World
                           (Faction.Capital),
                           Faction.Name & " does not own capital system "
                           & Faction.Capital.Name);
            pragma Assert (Faction.Capital.Is_Capital,
                           Faction.Name & ": capital system is not a capital");
         end if;
      end Check;
   begin
      Db.Scan (Check'Access);
   end Check_Invariants;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Faction   : in out Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
   is
      Data : System_Data_Array renames Faction.System_Data.all;
   begin
      Data (System.Index).Flags (Flag) := False;
   end Clear;

   -------------------
   -- Clear_Battles --
   -------------------

   procedure Clear_Battles is

      procedure Clear (Faction : in out Root_Faction_Type'Class);

      -----------
      -- Clear --
      -----------

      procedure Clear (Faction : in out Root_Faction_Type'Class) is null;
--        begin
--           Faction.Battles.Clear;
--        end Clear;

   begin
      Db.Iterate (Clear'Access);
   end Clear_Battles;

   ----------------------
   -- Clear_Path_Cache --
   ----------------------

   procedure Clear_Path_Cache (Faction : in out Root_Faction_Type'Class) is
   begin
      for I in Faction.System_Data'Range loop
         if Faction.System_Data (I).Next_Node /= null then
            Faction.System_Data (I).Next_Node.all :=
              (others => (0, -1));
         end if;
      end loop;
   end Clear_Path_Cache;

   ------------------------
   -- Clear_System_Flags --
   ------------------------

   procedure Clear_System_Flags
     (Faction   : in out Root_Faction_Type'Class;
      System   : Concorde.Systems.Root_Star_System_Type'Class)
   is
      System_Data : Faction_Star_System_Record renames
                      Faction.System_Data (System.Index);
   begin
      System_Data.Flags (Internal) := False;
      System_Data.Flags (Frontier) := False;
      System_Data.Flags (Border) := False;
      System_Data.Flags (Neighbour) := False;
   end Clear_System_Flags;

   ------------
   -- Colour --
   ------------

   function Colour
     (Faction : Root_Faction_Type'Class)
      return Lui.Colours.Colour_Type
   is
   begin
      return Faction.Colour;
   end Colour;

   ---------------------------
   -- Current_Effectiveness --
   ---------------------------

   function Current_Effectiveness
     (Faction   : Root_Faction_Type'Class;
      Portfolio : Concorde.Offices.Responsibility_Type)
      return Unit_Real
   is
      Office : constant Concorde.Offices.Office_Type :=
                 Concorde.Offices.Get (Portfolio);
   begin
      if Faction.Has_Minister (Office) then
         return Office.Effectiveness
           (Portfolio => Faction.Portfolio_Size (Portfolio),
            Holder    => Faction.Minister (Office).all);
      else
         return 1.0 / Real (Faction.Portfolio_Size (Portfolio));
      end if;
   end Current_Effectiveness;

   --------------------
   -- Current_Ships --
   --------------------

   function Current_Ships
     (Faction : Root_Faction_Type'Class)
      return Natural
   is
   begin
      return Faction.Current_Ships;
   end Current_Ships;

   ---------------------
   -- Current_Systems --
   ---------------------

   function Current_Systems
     (Faction : Root_Faction_Type'Class)
      return Natural
   is
   begin
      return Faction.Current_Systems;
   end Current_Systems;

   -------------------------
   -- Default_Ship_Design --
   -------------------------

   function Default_Ship_Design
     (Faction : Root_Faction_Type'Class)
      return String
   is
   begin
      return Faction.Default_Ship.all;
   end Default_Ship_Design;

   ---------
   -- Get --
   ---------

   function Get
     (Rank_Type : Ranking;
      Index     : Positive)
      return Faction_Type
   is
      Ranked_Factions : constant Array_Of_Factions :=
                         Rank (Rank_Type);
   begin
      return Ranked_Factions (Index);
   end Get;

   -----------------
   -- Get_By_Name --
   -----------------

   function Get_By_Name (Name : String) return Faction_Type is
   begin
      for Faction of All_Factions loop
         if Faction.Name = Name then
            return Faction;
         end if;
      end loop;
      return null;
   end Get_By_Name;

   -----------
   -- Image --
   -----------

   function Image
     (Path : Concorde.Systems.Graphs.Array_Of_Vertices)
      return String
   is
   begin
      if Path'Length = 0 then
         return "";
      elsif Path'Length = 1 then
         return Integer'Image (Path (Path'First));
      else
         return Integer'Image (Path (Path'First))
           & " ->" & Image (Path (Path'First + 1 .. Path'Last));
      end if;
   end Image;

   --------------
   -- Is_Clear --
   --------------

   function Is_Clear
     (Faction   : Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
      return Boolean
   is
   begin
      return not Faction.Is_Set (System, Flag);
   end Is_Clear;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Faction   : Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
      return Boolean
   is
   begin
      return Faction.System_Data (System.Index).Flags (Flag);
   end Is_Set;

   ----------------
   -- New_Ships --
   ----------------

   procedure New_Ship
     (Faction : in out Root_Faction_Type'Class)
   is
   begin
      Faction.Current_Ships := Faction.Current_Ships + 1;
      Faction.Built_Ships := Faction.Built_Ships + 1;
   end New_Ship;

   --------------------------
   -- Next_Path_Node_Index --
   --------------------------

   function Next_Path_Node_Index
     (Faction : Root_Faction_Type'Class;
      From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Natural
   is
      Data : Faction_Star_System_Record renames
               Faction.System_Data (From.Index);
   begin
      Faction.Check_Cache (From, To);

      return Data.Next_Node (To.Index).Next_Node;
   end Next_Path_Node_Index;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Faction : Root_Faction_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Faction);
   begin
      return Db.Get_Database;
   end Object_Database;

   ------------------
   -- Owned_System --
   ------------------

--     function Owned_System
--       (Faction : Root_Faction_Type'Class;
--        System : not null access constant
--          Concorde.Systems.Root_Star_System_Type'Class)
--        return Boolean
--     is
--        use type Memor.Database_Reference;
--     begin
--        return System.Owned
--  and then System.Owner.Reference = Faction.Reference;
--     end Owned_System;

   -----------------
   -- Owned_World --
   -----------------

   function Owned_World
     (Faction : Root_Faction_Type'Class;
      World  : not null access constant
        Concorde.Worlds.Root_World_Type'Class)
      return Boolean
   is
   begin
      return World.Owned_By (Faction);
   end Owned_World;

   -----------------
   -- Path_Length --
   -----------------

   function Path_Length
     (Faction : Root_Faction_Type'Class;
      From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Natural
   is
      Data : Faction_Star_System_Record renames
               Faction.System_Data (From.Index);
   begin
      Faction.Check_Cache (From, To);

      return Data.Next_Node (To.Index).Path_Length;
   end Path_Length;

   --------------------
   -- Portfolio_Size --
   --------------------

   function Portfolio_Size
     (Faction   : Root_Faction_Type'Class;
      Portfolio : Concorde.Offices.Responsibility_Type)
      return Concorde.Offices.Portfolio_Size_Range
   is
      use Concorde.Offices;
   begin
      case Portfolio is
         when Leader =>
            declare
               Result : Portfolio_Size_Range := 1;
            begin
               for Item in Responsibility_Type loop
                  if Item /= Leader then
                     if not Faction.Has_Minister (Get (Item)) then
                        Result := Result + Faction.Portfolio_Size (Item);
                     else
                        Result := Result + 1;
                     end if;
                  end if;
               end loop;
               return Result;
            end;
         when Treasury =>
            return Portfolio_Size_Range (Faction.Current_Systems);
         when Army =>
            declare
               Result : Portfolio_Size_Range := 1;
               Units  : Positive := 10;
            begin
               while Units < Faction.Current_Units loop
                  Units := Units * 5;
                  Result := Result + 1;
               end loop;
               return Result;
            end;
         when Navy =>
            declare
               Result : Portfolio_Size_Range := 1;
               Ships  : Positive := 5;
            begin
               while Ships < Faction.Current_Ships loop
                  Ships := Ships * 2;
                  Result := Result + 1;
               end loop;
               return Result;
            end;
         when Diplomacy =>
            if Faction.Current_Relations <= 3 then
               return 1;
            else
               return Portfolio_Size_Range (Faction.Current_Relations / 2);
            end if;
      end case;
   end Portfolio_Size;

   ----------
   -- Rank --
   ----------

   function Rank
     (Rank_Type : Ranking)
      return Array_Of_Factions
   is
      Es : constant Array_Of_Factions := All_Factions;
      Rank  : Array_Of_Factions (Es'Range);
      Count : Natural := 0;
      Target : Natural;

      function Higher (X, Y : Faction_Type) return Boolean;

      ------------
      -- Higher --
      ------------

      function Higher (X, Y : Faction_Type) return Boolean is
      begin
         case Rank_Type is
            when Normal =>
               return False;
            when By_Star_Systems =>
               return X.Current_Ships > Y.Current_Ships;
            when By_Ships =>
               return X.Current_Ships > Y.Current_Ships;
         end case;
      end Higher;

   begin
      for E of Es loop
         Target := Count;
         while Target > 0
           and then Higher (E, Rank (Target))
         loop
            Rank (Target + 1) := Rank (Target);
            Target := Target - 1;
         end loop;
         Count := Count + 1;
         Rank (Target + 1) := E;
      end loop;
      return Rank;
   end Rank;

   ------------------
   -- Relationship --
   ------------------

   function Relationship
     (Faction : Root_Faction_Type'Class;
      To     : Root_Faction_Type'Class)
      return Faction_Relationship_Range
   is
   begin
      if Faction.Faction_Data = null then
         return 0;
      else
         return Faction.Faction_Data.Vector.Element (To).Relationship;
      end if;
   end Relationship;

   -----------------
   -- Remove_Ship --
   -----------------

   procedure Remove_Ship
     (Faction : in out Root_Faction_Type'Class)
   is
   begin
      Faction.Current_Ships := Faction.Current_Ships - 1;
      Faction.Destroyed_Ships := Faction.Destroyed_Ships + 1;
   end Remove_Ship;

   ------------------
   -- Require_Cash --
   ------------------

   overriding procedure Require_Cash
     (Faction : in out Root_Faction_Type;
      Amount  : WL.Money.Money_Type)
   is
      use WL.Money;
   begin
      if Amount > Faction.Cash and then Faction.Central_Bank then
         declare
            Fiat_Amount : constant Money_Type :=
                            Amount - Faction.Cash;
         begin
            Faction.Log
              ("budget",
               "creating " & Show (Fiat_Amount) & " by fiat");
            Faction.Add_Cash (Fiat_Amount);
         end;
      end if;
   end Require_Cash;

   --------------
   -- Required --
   --------------

   function Required
     (Faction : Root_Faction_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Integer
   is
   begin
      return Faction.System_Data (System.Index).Required;
   end Required;

   ------------------
   -- Scan_Factions --
   ------------------

   procedure Scan_Factions
     (Process : not null access
        procedure (Faction : Faction_Type))
   is
   begin
      Db.Scan (Process);
   end Scan_Factions;

   ---------
   -- Set --
   ---------

   procedure Set
     (Faction   : in out Root_Faction_Type'Class;
      System   : Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
   is
      Data : System_Data_Array renames Faction.System_Data.all;
   begin
      Data (System.Index).Flags (Flag) := True;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Faction   : in out Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
   is
   begin
      Faction.Set (System.all, Flag);
   end Set;

   ------------------
   -- Set_Minister --
   ------------------

   procedure Set_Minister
     (Faction  : in out Root_Faction_Type'Class;
      Office   : Concorde.Offices.Office_Type;
      Minister : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
   is
      Old_Minister : constant Individual_Access :=
                       Faction.Cabinet.Element (Office);
      Event        : Concorde.Factions.Events.Office_Changed_Event;

   begin
      Faction.Cabinet.Replace_Element (Office, Individual_Access (Minister));
      Event.Set_Time_Stamp (Concorde.Calendar.Clock);
      Event.Office := Office;
      Event.Old_Minister :=
        Concorde.People.Individuals.Individual_Type (Old_Minister);
      Event.New_Minister :=
        Concorde.People.Individuals.Individual_Type
          (Minister);

      Faction.Signal
        (Sig   => Concorde.Factions.Events.Signal_Office_Changed,
         Event => Event);

   end Set_Minister;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Faction : in out Root_Faction_Type;
      Name   : String)
   is
   begin
      Faction.Faction_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

   ----------------------
   -- Set_Relationship --
   ----------------------

   procedure Set_Relationship
     (Faction : in out Root_Faction_Type'Class;
      To     : Root_Faction_Type'Class;
      Value  : Faction_Relationship_Range)
   is
      Data : Faction_Data_Record :=
               Faction.Faction_Data.Vector.Element (To);
   begin
      declare
         Relationship : Faction_Relationship_Range renames
                          Data.Relationship;
      begin
         if Relationship >= 0
           and then Value < 0
         then
            Concorde.Factions.Logging.Log
              (Faction, "now at war with " & To.Name);
         end if;

         Relationship := Value;
      end;

      Faction.Faction_Data.Vector.Replace_Element (To, Data);

   end Set_Relationship;

   ------------------
   -- Set_Required --
   ------------------

   procedure Set_Required
     (Faction   : in out Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Required : Integer)
   is
      R : Integer renames Faction.System_Data (System.Index).Required;
   begin
      R := Required;
   end Set_Required;

   ---------------------
   -- System_Acquired --
   ---------------------

--     procedure System_Acquired
--       (Faction : in out Root_Faction_Type'Class;
--        System : in out Concorde.Systems.Root_Star_System_Type'Class)
--     is
--        use type Concorde.Systems.Star_System_Type;
--     begin
--        Faction.Current_Systems := Faction.Current_Systems + 1;
--        Concorde.Factions.Logging.Log
--          (Faction,
--           "new system count:"
--           & Faction.Current_Systems'Img);
--        Faction.Clear_Path_Cache;
--        if Faction.Capital = null then
--           Faction.Capital_World := Concorde.Systems.Db.Reference (System);
--           System.Set_Capital (True);
--           Concorde.Factions.Logging.Log
--             (Faction,
--              System.Name & " is our new capital");
--        end if;
--
--        Faction.Update_System_Owner (System);
--
--     end System_Acquired;

   -----------------
   -- System_Lost --
   -----------------

--     procedure System_Lost
--       (Faction : in out Root_Faction_Type'Class;
--        System : in out Concorde.Systems.Root_Star_System_Type'Class)
--     is
--     begin
--        Faction.Current_Systems := Faction.Current_Systems - 1;
--        Faction.Clear_Path_Cache;
--
--        Concorde.Factions.Logging.Log
--          (Faction,
--           "new system count:"
--           & Faction.Current_Systems'Img);
--        if System.Capital then
--           Factions.Logging.Log
--             (Faction,
--              "lost its capital " & System.Name);
--           System.Set_Capital (False);
--           declare
--              use Concorde.Systems;
--
--              function Score
--                (Test : Concorde.Systems.Star_System_Type)
--                 return Natural;
--
--              procedure Set_Capital
--                (System : in out Root_Star_System_Type'Class);
--
--              -----------
--              -- Score --
--              -----------
--
--              function Score
--                (Test : Concorde.Systems.Star_System_Type)
--                 return Natural
--              is
--                 use type Memor.Database_Reference;
--                 Result : Natural := 0;
--              begin
--                 if Faction.Owned_System (Test)
--                   and then Test.Reference /= System.Reference
--                 then
--                    Result := 100 + Test.Ships;
--
--                    for N of Concorde.Galaxy.Neighbours (Test.Index) loop
--                       if Faction.Owned_System (N) then
--                          Result := Result + 5;
--                       elsif N.Owned then
--                          if Concorde.Factions.Relations.At_War
--                            (Faction, N.Owner.all)
--                          then
--                             Result := Result - 10;
--                          else
--                             Result := Result - 2;
--                          end if;
--                       end if;
--                    end loop;
--                 end if;
--
--                 return Result;
--              end Score;
--
--              -----------------
--              -- Set_Capital --
--              -----------------
--
--              procedure Set_Capital
--                (System : in out Root_Star_System_Type'Class)
--              is
--              begin
--                 System.Set_Capital (True);
--              end Set_Capital;
--
--              New_Capital : constant Concorde.Systems.Star_System_Type :=
--                              Galaxy.Maximum (Score'Access);
--
--           begin
--              if New_Capital /= null then
--                 Concorde.Systems.Db.Update (New_Capital.Reference,
--                                             Set_Capital'Access);
--                 Faction.Capital := New_Capital;
--                 Factions.Logging.Log
--                   (Faction, "new capital: " & New_Capital.Name);
--              else
--                 Factions.Logging.Log
--                   (Faction, "eliminated");
--                 Faction.Capital := null;
--              end if;
--           end;
--        end if;
--     end System_Lost;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Faction_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

   -------------------------
   -- Update_System_Owner --
   -------------------------

   procedure Update_System_Owner
     (Owner  : in out Root_Faction_Type'Class;
      System : Concorde.Systems.Root_Star_System_Type'Class)
   is
      use Concorde.Galaxy;
      Border : Boolean := False;
   begin
      for N of Neighbours (System.Index) loop
         if System.Owner /= N.Owner then
            Border := True;
            Owner.Set (N, Concorde.Factions.Neighbour);
            if N.Owned then
               if N.Owner /= System.Owner then
                  Owner.Set (System, Concorde.Factions.Border);
                  Owner.Set (N, Concorde.Factions.Neighbour);
               end if;
            else
               Owner.Set (System, Concorde.Factions.Frontier);
               Owner.Set (N, Concorde.Factions.Neighbour);
            end if;
         end if;
      end loop;
      if not Border then
         Owner.Set (System, Concorde.Factions.Internal);
      end if;
   end Update_System_Owner;

end Concorde.Factions;
