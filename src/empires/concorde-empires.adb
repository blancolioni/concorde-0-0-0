with Ada.Containers.Vectors;

with Concorde.Empires.Logging;
with Concorde.Empires.Relations;
with Concorde.Galaxy;
with Concorde.Systems.Graphs;

with Concorde.Empires.Db;
with Concorde.Systems.Db;

package body Concorde.Empires is

   function Image
     (Path : Concorde.Systems.Graphs.Array_Of_Vertices)
      return String;

   function All_Empires return Array_Of_Empires;

   -----------------
   -- All_Empires --
   -----------------

   function All_Empires return Array_Of_Empires is

      package Vectors is new Ada.Containers.Vectors (Positive, Empire_Type);
      V : Vectors.Vector;

      procedure Add (E : Empire_Type);

      ---------
      -- Add --
      ---------

      procedure Add (E : Empire_Type) is
      begin
         V.Append (E);
      end Add;

   begin
      Db.Scan (Add'Access);

      declare
         Result : Array_Of_Empires (1 .. V.Last_Index);
      begin
         for I in Result'Range loop
            Result (I) := V (I);
         end loop;
         return Result;
      end;

   end All_Empires;

   ------------------------------
   -- Available_Ship_Capacity --
   ------------------------------

   function Available_Ship_Capacity
     (Empire : Root_Empire_Type'Class)
      return Natural
   is
   begin
      return Integer'Max
        (Natural (Empire.Max_Ships) - Empire.Current_Ships, 0);
   end Available_Ship_Capacity;

   -------------
   -- Capital --
   -------------

   function Capital
     (Empire : Root_Empire_Type'Class)
      return Concorde.Systems.Star_System_Type
   is
   begin
      return Empire.Capital;
   end Capital;

   -------------------------
   -- Change_Relationship --
   -------------------------

   procedure Change_Relationship
     (Empire  : in out Root_Empire_Type'Class;
      To      : Root_Empire_Type'Class;
      Change  : Empire_Relationship_Range)
   is
      Current_Value : constant Integer :=
                        Integer (Empire.Relationship (To));
      New_Value     : constant Integer :=
                        Current_Value + Integer (Change);
      New_Relationship : constant Empire_Relationship_Range :=
                           (if New_Value < Minimum_Relationship
                            then Minimum_Relationship
                            elsif New_Value > Maximum_Relationship
                            then Maximum_Relationship
                            else Empire_Relationship_Range (New_Value));
   begin
      Empire.Set_Relationship (To, New_Relationship);
   end Change_Relationship;

   ---------------------
   -- Change_Required --
   ---------------------

   procedure Change_Required
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Change   : Integer)
   is
      R : Integer renames Empire.System_Data (System.Index).Required;
   begin
      R := R + Change;
   end Change_Required;

   ------------------
   -- Change_Ships --
   ------------------

   procedure Change_Ships
     (Empire : in out Root_Empire_Type'Class;
      Change : Integer)
   is
   begin
      Empire.Current_Ships := Empire.Current_Ships + Change;
      if Change < 0 then
         Empire.Lost_Ships := Empire.Lost_Ships - Change;
      else
         Empire.Captured_Ships := Empire.Captured_Ships + Change;
      end if;
   end Change_Ships;

   -----------------
   -- Check_Cache --
   -----------------

   procedure Check_Cache
     (Empire   : Root_Empire_Type'Class;
      From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
   is
      Data : Empire_Star_System_Record renames
               Empire.System_Data (From.Index);
   begin
      if Data.Next_Node = null
        or else Data.Next_Node (To.Index).Next_Node = -1
      then
         declare
            function OK
              (System : Concorde.Systems.Star_System_Type)
               return Boolean
            is (not System.Owned
                or else Empire.Owned_System (System)
                or else not Relations.At_War (Empire, System.Owner.all));

            Path : constant Concorde.Systems.Graphs.Array_Of_Vertices :=
                     Concorde.Galaxy.Shortest_Path
                       (From, To, OK'Access);

         begin

            if False and then Path'Length > 2 then
               Logging.Log (Empire,
                            "from" & From.Name & " to " & To.Name & ":"
                            & Image (Path));
            end if;

            for I in Path'First .. Path'Last loop
               declare
                  D : Empire_Star_System_Record renames
                        Empire.System_Data (Path (I));
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
               Empire.System_Data (From.Index).Next_Node (To.Index) :=
                 (Natural'Last / 2, 0);
            end if;

         end;
      end if;
   end Check_Cache;

   ----------------------
   -- Check_Invariants --
   ----------------------

   procedure Check_Invariants is

      procedure Check (Empire : Root_Empire_Type'Class);

      -----------
      -- Check --
      -----------

      procedure Check (Empire : Root_Empire_Type'Class) is
         use type Concorde.Systems.Star_System_Type;
      begin
         if Empire.Current_Systems > 0 then
            pragma Assert (Empire.Capital /= null,
                           Empire.Name & ": no capital");
            pragma Assert (Empire.Owned_System (Empire.Capital),
                           Empire.Name & " does not own capital system "
                           & Empire.Capital.Name);
            pragma Assert (Empire.Capital.Capital,
                           Empire.Name & ": capital system is not a capital");
         end if;
      end Check;
   begin
      Db.Scan (Check'Access);
   end Check_Invariants;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
   is
      Data : System_Data_Array renames Empire.System_Data.all;
   begin
      Data (System.Index).Flags (Flag) := False;
   end Clear;

   -------------------
   -- Clear_Battles --
   -------------------

   procedure Clear_Battles is

      procedure Clear (Empire : in out Root_Empire_Type'Class);

      -----------
      -- Clear --
      -----------

      procedure Clear (Empire : in out Root_Empire_Type'Class) is
      begin
         Empire.Battles.Clear;
      end Clear;

   begin
      Db.Iterate (Clear'Access);
   end Clear_Battles;

   ----------------------
   -- Clear_Path_Cache --
   ----------------------

   procedure Clear_Path_Cache (Empire : in out Root_Empire_Type'Class) is
   begin
      for I in Empire.System_Data'Range loop
         if Empire.System_Data (I).Next_Node /= null then
            Empire.System_Data (I).Next_Node.all :=
              (others => (0, -1));
         end if;
      end loop;
   end Clear_Path_Cache;

   ------------------------
   -- Clear_System_Flags --
   ------------------------

   procedure Clear_System_Flags
     (Empire   : in out Root_Empire_Type'Class;
      System   : Concorde.Systems.Root_Star_System_Type'Class)
   is
      System_Data : Empire_Star_System_Record renames
                      Empire.System_Data (System.Index);
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
     (Empire : Root_Empire_Type'Class)
      return Lui.Colours.Colour_Type
   is
   begin
      return Empire.Colour;
   end Colour;

   --------------------
   -- Current_Ships --
   --------------------

   function Current_Ships
     (Empire : Root_Empire_Type'Class)
      return Natural
   is
   begin
      return Empire.Current_Ships;
   end Current_Ships;

   ---------------------
   -- Current_Systems --
   ---------------------

   function Current_Systems
     (Empire : Root_Empire_Type'Class)
      return Natural
   is
   begin
      return Empire.Current_Systems;
   end Current_Systems;

   -------------------------
   -- Default_Ship_Design --
   -------------------------

   function Default_Ship_Design
     (Empire : Root_Empire_Type'Class)
      return String
   is
   begin
      return Empire.Default_Ship.all;
   end Default_Ship_Design;

   ---------
   -- Get --
   ---------

   function Get
     (Rank_Type : Ranking;
      Index     : Positive)
      return Empire_Type
   is
      Ranked_Empires : constant Array_Of_Empires :=
                         Rank (Rank_Type);
   begin
      return Ranked_Empires (Index);
   end Get;

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
     (Empire   : Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
      return Boolean
   is
   begin
      return not Empire.Is_Set (System, Flag);
   end Is_Clear;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Empire   : Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
      return Boolean
   is
   begin
      return Empire.System_Data (System.Index).Flags (Flag);
   end Is_Set;

   ------------------------------
   -- Maximum_Supported_Ships --
   ------------------------------

   function Maximum_Supported_Ships
     (Empire : Root_Empire_Type'Class)
      return Natural
   is
   begin
      return Natural (Empire.Max_Ships);
   end Maximum_Supported_Ships;

   ----------------
   -- New_Ships --
   ----------------

   procedure New_Ship
     (Empire : in out Root_Empire_Type'Class)
   is
   begin
      Empire.Current_Ships := Empire.Current_Ships + 1;
      Empire.Built_Ships := Empire.Built_Ships + 1;
   end New_Ship;

   --------------------------
   -- Next_Path_Node_Index --
   --------------------------

   function Next_Path_Node_Index
     (Empire : Root_Empire_Type'Class;
      From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Natural
   is
      Data : Empire_Star_System_Record renames
               Empire.System_Data (From.Index);
   begin
      Empire.Check_Cache (From, To);

      return Data.Next_Node (To.Index).Next_Node;
   end Next_Path_Node_Index;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Empire : Root_Empire_Type)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Empire);
   begin
      return Db.Get_Database;
   end Object_Database;

   ------------------
   -- Owned_System --
   ------------------

   function Owned_System
     (Empire : Root_Empire_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is
      use type Memor.Database_Reference;
   begin
      return System.Owned and then System.Owner.Reference = Empire.Reference;
   end Owned_System;

   -----------------
   -- Path_Length --
   -----------------

   function Path_Length
     (Empire : Root_Empire_Type'Class;
      From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Natural
   is
      Data : Empire_Star_System_Record renames
               Empire.System_Data (From.Index);
   begin
      Empire.Check_Cache (From, To);

      return Data.Next_Node (To.Index).Path_Length;
   end Path_Length;

   ------------
   -- Player --
   ------------

   function Player
     (Empire : Root_Empire_Type'Class)
      return access Concorde.Players.Root_Player_Type'Class
   is
   begin
      return Empire.Player;
   end Player;

   ----------
   -- Rank --
   ----------

   function Rank
     (Rank_Type : Ranking)
      return Array_Of_Empires
   is
      Es : constant Array_Of_Empires := All_Empires;
      Rank  : Array_Of_Empires (Es'Range);
      Count : Natural := 0;
      Target : Natural;

      function Higher (X, Y : Empire_Type) return Boolean;

      ------------
      -- Higher --
      ------------

      function Higher (X, Y : Empire_Type) return Boolean is
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
     (Empire : Root_Empire_Type'Class;
      To     : Root_Empire_Type'Class)
      return Empire_Relationship_Range
   is
   begin
      return Empire.Empire_Data.Element (To.Reference).Relationship;
   end Relationship;

   -----------------
   -- Remove_Ship --
   -----------------

   procedure Remove_Ship
     (Empire : in out Root_Empire_Type'Class)
   is
   begin
      Empire.Current_Ships := Empire.Current_Ships - 1;
      Empire.Destroyed_Ships := Empire.Destroyed_Ships + 1;
   end Remove_Ship;

   --------------
   -- Required --
   --------------

   function Required
     (Empire : Root_Empire_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Integer
   is
   begin
      return Empire.System_Data (System.Index).Required;
   end Required;

   ---------
   -- Set --
   ---------

   procedure Set
     (Empire   : in out Root_Empire_Type'Class;
      System   : Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
   is
      Data : System_Data_Array renames Empire.System_Data.all;
   begin
      Data (System.Index).Flags (Flag) := True;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
   is
   begin
      Empire.Set (System.all, Flag);
   end Set;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Empire : in out Root_Empire_Type;
      Name   : String)
   is
   begin
      Empire.Empire_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

   ----------------------
   -- Set_Relationship --
   ----------------------

   procedure Set_Relationship
     (Empire : in out Root_Empire_Type'Class;
      To     : Root_Empire_Type'Class;
      Value  : Empire_Relationship_Range)
   is
      Data : Empire_Data_Record := Empire.Empire_Data.Element (To.Reference);
   begin
      declare
         Relationship : Empire_Relationship_Range renames
                          Data.Relationship;
      begin
         if Relationship >= 0
           and then Value < 0
         then
            Concorde.Empires.Logging.Log
              (Empire, "now at war with " & To.Name);
         end if;

         Relationship := Value;
      end;

      Empire.Empire_Data.Replace_Element (To.Reference, Data);

   end Set_Relationship;

   ------------------
   -- Set_Required --
   ------------------

   procedure Set_Required
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Required : Integer)
   is
      R : Integer renames Empire.System_Data (System.Index).Required;
   begin
      R := Required;
   end Set_Required;

   ---------------------
   -- System_Acquired --
   ---------------------

   procedure System_Acquired
     (Empire : in out Root_Empire_Type'Class;
      System : in out Concorde.Systems.Root_Star_System_Type'Class)
   is
      use type Concorde.Systems.Star_System_Type;
   begin
      Empire.Current_Systems := Empire.Current_Systems + 1;
      Concorde.Empires.Logging.Log
        (Empire,
         "new system count:"
         & Empire.Current_Systems'Img);
      Empire.Clear_Path_Cache;
      if Empire.Capital = null then
         Empire.Capital := Concorde.Systems.Db.Reference (System);
         System.Set_Capital (True);
         Concorde.Empires.Logging.Log
           (Empire,
            System.Name & " is our new capital");
      end if;

      Empire.Update_System_Owner (System);

   end System_Acquired;

   -----------------
   -- System_Lost --
   -----------------

   procedure System_Lost
     (Empire : in out Root_Empire_Type'Class;
      System : in out Concorde.Systems.Root_Star_System_Type'Class)
   is
   begin
      Empire.Current_Systems := Empire.Current_Systems - 1;
      Empire.Clear_Path_Cache;

      Concorde.Empires.Logging.Log
        (Empire,
         "new system count:"
         & Empire.Current_Systems'Img);
      if System.Capital then
         Empires.Logging.Log
           (Empire,
            "lost its capital " & System.Name);
         System.Set_Capital (False);
         declare
            use Concorde.Systems;

            function Score
              (Test : Concorde.Systems.Star_System_Type)
               return Natural;

            procedure Set_Capital
              (System : in out Root_Star_System_Type'Class);

            -----------
            -- Score --
            -----------

            function Score
              (Test : Concorde.Systems.Star_System_Type)
               return Natural
            is
               use type Memor.Database_Reference;
               Result : Natural := 0;
            begin
               if Empire.Owned_System (Test)
                 and then Test.Reference /= System.Reference
               then
                  Result := 100 + Test.Ships;

                  for N of Concorde.Galaxy.Neighbours (Test.Index) loop
                     if Empire.Owned_System (N) then
                        Result := Result + 5;
                     elsif N.Owned then
                        if Concorde.Empires.Relations.At_War
                          (Empire, N.Owner.all)
                        then
                           Result := Result - 10;
                        else
                           Result := Result - 2;
                        end if;
                     end if;
                  end loop;
               end if;

               return Result;
            end Score;

            -----------------
            -- Set_Capital --
            -----------------

            procedure Set_Capital
              (System : in out Root_Star_System_Type'Class)
            is
            begin
               System.Set_Capital (True);
            end Set_Capital;

            New_Capital : constant Concorde.Systems.Star_System_Type :=
                            Galaxy.Maximum (Score'Access);

         begin
            if New_Capital /= null then
               Concorde.Systems.Db.Update (New_Capital.Reference,
                                           Set_Capital'Access);
               Empire.Capital := New_Capital;
               Empires.Logging.Log
                 (Empire, "new capital: " & New_Capital.Name);
            else
               Empires.Logging.Log
                 (Empire, "eliminated");
               Empire.Capital := null;
            end if;
         end;
      end if;
   end System_Lost;

   -------------------------
   -- Update_System_Owner --
   -------------------------

   procedure Update_System_Owner
     (Owner  : in out Root_Empire_Type'Class;
      System : Concorde.Systems.Root_Star_System_Type'Class)
   is
      use Concorde.Galaxy;
      Border : Boolean := False;
   begin
      for N of Neighbours (System.Index) loop
         if System.Owner /= N.Owner then
            Border := True;
            Owner.Set (N, Concorde.Empires.Neighbour);
            if N.Owned then
               if N.Owner /= System.Owner then
                  Owner.Set (System, Concorde.Empires.Border);
                  Owner.Set (N, Concorde.Empires.Neighbour);
               end if;
            else
               Owner.Set (System, Concorde.Empires.Frontier);
               Owner.Set (N, Concorde.Empires.Neighbour);
            end if;
         end if;
      end loop;
      if not Border then
         Owner.Set (System, Concorde.Empires.Internal);
      end if;
   end Update_System_Owner;

end Concorde.Empires;
