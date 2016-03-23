with Concorde.Empires.Logging;
with Concorde.Galaxy;
with Concorde.Systems.Graphs;

package body Concorde.Empires is

   function Image
     (Path : Concorde.Systems.Graphs.Array_Of_Vertices)
      return String;

   ----------------
   -- Add_Empire --
   ----------------

   procedure Add_Empire (Empire : Empire_Type) is
   begin
      Vector.Append (Empire);
      Empire.Index := Vector.Last_Index;
   end Add_Empire;

   ---------------
   -- Add_Focus --
   ---------------

   procedure Add_Focus
     (Empire   : in out Root_Empire_Type'Class;
      Focus    : Concorde.Systems.Star_System_Type;
      Priority : Non_Negative_Real := 1.0)
   is
      pragma Unreferenced (Priority);
      Added : Boolean;
   begin
      Empire.System_Data (Focus.Index).Focus := True;
      Empire.Focus_List.Add_If_Missing (Focus, Added);
      if Added then
         Concorde.Empires.Logging.Log
           (Empire'Unchecked_Access,
            "add focus: " & Focus.Name);
      end if;
   end Add_Focus;

   --------
   -- AI --
   --------

   function AI
     (Empire : Root_Empire_Type'Class)
      return access Concorde.AI.Root_AI_Type'Class
   is
   begin
      return Empire.AI;
   end AI;

   ------------------------------
   -- Available_Ship_Capacity --
   ------------------------------

   function Available_Ship_Capacity
     (Empire : Root_Empire_Type'Class)
      return Natural
   is
   begin
      return Integer'Max (Empire.Max_Ships - Empire.Current_Ships, 0);
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
      To      : not null access constant Root_Empire_Type'Class;
      Change  : Empire_Relationship_Range)
   is
      Current_Value : constant Integer := Integer (Empire.Relationship (To));
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

   -----------------
   -- Check_Cache --
   -----------------

   procedure Check_Cache
     (Empire   : in out Root_Empire_Type'Class;
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
            is (Empire.Owned_System (System));

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
                 (Natural'Last, 0);
            end if;

         end;
      end if;
   end Check_Cache;

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
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
   is
      Flags : Empire_Star_System_Record renames
                Empire.System_Data (System.Index);
   begin
      Flags.Internal := False;
      Flags.Frontier := False;
      Flags.Border   := False;
      Flags.Neighbour := False;
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

   ------------------
   -- Empire_Count --
   ------------------

   function Empire_Count return Natural is
   begin
      return Vector.Last_Index;
   end Empire_Count;

   ---------
   -- Get --
   ---------

   function Get (Index : Positive) return Empire_Type is
   begin
      return Vector.Element (Index);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Rank_Type : Ranking;
      Index     : Positive)
      return Empire_Type
   is
      Rank  : array (1 .. Vector.Last_Index) of Empire_Type;
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
      for E of Vector loop
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
      return Rank (Index);
   end Get;

   ---------------
   -- Has_Focus --
   ---------------

   function Has_Focus
     (Empire : Root_Empire_Type'Class;
      Focus  : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is
   begin
      return Empire.System_Data (Focus.Index).Focus;
   end Has_Focus;

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

   ----------------------
   -- Is_Attack_Target --
   ----------------------

   function Is_Attack_Target
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is
   begin
      return Empire.System_Data (System.Index).Attack;
   end Is_Attack_Target;

   ---------------
   -- Is_Border --
   ---------------

   function Is_Border
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is
   begin
      return Empire.System_Data (System.Index).Border;
   end Is_Border;

   -----------------
   -- Is_Frontier --
   -----------------

   function Is_Frontier
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is
   begin
      return Empire.System_Data (System.Index).Frontier;
   end Is_Frontier;

   -----------------
   -- Is_Internal --
   -----------------

   function Is_Internal
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is
   begin
      return Empire.System_Data (System.Index).Internal;
   end Is_Internal;

   ------------------
   -- Is_Neighbour --
   ------------------

   function Is_Neighbour
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is
   begin
      return Empire.System_Data (System.Index).Neighbour;
   end Is_Neighbour;

   ------------------------------
   -- Maximum_Supported_Ships --
   ------------------------------

   function Maximum_Supported_Ships
     (Empire : Root_Empire_Type'Class)
      return Natural
   is
   begin
      return Empire.Max_Ships;
   end Maximum_Supported_Ships;

   -------------------------
   -- Minimum_Score_Focus --
   -------------------------

   function Minimum_Score_Focus
     (Empire : Root_Empire_Type'Class;
      Score  : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Natural)
      return Concorde.Systems.Star_System_Type
   is
      Min_Score  : Natural := Natural'Last;
      Min_System : Concorde.Systems.Star_System_Type := null;

      procedure Update (System : Concorde.Systems.Star_System_Type);

      ------------
      -- Update --
      ------------

      procedure Update (System : Concorde.Systems.Star_System_Type) is
         This_Score : constant Natural := Score (System);
      begin
         if This_Score < Min_Score then
            Min_Score := This_Score;
            Min_System := System;
         end if;
      end Update;

   begin

      Empire.Focus_List.Iterate (Update'Access);
      return Min_System;

   end Minimum_Score_Focus;

   ----------------------
   -- Neighbour_System --
   ----------------------

   function Neighbour_System
     (Empire : Root_Empire_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is
   begin
      return Empire.System_Data (System.Index).Neighbour;
   end Neighbour_System;

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
     (Empire : in out Root_Empire_Type'Class;
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

   ------------------
   -- Owned_System --
   ------------------

   function Owned_System
     (Empire : Root_Empire_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is
   begin
      return System.Owned and then System.Owner.Index = Empire.Index;
   end Owned_System;

   -----------------
   -- Path_Length --
   -----------------

   function Path_Length
     (Empire : in out Root_Empire_Type'Class;
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

   ------------------
   -- Relationship --
   ------------------

   function Relationship
     (Empire : Root_Empire_Type'Class;
      To     : not null access constant Root_Empire_Type'Class)
      return Empire_Relationship_Range
   is
   begin
      if Empire.Empire_Data = null then
         return 0;
      else
         return Empire.Empire_Data (To.Index).Relationship;
      end if;
   end Relationship;

   ------------------
   -- Remove_Focus --
   ------------------

   procedure Remove_Focus
     (Empire : in out Root_Empire_Type'Class;
      Focus  : Concorde.Systems.Star_System_Type)
   is
      Deleted : Boolean;
   begin
      Empire.System_Data (Focus.Index).Focus := False;
      Empire.Focus_List.Delete_If_Present (Focus, Deleted);
      if Deleted then
         Concorde.Empires.Logging.Log
           (Empire'Unchecked_Access,
            "remove focus: " & Focus.Name);
      end if;
   end Remove_Focus;

   ------------------
   -- Remove_Focus --
   ------------------

   procedure Remove_Focus
     (Empire : in out Root_Empire_Type'Class;
      Matching : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Boolean)
   is
      Systems : constant Concorde.Galaxy.Array_Of_Star_Systems :=
                  Concorde.Galaxy.Get_Systems
                    (Matching);
   begin
      for System of Systems loop
         Empire.System_Data (System.Index).Focus := False;
      end loop;
      Empire.Focus_List.Delete_Matching (Matching);
   end Remove_Focus;

   -----------------
   -- Remove_Ship --
   -----------------

   procedure Remove_Ship
     (Empire : in out Root_Empire_Type'Class)
   is
   begin
      Empire.Current_Ships := Empire.Current_Ships - 1;
      Empire.Lost_Ships := Empire.Lost_Ships + 1;
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

   -----------------------
   -- Set_Attack_Target --
   -----------------------

   procedure Set_Attack_Target
     (Empire        : in out Root_Empire_Type'Class;
      System        : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Attack_Target : Boolean)
   is
   begin
      Empire.System_Data (System.Index).Attack := Attack_Target;
   end Set_Attack_Target;

   ----------------
   -- Set_Border --
   ----------------

   procedure Set_Border
     (Empire  : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Border   : Boolean)
   is
   begin
      Empire.System_Data (System.Index).Border := Border;
   end Set_Border;

   ------------------
   -- Set_Frontier --
   ------------------

   procedure Set_Frontier
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Frontier : Boolean)
   is
   begin
      Empire.System_Data (System.Index).Frontier := Frontier;
   end Set_Frontier;

   ------------------
   -- Set_Internal --
   ------------------

   procedure Set_Internal
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Internal : Boolean)
   is
   begin
      Empire.System_Data (System.Index).Internal := Internal;
   end Set_Internal;

   -------------------
   -- Set_Neighbour --
   -------------------

   procedure Set_Neighbour
     (Empire    : in out Root_Empire_Type'Class;
      System    : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Neighbour : Boolean)
   is
   begin
      Empire.System_Data (System.Index).Neighbour := Neighbour;
   end Set_Neighbour;

   ----------------------
   -- Set_Relationship --
   ----------------------

   procedure Set_Relationship
     (Empire : in out Root_Empire_Type'Class;
      To     : not null access constant Root_Empire_Type'Class;
      Value  : Empire_Relationship_Range)
   is
   begin
      if Empire.Empire_Data = null then
         Empire.Empire_Data := new Empire_Data_Array (1 .. Empire_Count);
      end if;

      Empire.Empire_Data (To.Index).Relationship := Value;
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
      System : Concorde.Systems.Star_System_Type)
   is
      pragma Unreferenced (System);
   begin
      Empire.Current_Systems := Empire.Current_Systems + 1;
      Empire.Clear_Path_Cache;
   end System_Acquired;

   -----------------
   -- System_Lost --
   -----------------

   procedure System_Lost
     (Empire : in out Root_Empire_Type'Class;
      System : Concorde.Systems.Star_System_Type)
   is
   begin
      Empire.Current_Systems := Empire.Current_Systems - 1;
      if System.Capital then
         Empires.Logging.Log
           (Empire,
            "lost its capital " & System.Name);
         Galaxy.Set_Capital (System, False);
         declare
            use Concorde.Systems;
            function Score
              (Test : Concorde.Systems.Star_System_Type)
               return Natural
            is (if Empire.Owned_System (Test)
                then Test.Ships + 10
                else 0);
            New_Capital : constant Concorde.Systems.Star_System_Type :=
                            Galaxy.Maximum (Score'Access);
         begin
            if New_Capital /= null then
               Galaxy.Set_Capital (New_Capital, True);
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
      Empire.Clear_Path_Cache;
   end System_Lost;

   ------------
   -- Unload --
   ------------

   procedure Unload is
   begin
      Vector.Clear;
   end Unload;

end Concorde.Empires;
