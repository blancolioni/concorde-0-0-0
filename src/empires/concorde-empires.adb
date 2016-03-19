with Concorde.Empires.Logging;

package body Concorde.Empires is

   ----------------
   -- Add_Empire --
   ----------------

   procedure Add_Empire (Empire : Empire_Type) is
   begin
      Vector.Append (Empire);
   end Add_Empire;

   ---------------
   -- Add_Focus --
   ---------------

   procedure Add_Focus
     (Empire : in out Root_Empire_Type'Class;
      Focus  : Concorde.Systems.Star_System_Type)
   is
      Added : Boolean;
   begin
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
   -- Available_Fleet_Capacity --
   ------------------------------

   function Available_Fleet_Capacity
     (Empire : Root_Empire_Type'Class)
      return Natural
   is
   begin
      return Integer'Max (Empire.Max_Fleets - Empire.Current_Fleets, 0);
   end Available_Fleet_Capacity;

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
   -- Current_Fleets --
   --------------------

   function Current_Fleets
     (Empire : Root_Empire_Type'Class)
      return Natural
   is
   begin
      return Empire.Current_Fleets;
   end Current_Fleets;

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
               return X.Current_Fleets > Y.Current_Fleets;
            when By_Fleets =>
               return X.Current_Fleets > Y.Current_Fleets;
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
      Focus  : Concorde.Systems.Star_System_Type)
      return Boolean
   is
   begin
      return Empire.Focus_List.Contains (Focus);
   end Has_Focus;

   ------------------------------
   -- Maximum_Supported_Fleets --
   ------------------------------

   function Maximum_Supported_Fleets
     (Empire : Root_Empire_Type'Class)
      return Natural
   is
   begin
      return Empire.Max_Fleets;
   end Maximum_Supported_Fleets;

   ----------------
   -- New_Fleets --
   ----------------

   procedure New_Fleets
     (Empire : in out Root_Empire_Type'Class;
      Count  : Natural)
   is
   begin
      Empire.Current_Fleets := Empire.Current_Fleets + Count;
   end New_Fleets;

   ------------------
   -- Remove_Focus --
   ------------------

   procedure Remove_Focus
     (Empire : in out Root_Empire_Type'Class;
      Focus  : Concorde.Systems.Star_System_Type)
   is
      Deleted : Boolean;
   begin
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
   begin
      Empire.Focus_List.Delete_Matching (Matching);
   end Remove_Focus;

--        New_List : List_Of_Systems.List;
--        Changed  : Boolean := False;
--     begin
--        for Focus of Empire.Focus_List loop
--           if not Matching (Focus) then
--              New_List.Append (Focus);
--           else
--              Concorde.Empires.Logging.Log
--                (Empire'Unchecked_Access,
--                 "remove focus: " & Focus.Name);
--              Changed := True;
--           end if;
--        end loop;
--        if Changed then
--           Empire.Focus_List := New_List;
--        end if;
--     end Remove_Focus;

end Concorde.Empires;
