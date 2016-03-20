with Concorde.Empires.Logging;
with Concorde.Galaxy;

package body Concorde.AI is

   ---------------------------
   -- Minimum_Attack_Factor --
   ---------------------------

   function Minimum_Attack_Factor
     (AI : Root_AI_Type)
      return Non_Negative_Real
   is
   begin
      return AI.Defensiveness;
   end Minimum_Attack_Factor;

   -----------
   -- Start --
   -----------

   procedure Start
     (AI     : in out Root_AI_Type;
      Empire : Concorde.Empires.Empire_Type)
   is
      pragma Unreferenced (AI);

      function Owner
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is (System.Owner /= null
          and then System.Owner.Name = Empire.Name);

      procedure Start_System
        (System : Concorde.Systems.Star_System_Type);

      ------------------
      -- Start_System --
      ------------------

      procedure Start_System
        (System : Concorde.Systems.Star_System_Type)
      is
         Ns : constant Concorde.Galaxy.Array_Of_Star_Systems :=
                Concorde.Galaxy.Neighbours (System);
      begin
         for N of Ns loop
            Empire.Add_Focus (N);
         end loop;
      end Start_System;

   begin
      AI.Empire := Empire;
      Concorde.Galaxy.Iterate (Owner'Access, Start_System'Access);
   end Start;

   ---------------------
   -- System_Acquired --
   ---------------------

   procedure System_Acquired
     (AI           : in out Root_AI_Type;
      System       : Concorde.Systems.Star_System_Type;
      Former_Owner : Concorde.Empires.Empire_Type)
   is
      pragma Unreferenced (System);
      use Concorde.Empires;
   begin
--        if AI.Empire.Has_Focus (System) then
--           declare
--              Stop : Boolean := True;
--              Ns   : constant Concorde.Galaxy.Array_Of_Star_Systems :=
--                       Concorde.Galaxy.Neighbours (System);
--           begin
--              for N of Ns loop
--                 if N.Owner = null then
--                    Stop := False;
--                    AI.Empire.Add_Focus (N);
--                 end if;
--              end loop;
--              if not Stop then
--                 AI.Empire.Remove_Focus (System);
--              end if;
--           end;
--        end if;

      if Former_Owner /= null then
         Update_Defensiveness (AI, Former_Owner);
      end if;

   end System_Acquired;

   -----------------
   -- System_Lost --
   -----------------

   procedure System_Lost
     (AI        : in out Root_AI_Type;
      System    : Concorde.Systems.Star_System_Type;
      New_Owner : Concorde.Empires.Empire_Type)
   is
      pragma Unreferenced (System);
   begin
      Update_Defensiveness (AI, New_Owner, Can_Decrease => False);
   end System_Lost;

   --------------------------
   -- Update_Defensiveness --
   --------------------------

   procedure Update_Defensiveness
     (AI           : in out Root_AI_Type'Class;
      Enemy        : Concorde.Empires.Empire_Type;
      Can_Increase : Boolean := True;
      Can_Decrease : Boolean := True)
   is
      D : Non_Negative_Real;
   begin
      if AI.Empire.Current_Ships > Enemy.Current_Ships then
         D := 1.2;
      elsif AI.Empire.Current_Ships = 0
        or else AI.Empire.Current_Ships * 4 < Enemy.Current_Ships
      then
         D := 4.0;
      else
         D :=
           Real (Enemy.Current_Ships) * 1.3
           / Real (AI.Empire.Current_Ships);
      end if;

      if (D < AI.Defensiveness and then Can_Decrease)
        or else (D > AI.Defensiveness and then Can_Increase)
      then
         AI.Defensiveness := (AI.Defensiveness + D) / 2.0;
      end if;

   end Update_Defensiveness;

   ------------------
   -- Update_Focus --
   ------------------

   procedure Update_Focus
     (AI : in out Root_AI_Type)
   is
      function Other_Owner
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is (System.Owner /= null and then System.Owner /= AI.Empire);

      procedure Add_Focus
        (System : Concorde.Systems.Star_System_Type);

      function Border_System
        (System : Concorde.Systems.Star_System_Type)
         return Boolean;

      function Interior
        (System : Concorde.Systems.Star_System_Type)
         return Boolean;

      function Outnumbered_Defenders
        (System : Concorde.Systems.Star_System_Type)
         return Boolean;

      function Unexplored
        (System : Concorde.Systems.Star_System_Type)
         return Boolean;

      ---------------
      -- Add_Focus --
      ---------------

      procedure Add_Focus
        (System : Concorde.Systems.Star_System_Type)
      is
      begin
         AI.Empire.Add_Focus (System);
      end Add_Focus;

      -------------------
      -- Border_System --
      -------------------

      function Border_System
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is
         use Concorde.Galaxy;
         Ns : constant Array_Of_Star_Systems :=
                Galaxy.Neighbours (System);
      begin
         if System.Owner /= AI.Empire then
            return False;
         end if;

         for N of Ns loop
            if N.Owner /= null and then N.Owner /= AI.Empire then
               return True;
            end if;
         end loop;
         return False;
      end Border_System;

      --------------
      -- Interior --
      --------------

      function Interior
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is
         use Concorde.Galaxy;
         Ns : constant Array_Of_Star_Systems :=
                Galaxy.Neighbours (System);
      begin
         if System.Owner /= AI.Empire then
            return False;
         end if;

         for N of Ns loop
            if N.Owner /= null and then N.Owner /= AI.Empire then
               return False;
            end if;
         end loop;
         return True;
      end Interior;

      ---------------------------
      -- Outnumbered_Defenders --
      ---------------------------

      function Outnumbered_Defenders
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is
         use Concorde.Galaxy;
         Ns : constant Array_Of_Star_Systems :=
                Galaxy.Neighbours (System);
         Defenders : Natural := 0;
         Attackers : Natural := 0;
      begin
         if System.Owner = null or else System.Owner = AI.Empire then
            return False;
         end if;

         Defenders := System.Ships;
         for N of Ns loop
            if N.Owner = AI.Empire then
               Attackers := Attackers + N.Ships;
            elsif N.Owner = System.Owner then
               Defenders := Defenders + N.Ships / 2;
            end if;
         end loop;

         if Attackers /= 0 and then Defenders /= 0 then
            declare
               Ratio : constant Real :=
                         Real (Attackers) / Real (Defenders);
               Min   : constant Real :=
                         Root_AI_Type'Class (AI).Minimum_Attack_Factor;
            begin
               if Ratio > Min then
                  Concorde.Empires.Logging.Log
                    (AI.Empire,
                     "ordering attack on " & System.Name
                     & "; attackers" & Attackers'Img
                     & "; defenders" & Defenders'Img
                     & "; ratio = "
                     & Lui.Approximate_Image (Ratio)
                     & "; minimum = "
                     & Lui.Approximate_Image (Min));
                  return True;
               else
                  return False;
               end if;
            end;
         else
            return False;
         end if;
      end Outnumbered_Defenders;

      ----------------
      -- Unexplored --
      ----------------

      function Unexplored
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is
      begin
         if System.Owner /= null then
            return False;
         end if;

         for N of Galaxy.Neighbours (System) loop
            if N.Owner = AI.Empire then
               return True;
            end if;
         end loop;

         return False;

      end Unexplored;

   begin
      AI.Empire.Remove_Focus (Other_Owner'Access);
      AI.Empire.Remove_Focus (Interior'Access);

      Galaxy.Iterate (Border_System'Access, Add_Focus'Access);
      Galaxy.Iterate (Outnumbered_Defenders'Access, Add_Focus'Access);
      Galaxy.Iterate (Unexplored'Access, Add_Focus'Access);
   end Update_Focus;

end Concorde.AI;
