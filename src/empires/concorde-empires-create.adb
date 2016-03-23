with Ada.Text_IO;

with Concorde.Galaxy;
with Concorde.Systems;

package body Concorde.Empires.Create is

   ----------------
   -- New_Empire --
   ----------------

   procedure New_Empire
     (Name    : String;
      Capital : String;
      Colour  : Lui.Colours.Colour_Type;
      AI      : Concorde.AI.AI_Type      := null)
   is
      function OK_For_Start
        (System : Concorde.Systems.Star_System_Type)
         return Boolean;

      procedure Choose
        (System : in out Concorde.Systems.Root_Star_System_Type'Class);

      Taken : Concorde.Galaxy.Star_System_Set;

      New_Empire : constant Empire_Type := new Root_Empire_Type;

      ------------
      -- Choose --
      ------------

      procedure Choose
        (System : in out Concorde.Systems.Root_Star_System_Type'Class)
      is
      begin
         System.Set_Owner (New_Empire);
         System.Set_Production (System.Production * 4.0);
         System.Set_Capacity (System.Capacity * 4.0);
         System.Set_Capital (True);
         System.Set_Name (Capital);
      end Choose;

      ------------------
      -- OK_For_Start --
      ------------------

      function OK_For_Start
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is
         use Concorde.Galaxy;
         Ns : constant Array_Of_Star_Systems :=
                Neighbours (System);
      begin
         if System.Owner /= null then
            return False;
         end if;

         if Concorde.Galaxy.Is_Element (Taken, System) then
            return False;
         end if;

         for S of Ns loop
            if S.Owner /= null then
               return False;
            end if;
         end loop;
         return True;
      end OK_For_Start;

   begin

      for E of Vector loop
         Concorde.Galaxy.Add_Systems (Taken, E.Capital, 0.4);
      end loop;

      declare
         Start      : constant Concorde.Systems.Star_System_Type :=
                        Concorde.Galaxy.Find_System
                          (OK_For_Start'Access);
      begin
         New_Empire.Set_Name (Name);
         New_Empire.Focus_List := new List_Of_Focus_Systems.List;
         New_Empire.System_Data :=
           new System_Data_Array (1 .. Galaxy.System_Count);
         New_Empire.Colour := Colour;
         New_Empire.Capital := Start;
         New_Empire.AI := AI;
         New_Empire.Current_Systems := 1;
         Concorde.Galaxy.Update_System (Start, Choose'Access);
         Add_Empire (New_Empire);
         Ada.Text_IO.Put_Line ("New empire: " & Name);
      end;
   end New_Empire;

end Concorde.Empires.Create;
