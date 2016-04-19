with Concorde.Galaxy;
with Concorde.Ships.Create;
with Concorde.Stars;
with Concorde.Systems;

with Concorde.Colonies.Configure;

with Concorde.Empires.Db;

package body Concorde.Empires.Create is

   ----------------
   -- New_Empire --
   ----------------

   function New_Empire
     (Name                : String;
      Capital             : String;
      Colour              : Lui.Colours.Colour_Type;
      Default_Ship_Design : String;
      Player              : Concorde.Players.Player_Type)
      return Empire_Type
   is

      Taken : Concorde.Galaxy.Star_System_Set;

      procedure Add_Taken_Systems
        (Empire : Root_Empire_Type'Class);

      procedure Create
        (New_Empire : in out Root_Empire_Type'Class);

      -----------------------
      -- Add_Taken_Systems --
      -----------------------

      procedure Add_Taken_Systems
        (Empire : Root_Empire_Type'Class)
      is
      begin
         Concorde.Galaxy.Add_Systems (Taken, Empire.Capital, 0.4);
      end Add_Taken_Systems;

      ------------
      -- Create --
      ------------

      procedure Create
        (New_Empire : in out Root_Empire_Type'Class)
      is

         procedure Choose
           (System : in out Concorde.Systems.Root_Star_System_Type'Class);

         function OK_For_Start
           (System : Concorde.Systems.Star_System_Type)
            return Boolean;

         ------------
         -- Choose --
         ------------

         procedure Choose
           (System : in out Concorde.Systems.Root_Star_System_Type'Class)
         is
         begin
            System.Set_Owner (Db.Reference (New_Empire));
            System.Set_Production (System.Production * 4.0);
            System.Set_Capacity (System.Capacity * 4.0);
            System.Set_Capital (True);
            System.Set_Name (Capital);

            Concorde.Colonies.Configure.Create_Colony_From_Template
              (System, "initial");

            for I in 1 .. 2 loop
               declare
                  Ship : constant Concorde.Ships.Ship_Type :=
                           Concorde.Ships.Create.New_Ship
                             (Owner  => System.Owner,
                              Name   =>
                                System.Owner.Name
                              & " Trader" & I'Img,
                              System => System,
                              Design => "trader");
               begin
                  System.Add_Ship (Ship);
               end;
            end loop;
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

            case Concorde.Stars.Star_Type (System.Main_Object).Stellar_Class is
               when Concorde.Stars.G =>
                  return True;
               when others =>
                  return False;
            end case;
         end OK_For_Start;

         Start      : constant Concorde.Systems.Star_System_Type :=
                        Concorde.Galaxy.Find_System
                          (OK_For_Start'Access);
      begin
         New_Empire.New_Agent (Start);
         New_Empire.Identifier :=
           Ada.Strings.Unbounded.To_Unbounded_String (Name);
         New_Empire.Set_Name (Name);
         New_Empire.System_Data :=
           new System_Data_Array (1 .. Galaxy.System_Count);
         New_Empire.Colour := Colour;
         New_Empire.Capital := Start;
         New_Empire.Player := Player;
         New_Empire.Current_Systems := 1;
         New_Empire.Default_Ship := new String'(Default_Ship_Design);
         Concorde.Galaxy.Update_System (Start, Choose'Access);
      end Create;

   begin
      Db.Scan (Add_Taken_Systems'Access);
      return Db.Create (Create'Access);
   end New_Empire;

end Concorde.Empires.Create;
