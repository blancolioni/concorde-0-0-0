with Concorde.Galaxy;
with Concorde.Ships.Create;
with Concorde.Stars;
with Concorde.Systems;

with Concorde.Scenarios;

with Concorde.Colonies.Configure;

with Concorde.Empires.Db;
with Concorde.Ships.Db;
with Concorde.Systems.Db;

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
            System.Set_Capital (True);
            System.Set_Name (Capital);

            if Concorde.Scenarios.Imperial_Centre
              and then System.Index = 1
            then
               System.Set_Production (System.Production * 100.0);
               System.Set_Capacity (System.Capacity * 100.0);
               Concorde.Colonies.Configure.Create_Colony_From_Template
                 (System, "imperial_capital");
            else
               System.Set_Production (System.Production * 20.0);
               System.Set_Capacity (System.Capacity * 20.0);
               Concorde.Colonies.Configure.Create_Colony_From_Template
                 (System, "initial");
            end if;

            declare
               Trader : constant Concorde.Ships.Ship_Type :=
                          Concorde.Ships.Create.New_Ship
                            (Owner  => System.Owner,
                             Name   =>
                               System.Owner.Name
                             & " Trader",
                             System => System,
                             Design => "trader");
               Defender : constant Concorde.Ships.Ship_Type :=
                            Concorde.Ships.Create.New_Ship
                              (Owner  => System.Owner,
                               Name   =>
                                 System.Owner.Name
                               & " Defender",
                               System => System,
                               Design => "defender");

               procedure Initial_Trade_Route
                 (Ship : in out Concorde.Ships.Root_Ship_Type'Class);

               -------------------------
               -- Initial_Trade_Route --
               -------------------------

               procedure Initial_Trade_Route
                 (Ship : in out Concorde.Ships.Root_Ship_Type'Class)
               is
               begin
                  Ship.Add_Buy_Order
                    (Concorde.Systems.Db.Reference (System),
                     System.Resource);
                  Ship.Add_Sell_Order
                    (Galaxy.Get_System (1), System.Resource);
               end Initial_Trade_Route;

            begin
               Concorde.Ships.Db.Update
                 (Trader.Reference, Initial_Trade_Route'Access);

               System.Add_Ship (Trader);
               System.Add_Ship (Defender);
            end;

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
--              if Imperial_Centre and then System.Index = 1 then
--                 return False;
--              end if;

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

         if False
           and then Concorde.Scenarios.Imperial_Centre
           and then not Concorde.Galaxy.Neighbours (1, Start.Index)
         then
            Concorde.Galaxy.Connect (1, Start.Index);
         end if;

      end Create;

   begin
      Db.Scan (Add_Taken_Systems'Access);
      return Db.Create (Create'Access);
   end New_Empire;

end Concorde.Empires.Create;
