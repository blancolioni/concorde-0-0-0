with Xi.Float_Images;

with Concorde.Commodities;
with Concorde.Logs;
with Concorde.Random;
with Concorde.Worlds;
with WL.Money;

package body Concorde.People.Pops is

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   procedure Add_Trade_Offers
     (Item   : not null access constant Root_Pop_Type)
   is
      use Concorde.Commodities, WL.Quantities;
      Group : constant Concorde.People.Groups.Pop_Group :=
                Item.Wealth_Group;

      procedure Check_Food;

      procedure Check_Need
        (Commodity : Concorde.Commodities.Commodity_Type;
         Need      : Non_Negative_Real);

      ----------------
      -- Check_Food --
      ----------------

      procedure Check_Food is
         Food : constant Array_Of_Commodities := Food_Commodities;
         Required_Energy : Real :=
                             Non_Negative_Real (Item.Size)
                             * Group.Energy_Needs;

         Total_Score     : Non_Negative_Real := 0.0;
         Food_Scores     : array (Food'Range) of Non_Negative_Real;

      begin
         for I in Food'Range loop
            declare
               use WL.Money;
               Commodity : constant Commodity_Type := Food (I);

               This_Energy : constant Non_Negative_Real :=
                               Commodity.Energy;
               This_Cost   : constant Price_Type :=
                               Item.Mean_Price_Belief (Commodity);
               Energy_Per_Price : constant Non_Negative_Real :=
                                    This_Energy / Real (To_Float (This_Cost));
            begin
               Food_Scores (I) := Energy_Per_Price;
               Total_Score := Total_Score + Food_Scores (I);
               Required_Energy := Required_Energy
                 - This_Energy
                 * Real (WL.Quantities.To_Float
                         (Item.Get_Quantity (Commodity)));
            end;
         end loop;

         Item.Log
           ("checking food; required energy "
            & Integer'Image (Integer (Required_Energy)));

         if Required_Energy > 0.0 then
            for I in Food'Range loop
               declare
                  use WL.Money;
                  Commodity        : constant Commodity_Type := Food (I);
                  This_Energy      : constant Non_Negative_Real :=
                                       Commodity.Energy;
                  Proportion       : constant Non_Negative_Real :=
                                       Food_Scores (I) / Total_Score;
                  Quantity         : constant Quantity_Type :=
                                       To_Quantity
                                         (Float
                                            (Proportion * Required_Energy
                                             / This_Energy));
               begin
                  Item.Create_Bid (Commodity, Quantity);
               end;
            end loop;
         end if;

      end Check_Food;

      ----------------
      -- Check_Need --
      ----------------

      procedure Check_Need
        (Commodity : Concorde.Commodities.Commodity_Type;
         Need      : Non_Negative_Real)
      is
         Current  : constant Quantity_Type :=
                      Item.Get_Quantity (Commodity)
                      + Item.Current_Bid_Quantity (Commodity);
         Required : constant Non_Negative_Real :=
                      Need * Non_Negative_Real (Item.Size);
      begin
         if Required < 1.0 and then Current = Zero then
            Item.Create_Bid (Commodity, Unit);
         elsif Required >= 1.0
           and then Current < To_Quantity (Float'Ceiling (Float (Required)))
         then
            Item.Create_Bid
              (Commodity,
               To_Quantity (Float'Ceiling (Float (Required))) - Current);
         end if;
      end Check_Need;

   begin

      Check_Food;

      Group.Scan_Needs (Check_Need'Access);

      for Skill of Item.Skills loop
         if Item.Get_Quantity (Skill.Commodity)
           > Item.Current_Ask_Quantity (Skill.Commodity)
         then
            Item.Create_Ask
              (Skill.Commodity,
               Item.Get_Quantity (Skill.Commodity)
                   - Item.Current_Ask_Quantity (Skill.Commodity));
         end if;
      end loop;

   end Add_Trade_Offers;

   -----------------
   -- Affiliation --
   -----------------

   overriding function Affiliation
     (Pop   : Root_Pop_Type;
      Group : Concorde.People.Groups.Pop_Group)
      return Concorde.People.Groups.Affiliation_Range
   is
   begin
      return Pop.Groups.Get_Affiliation_Range (Group);
   end Affiliation;

   -------------------------
   -- Execute_Consumption --
   -------------------------

   procedure Execute_Consumption
     (Pop : in out Root_Pop_Type'Class)
   is
      procedure Consume
        (Commodity : Concorde.Commodities.Commodity_Type;
         Need      : Non_Negative_Real);

      procedure Eat_Food;

      -------------
      -- Consume --
      -------------

      procedure Consume
        (Commodity : Concorde.Commodities.Commodity_Type;
         Need      : Non_Negative_Real)
      is
         use WL.Quantities;
         Total_Need : constant Non_Negative_Real :=
                        Need * Non_Negative_Real (Pop.Size);
         Base_Need  : constant Non_Negative_Real :=
                        Non_Negative_Real'Floor (Total_Need);
         Partial_Need : constant Non_Negative_Real :=
                          Total_Need - Base_Need;
         Available    : constant Quantity_Type :=
                          Pop.Get_Quantity (Commodity);
         Base_Quantity : constant Quantity_Type :=
                           Min (To_Quantity (Float (Base_Need)), Available);
         Extra_Quantity : constant Quantity_Type :=
                            (if Available > Base_Quantity
                             and then Partial_Need > 0.0
                             and then Concorde.Random.Unit_Random
                             < Partial_Need
                             then Unit else Zero);
         Consumed       : constant Quantity_Type :=
                            Base_Quantity + Extra_Quantity;
      begin
         Concorde.Logs.Log_Line
           (Pop.Current_World.Name
            & "/" & Pop.Short_Name
            & "/consumption"
            & "/" & Commodity.Identifier,
            Pop.Identifier
            & "," & Image (Pop.Size_Quantity)
            & "," & Xi.Float_Images.Image (Need)
            & "," & Xi.Float_Images.Image (Total_Need)
            & "," & Image (Available)
            & "," & Image (Consumed));
         Pop.Remove_Quantity (Commodity, Consumed);
      end Consume;

      --------------
      -- Eat_Food --
      --------------

      procedure Eat_Food is
         use Concorde.Commodities;
         use WL.Quantities;

         Food            : constant Array_Of_Commodities := Food_Commodities;
         Required_Energy : constant Non_Negative_Real :=
                             Non_Negative_Real (Pop.Size)
                             * Pop.Wealth_Group.Energy_Needs;

         Total_Energy    : Non_Negative_Real := 0.0;
         Food_Energy     : array (Food'Range) of Non_Negative_Real;

         procedure Eat
           (Commodity : Commodity_Type;
            Quantity  : Quantity_Type);

         ---------
         -- Eat --
         ---------

         procedure Eat
           (Commodity : Commodity_Type;
            Quantity  : Quantity_Type)
         is
         begin
            Concorde.Logs.Log_Line
              (Pop.Current_World.Name
               & "/" & Pop.Short_Name
               & "/eat"
               & "/" & Commodity.Identifier,
               Image (Pop.Size_Quantity)
               & "," & Xi.Float_Images.Image (Required_Energy)
               & "," & Xi.Float_Images.Image (Total_Energy)
               & "," & Image (Pop.Get_Quantity (Commodity))
               & "," & Image (Quantity));
            Pop.Remove_Quantity (Commodity, Quantity);
         end Eat;

      begin
         for I in Food'Range loop
            declare
               use WL.Money;
               Commodity   : constant Commodity_Type := Food (I);
               This_Energy : constant Non_Negative_Real :=
                               Real (To_Float (Pop.Get_Quantity (Commodity)))
                               * Commodity.Energy;
            begin
               Food_Energy (I) := This_Energy;
               Total_Energy := Total_Energy + This_Energy;
            end;
         end loop;

         declare
            Factor : constant Non_Negative_Real :=
                       Total_Energy / Required_Energy;
         begin
            if Factor < 1.0 then
               for I in Food'Range loop
                  if Food_Energy (I) > 0.0 then
                     Eat (Food (I), Pop.Get_Quantity (Food (I)));
                  end if;
               end loop;
            else
               for I in Food'Range loop
                  if Food_Energy (I) > 0.0 then
                     Eat (Food (I),
                          To_Quantity
                            (Float
                               (Food_Energy (I) / Factor / Food (I).Energy)));
                  end if;
               end loop;
            end if;
         end;

      end Eat_Food;

   begin
      Eat_Food;
      Pop.Wealth_Group.Scan_Needs (Consume'Access);
   end Execute_Consumption;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Pop_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   ----------
   -- Size --
   ----------

   function Size (Pop : Root_Pop_Type'Class) return Pop_Size is
   begin
      return Pop.Size;
   end Size;

   -------------------
   -- Size_Quantity --
   -------------------

   function Size_Quantity
     (Pop : Root_Pop_Type'Class)
      return WL.Quantities.Quantity_Type
   is
   begin
      return WL.Quantities.To_Quantity (Float (Pop.Size));
   end Size_Quantity;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Pop_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Concorde.People.Pops;
