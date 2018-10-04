with Ada.Characters.Handling;

with Concorde.People.Communities;
with Concorde.Worlds;

package body Concorde.Corporations is

   ------------------
   -- Daily_Budget --
   ------------------

   overriding function Daily_Budget
     (Corporation  : Root_Corporation_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Money_Type
   is
   begin
      if Corporation.Commodities.Contains (Commodity) then
         return Corporation.Cash;
      else
         return Concorde.Money.Zero;
      end if;
   end Daily_Budget;

   -----------------
   -- Daily_Needs --
   -----------------

   overriding function Daily_Needs
     (Corporation  : Root_Corporation_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      if Corporation.Commodities.Contains (Commodity) then
         case Corporation.Business is
            when Import =>
               return Concorde.Quantities.Zero;
            when Export =>
               declare
                  use Concorde.Quantities;
                  Proportion : constant Unit_Real :=
                                 1.0 / Real (Corporation.Commodities.Length);
                  Current    : constant Quantity_Type :=
                              Corporation.Get_Quantity (Commodity);
                  Remaining : constant Quantity_Type :=
                                 Corporation.Available_Capacity;
                  Requirement : constant Quantity_Type :=
                                  Concorde.Commodities.Get_Quantity
                                    (Corporation.Requirements,
                                     Commodity);
               begin
                  return Current
                    + Min (Requirement,
                           Scale (Remaining, Proportion));
               end;
            when Banking =>
               return Concorde.Quantities.Zero;
         end case;
      else
         return Concorde.Quantities.Zero;
      end if;
   end Daily_Needs;

   ------------------
   -- Daily_Supply --
   ------------------

   overriding function Daily_Supply
     (Corporation  : Root_Corporation_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      case Corporation.Business is
         when Import =>
            return Concorde.Quantities.Scale
              (Corporation.Get_Quantity (Commodity), 0.5);
         when Export =>
            return Concorde.Quantities.Zero;
         when Banking =>
            return Concorde.Quantities.Zero;
      end case;
   end Daily_Supply;

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Corporation : Root_Corporation_Type) return String
   is
   begin
      return Memor.To_String (Corporation.Reference)
        & "-"
        & Ada.Characters.Handling.To_Lower
        (Corporation.Business'Image);
   end Identifier;

   ------------------
   -- Perform_Work --
   ------------------

   procedure Perform_Work
     (Corporation : in out Root_Corporation_Type'Class)
   is
   begin
      case Corporation.Business is
         when Banking =>
            null;
         when Import =>
            for Commodity of Corporation.Commodities loop
               declare
                  Quantity : constant Concorde.Quantities.Quantity_Type :=
                               Concorde.Quantities.Scale
                                 (Corporation.Available_Capacity,
                                  1.0 / Real (Corporation.Commodities.Length));
                  Price    : constant Concorde.Money.Price_Type :=
                               Concorde.Money.Adjust_Price
                                 (Corporation.Community.Current_Price
                                    (Commodity), 0.9);

               begin
                  Corporation.Log
                    ("importing " & Concorde.Quantities.Show (Quantity)
                     & " " & Commodity.Identifier
                     & " for " & Concorde.Money.Show (Price));

                  Corporation.Community.Update.Import
                    (Importer  => Db.Reference (Corporation),
                     Commodity => Commodity,
                     Quantity  => Quantity,
                     Price     => Price);
               end;
            end loop;
         when Export =>
            declare
               procedure Add_Export
                 (Commodity : Concorde.Commodities.Commodity_Type);

               ----------------
               -- Add_Export --
               ----------------

               procedure Add_Export
                 (Commodity : Concorde.Commodities.Commodity_Type)
               is
               begin
                  Corporation.Community.Update.Export
                    (Exporter  => Db.Reference (Corporation),
                     Commodity => Commodity,
                     Quantity  => Corporation.Get_Quantity (Commodity),
                     Price     =>
                       Concorde.Money.Adjust_Price
                         (Corporation.Get_Average_Price (Commodity),
                          1.1));
               end Add_Export;

            begin
               Corporation.Scan_Stock (Add_Export'Access);
            end;
      end case;
   end Perform_Work;

   -----------------
   -- Set_Manager --
   -----------------

   overriding procedure Set_Manager
     (Corporation    : in out Root_Corporation_Type;
      Manager     : Concorde.Managers.Manager_Type)
   is
   begin
      Corporation.Manager := Manager;
   end Set_Manager;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Corporation_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

   ------------------
   -- Update_Agent --
   ------------------

   overriding procedure Update_Agent
     (Corporation            : not null access constant Root_Corporation_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class))
   is
   begin
      Perform_Update (Corporation.Update);
   end Update_Agent;
end Concorde.Corporations;
