with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

with Concorde.Money;
with Concorde.Quantities;
with WL.String_Maps;

with Concorde.Calendar;
with Concorde.Commodities;
with Concorde.Markets;
with Concorde.Trades;

with Concorde.People.Attributes.Configure;
with Concorde.People.Skills;
with Concorde.People.Pops;

with Concorde.Powers.Ministries;

package body Concorde.Powers.Execution is

   type Power_Pop_Group_Record is
      record
         Group : Concorde.People.Groups.Pop_Group;
         Effect : Duration;
      end record;

   package Power_Pop_Group_Vectors is
     new Ada.Containers.Vectors (Positive, Power_Pop_Group_Record);

   package Attribute_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, Concorde.People.Attributes.Attribute_Reference,
        Concorde.People.Attributes."=");

   type Commodity_Work_Calculation is (By_Metric, By_Transaction_Count);

   type Commodity_Work (Calculation : Commodity_Work_Calculation) is
      record
         Factor : Non_Negative_Real;
         case Calculation is
            when By_Metric =>
               Metric : Concorde.Trades.Quantity_Metric;
            when By_Transaction_Count =>
               null;
         end case;
      end record;

   package Commodity_Work_Holders is
      new Ada.Containers.Indefinite_Holders (Commodity_Work);

   type Population_Work_Calculation is (By_Quantity, By_Pop_Count, By_Wealth);

   type Population_Work (Calculation : Population_Work_Calculation) is
      record
         Factor : Non_Negative_Real;
      end record;

   package Population_Work_Holders is
     new Ada.Containers.Indefinite_Holders (Population_Work);

   type Work_Record is
      record
         Base       : Duration;
         Commodity  : Commodity_Work_Holders.Holder;
         Population : Population_Work_Holders.Holder;
      end record;

   type Power_Execution_Record is
      record
         Group       : Concorde.People.Groups.Pop_Group;
         Attributes  : Attribute_Vectors.Vector;
         Pop_Groups  : Power_Pop_Group_Vectors.Vector;
         Action_Cost : Work_Record;
         Daily_Work  : Work_Record;
      end record;

   package Power_Execution_Maps is
     new WL.String_Maps (Power_Execution_Record);

   Power_Execution_Map : Power_Execution_Maps.Map;

   procedure Configure_Work
     (Config : Tropos.Configuration;
      Target : in out Work_Record);

   function Configure_Commodity_Work
     (Config : Tropos.Configuration)
      return Commodity_Work;

   function Configure_Population_Work
     (Config : Tropos.Configuration)
      return Population_Work;

   function Calculate_Work
     (Power     : Power_Type;
      Work      : Work_Record;
      Community : access constant
        Concorde.People.Communities.Root_Community_Type'Class)
      return Duration;

   function P (Power : Power_Type) return Power_Execution_Record;

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Power : Power_Type;
      Index : Positive)
      return Concorde.People.Attributes.Attribute_Reference
   is
   begin
      return P (Power).Attributes (Index);
   end Attribute;

   ---------------------
   -- Attribute_Count --
   ---------------------

   function Attribute_Count
     (Power : Power_Type)
      return Natural
   is
   begin
      return P (Power).Attributes.Last_Index;
   end Attribute_Count;

   --------------------
   -- Calculate_Work --
   --------------------

   function Calculate_Work
     (Power     : Power_Type;
      Work      : Work_Record;
      Community : access constant
        Concorde.People.Communities.Root_Community_Type'Class)
      return Duration
   is
      Result : Duration := Work.Base;
      Market : constant Concorde.Markets.Market_Type :=
                 (if Community = null
                  then null
                  else Community.Market);
   begin
      if not Work.Commodity.Is_Empty then
         declare
            Item : Commodity_Work renames
                     Work.Commodity.Element;

            procedure Add_Commodity
              (Commodity : Concorde.Commodities.Commodity_Type);

            -------------------
            -- Add_Commodity --
            -------------------

            procedure Add_Commodity
              (Commodity : Concorde.Commodities.Commodity_Type)
            is
               W : Duration := 0.0;
            begin
               case Item.Calculation is
                  when By_Metric =>
                     W :=
                       Duration
                         (Concorde.Quantities.To_Real
                            (Market.Current_Quantity
                               (Item.Metric, Commodity))
                          * Item.Factor);
                  when By_Transaction_Count =>
                     W :=
                       Duration
                         (Real
                            (Market.Recent_Transaction_Count (Commodity))
                          * Item.Factor);
               end case;
               if W > 0.0 then
                  W := W + Concorde.Calendar.Hours (1);
               end if;
               Result := Result + W;
            end Add_Commodity;

         begin
            Concorde.Commodities.Scan (Add_Commodity'Access);
         end;
      end if;

      if not Work.Population.Is_Empty then
         declare

            Item  : constant Population_Work :=
                      Work.Population.Element;

            procedure Add_Pop (Pop : Concorde.People.Pops.Pop_Type);

            -------------
            -- Add_Pop --
            -------------

            procedure Add_Pop (Pop : Concorde.People.Pops.Pop_Type) is
               W : Duration := 0.0;
            begin
               case Item.Calculation is
                  when By_Quantity =>
                     W := W + Duration (Real (Pop.Size) * Item.Factor);
                  when By_Pop_Count =>
                     W := W + Duration (Item.Factor);
                  when By_Wealth =>
                     W := W + Duration (Concorde.Money.To_Real (Pop.Cash)
                                        * Item.Factor);
               end case;

               Result := Result + W;

            end Add_Pop;

         begin
            Community.Scan_Pops (Add_Pop'Access);
         end;

      end if;

      if Concorde.Powers.Ministries.Is_Direct_Minister (Power)
        and then not Concorde.Powers.Ministries.Ministry (Power).Has_Minister
      then
         Result := Result
           + Concorde.Powers.Ministries.Ministry (Power).Daily_Work;
      end if;

      return Result;
   end Calculate_Work;

   ------------------------------
   -- Configure_Commodity_Work --
   ------------------------------

   function Configure_Commodity_Work
     (Config : Tropos.Configuration)
      return Commodity_Work
   is
      Value : constant String := Config.Get ("value", "none");
   begin
      if Value = "recent_transaction_count" then
         return Commodity_Work'
           (Calculation => By_Transaction_Count,
            Factor      =>
              Non_Negative_Real (Float'(Config.Get ("factor", 1.0))));
      elsif Value = "daily_quantity" then
         return Commodity_Work'
           (Calculation => By_Metric,
            Factor      =>
              Non_Negative_Real (Float'(Config.Get ("factor", 1.0))),
            Metric      =>
              Concorde.Trades.Quantity_Metric'Value
                (Config.Get ("metric")));
      else
         raise Constraint_Error with
           "unknown commodity work type: " & Value;
      end if;
   end Configure_Commodity_Work;

   -------------------------------
   -- Configure_Population_Work --
   -------------------------------

   function Configure_Population_Work
     (Config : Tropos.Configuration)
      return Population_Work
   is
      Value : constant String := Config.Get ("value", "none");
      Factor : constant Non_Negative_Real :=
                 Non_Negative_Real (Float'(Config.Get ("factor", 1.0)));
   begin
      if Value = "quantity" then
         return (By_Quantity, Factor);
      elsif Value = "pop_count" then
         return (By_Pop_Count, Factor);
      elsif Value = "wealth" then
         return (By_Wealth, Factor);
      else
         raise Constraint_Error with
           "unknown population work type: " & Value;
      end if;
   end Configure_Population_Work;

   -------------------------------
   -- Configure_Power_Execution --
   -------------------------------

   procedure Configure_Power_Execution
     (Config : Tropos.Configuration)
   is
      Rec : Power_Execution_Record;
   begin
      for Attr_Config of Config.Child ("attributes") loop
         Rec.Attributes.Append
           (Concorde.People.Attributes.Configure.Configure_Attribute
              (Attr_Config));
      end loop;

      for Group_Config of Config.Child ("pops") loop
         Rec.Pop_Groups.Append
           (Power_Pop_Group_Record'
              (Group  =>
                   Concorde.People.Groups.Get (Group_Config.Config_Name),
               Effect =>
                 Concorde.Calendar.Hours
                   (Group_Config.Get ("effect"))));
      end loop;

      Configure_Work (Config.Child ("action_cost"), Rec.Action_Cost);
      Configure_Work (Config.Child ("daily_work"), Rec.Daily_Work);

      if Config.Get ("arg", "") = "pop_group" then
         for Group of Concorde.People.Groups.All_Groups loop
            Rec.Group := Group;
            Power_Execution_Map.Insert
              (Config.Config_Name & "-" & Group.Identifier, Rec);
         end loop;
      else
         Power_Execution_Map.Insert (Config.Config_Name, Rec);
      end if;
   end Configure_Power_Execution;

   --------------------
   -- Configure_Work --
   --------------------

   procedure Configure_Work
     (Config : Tropos.Configuration;
      Target : in out Work_Record)
   is
   begin
      if Config.Child_Count = 1
        and then not Config.Contains ("base")
      then
         Target.Base := Concorde.Calendar.Hours (Config.Value);
      else
         Target.Base := Concorde.Calendar.Hours (Config.Get ("base", 0));
         if Config.Contains ("commodity") then
            Target.Commodity.Replace_Element
              (Configure_Commodity_Work (Config.Child ("commodity")));
         end if;
         if Config.Contains ("population") then
            Target.Population.Replace_Element
              (Configure_Population_Work (Config.Child ("population")));
         end if;
      end if;
   end Configure_Work;

   ----------------
   -- Daily_Work --
   ----------------

   function Daily_Work
     (Power : Power_Type;
      Community : not null access constant
        Concorde.People.Communities.Root_Community_Type'Class)
      return Duration
   is
      Rec : constant Power_Execution_Record := P (Power);
   begin
      return Calculate_Work (Power, Rec.Daily_Work, Community);
   end Daily_Work;

   --------------------
   -- Execution_Work --
   --------------------

   function Execution_Work
     (Power  : Power_Type;
      Target : access constant
        Concorde.Objects.Root_Object_Type'Class)
      return Duration
   is
      Rec : constant Power_Execution_Record := P (Power);
   begin
      return Calculate_Work (Power, Rec.Action_Cost,
                             Concorde.People.Communities.Community_Type
                               (Target));
   end Execution_Work;

   -------
   -- P --
   -------

   function P (Power : Power_Type) return Power_Execution_Record
   is
      Id : constant String := Power.Class_Identifier;
   begin
      if Power_Execution_Map.Contains (Id) then
         return Power_Execution_Map.Element (Id);
      else
         raise Constraint_Error with
           "no such power: " & Id;
      end if;
   end P;

   ---------------
   -- Pop_Group --
   ---------------

   function Pop_Group
     (Power : Power_Type;
      Index : Positive)
      return Concorde.People.Groups.Pop_Group
   is
   begin
      return P (Power).Pop_Groups.Element (Index).Group;
   end Pop_Group;

   ---------------------
   -- Pop_Group_Count --
   ---------------------

   function Pop_Group_Count
     (Power : Power_Type)
      return Natural
   is
   begin
      return P (Power).Pop_Groups.Last_Index;
   end Pop_Group_Count;

   ----------------------
   -- Pop_Group_Effect --
   ----------------------

   function Pop_Group_Effect
     (Power : Power_Type;
      Index : Positive)
      return Duration
   is
   begin
      return P (Power).Pop_Groups.Element (Index).Effect;
   end Pop_Group_Effect;

end Concorde.Powers.Execution;
