with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;

with WL.Quantities;
with WL.String_Maps;

with Concorde.Calendar;
with Concorde.Commodities;
with Concorde.People.Skills;

with Concorde.People.Attributes.Configure;

package body Concorde.Powers.Execution is

   type Power_Pop_Group_Record is
      record
         Group : Concorde.People.Groups.Pop_Group;
         Effect : Duration;
      end record;

   package Power_Pop_Group_Vectors is
     new Ada.Containers.Vectors (Positive, Power_Pop_Group_Record);

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

   type Work_Record is
      record
         Base      : Duration;
         Commodity : Commodity_Work_Holders.Holder;
      end record;

   type Power_Execution_Record is
      record
         Attributes       : Concorde.People.Attributes.Attribute_Container;
         Pop_Groups       : Power_Pop_Group_Vectors.Vector;
         Legislative_Work : Work_Record;
         Daily_Work       : Work_Record;
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

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Power : Power_Type;
      Index : Positive)
      return Concorde.People.Attributes.Attribute_Reference
   is
      pragma Unreferenced (Index, Power);
   begin
      return Concorde.People.Attributes.Skill_Reference
        (Concorde.People.Skills.Get ("administration"));
   end Attribute;

   ---------------------
   -- Attribute_Count --
   ---------------------

   function Attribute_Count
     (Power : Power_Type)
      return Natural
   is
      pragma Unreferenced (Power);
   begin
      return 1;
   end Attribute_Count;

   -------------------------------
   -- Configure_Power_Execution --
   -------------------------------

   procedure Configure_Power_Execution
     (Config : Tropos.Configuration)
   is
      Rec : Power_Execution_Record;
   begin
      if Config.Contains ("attributes") then
         Concorde.People.Attributes.Configure.Configure_Attributes
           (Container => Rec.Attributes,
            Config    => Config.Child ("attributes"));
      end if;
      for Group_Config of Config.Child ("pops") loop
         Rec.Pop_Groups.Append
           (Power_Pop_Group_Record'
              (Group  =>
                   Concorde.People.Groups.Get (Group_Config.Config_Name),
               Effect =>
                 3600.0 * Duration (Float'(Group_Config.Get ("effect")))));
      end loop;

      Configure_Work (Config.Child ("legislative_work"), Rec.Legislative_Work);
      Configure_Work (Config.Child ("daily_work"), Rec.Daily_Work);

      Power_Execution_Map.Insert (Config.Config_Name, Rec);

   end Configure_Power_Execution;

   --------------------
   -- Configure_Work --
   --------------------

   procedure Configure_Work
     (Config : Tropos.Configuration;
      Target : in out Work_Record)
   is
   begin
      if Config.Child_Count = 1 then
         Target.Base := Concorde.Calendar.Hours (Config.Value);
      else
         Target.Base := Concorde.Calendar.Hours (Config.Get ("base", 0));
         if Config.Contains ("commodity") then
            Target.Commodity.Replace_Element
              (Configure_Commodity_Work (Config.Child ("commodity")));
         end if;
      end if;
   end Configure_Work;

   ----------------
   -- Daily_Work --
   ----------------

   function Daily_Work
     (Power : Power_Type;
      World : Concorde.Worlds.World_Type)
      return Duration
   is
   begin
      case Power.Class is
         when Set_Tax_Rate =>
            return Concorde.Calendar.Hours (8);

         when Collect_Tax =>
            declare
               Work : Duration :=
                        Concorde.Calendar.Hours (8);

               procedure Add_Commodity
                 (Commodity : Concorde.Commodities.Commodity_Type);

               -------------------
               -- Add_Commodity --
               -------------------

               procedure Add_Commodity
                 (Commodity : Concorde.Commodities.Commodity_Type)
               is
                  Result : Duration := 0.0;
               begin
                  case Power.Tax_Category is
                     when Concorde.Trades.Sales =>
                        Result :=
                          Duration
                            (World.Market.Recent_Transaction_Count
                               (Commodity)) * 300.0;
                        if Result > 0.0 then
                           World.Log
                             ("tax work: sales tax on "
                              & Commodity.Name
                              & " with"
                              & Natural'Image
                                (World.Market.Recent_Transaction_Count
                                     (Commodity))
                              & " transactions yields "
                              & Concorde.Calendar.Image (Result) & " work");
                        end if;
                     when Concorde.Trades.Import =>
                        Result :=
                          Duration
                            (WL.Quantities.To_Float
                               (World.Market.Current_Imports (Commodity))
                             / 100.0);
                        if Result > 0.0 then
                           World.Log
                             ("tax work: import tariff on "
                              & Commodity.Name
                              & " with volume "
                              & WL.Quantities.Show
                                (World.Market.Current_Imports (Commodity))
                              & " yields "
                              & Concorde.Calendar.Image (Result) & " work");
                        end if;
                     when Concorde.Trades.Export =>
                        Result :=
                          Duration
                            (WL.Quantities.To_Float
                               (World.Market.Current_Exports (Commodity))
                             / 100.0);
                        if Result > 0.0 then
                           World.Log
                             ("tax work: export tariff on "
                              & Commodity.Name
                              & " with volume "
                              & WL.Quantities.Show
                                (World.Market.Current_Exports (Commodity))
                              & " yields "
                              & Concorde.Calendar.Image (Result) & " work");
                        end if;
                  end case;
                  if Result > 0.0 then
                     Result := Result + Concorde.Calendar.Hours (1);
                  end if;
                  Work := Work + Result;
               end Add_Commodity;

            begin
               Concorde.Commodities.Scan (Add_Commodity'Access);
               return Work;
            end;

      end case;
   end Daily_Work;

   ---------------
   -- Pop_Group --
   ---------------

   function Pop_Group
     (Power : Power_Type;
      Index : Positive)
      return Concorde.People.Groups.Pop_Group
   is
      pragma Unreferenced (Index);
   begin
      case Power.Class is
         when Set_Tax_Rate =>
            return Concorde.People.Groups.Get ("clerks");
         when Collect_Tax =>
            return Concorde.People.Groups.Get ("bureaucrats");
      end case;
   end Pop_Group;

   ---------------------
   -- Pop_Group_Count --
   ---------------------

   function Pop_Group_Count
     (Power : Power_Type)
      return Natural
   is
   begin
      case Power.Class is
         when Set_Tax_Rate =>
            return 1;
         when Collect_Tax =>
            return 1;
      end case;
   end Pop_Group_Count;

   ----------------------
   -- Pop_Group_Effect --
   ----------------------

   function Pop_Group_Effect
     (Power : Power_Type;
      Index : Positive)
      return Duration
   is
      pragma Unreferenced (Index, Power);
   begin
      return Concorde.Calendar.Hours (4);
   end Pop_Group_Effect;

end Concorde.Powers.Execution;
