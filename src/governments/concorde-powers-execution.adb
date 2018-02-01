with WL.Quantities;

with Concorde.Calendar;
with Concorde.Commodities;
with Concorde.People.Skills;

package body Concorde.Powers.Execution is

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
