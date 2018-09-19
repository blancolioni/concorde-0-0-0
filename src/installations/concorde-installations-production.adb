with Concorde.Real_Images;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Commodities;

package body Concorde.Installations.Production is

   procedure Execute_Production
     (Installation : in out Root_Installation_Type'Class);

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production is
   begin
      Db.Iterate (Execute_Production'Access);
   end Execute_Production;

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production
     (Installation : in out Root_Installation_Type'Class)
   is
      use Concorde.Commodities;
      Facility : constant Concorde.Facilities.Facility_Type :=
                   Installation.Facility;
      Production_Cost : Concorde.Money.Money_Type :=
                          Money.Zero;
   begin

      if Facility.Has_Output
        and then Facility.Output.Is_Set (Virtual)
      then
         Installation.Set_Quantity
           (Facility.Output, Quantities.Zero, Money.Zero);
      end if;

      declare
         use Concorde.Money;
         use Concorde.Quantities;
         Raw_Capacity       : constant Quantity_Type :=
                                Facility.Capacity_Quantity;
         Throughput         : Unit_Real := 1.0;
         Effective_Capacity : Quantity_Type := Raw_Capacity;
      begin
         for I in 1 .. Facility.Worker_Count loop
            declare
               Commodity : constant Commodity_Type :=
                             Facility.Worker_Skill (I).Commodity;
               Available : constant Quantity_Type :=
                             Installation.Get_Quantity (Commodity);
               Required  : constant Quantity_Type :=
                             Facility.Worker_Quantity (I);
            begin
               Production_Cost :=
                 Production_Cost + Installation.Get_Value (Commodity);

               if Available < Required then
                  Throughput :=
                    Unit_Real'Min
                      (To_Real (Available) / To_Real (Required),
                       Throughput);
                  Effective_Capacity :=
                    Min (Effective_Capacity,
                         Scale_Down (Raw_Capacity, Available, Required));
                  Installation.Log_Production
                    (Image (Available) & " of "
                     & Image (Required)
                     & " " & Commodity.Name
                     & ": throughput = "
                     & Concorde.Real_Images.Approximate_Image
                       (Throughput * 100.0)
                     & "%");
               end if;
--                 Installation.Remove_Quantity (Commodity, Available);
--                 Conflict.Commodities.Remove
--                   (Installation.Reference, Commodity, Available);
--                 Production_Cost := Production_Cost + Cost;
            end;
         end loop;

         if Facility.Has_Output then

            for Input_Index in 1 .. Facility.Input_Count loop
               declare
                  Commodity : constant Commodity_Type :=
                                Facility.Input_Commodity (Input_Index);
                  Required : constant Quantity_Type :=
                                Facility.Input_Quantity (Input_Index)
                                * Raw_Capacity;
                  Available : constant Quantity_Type :=
                                Installation.Get_Quantity (Commodity);
               begin
                  if Available < Required then
                     Throughput :=
                       Unit_Real'Min
                         (To_Real (Available) / To_Real (Required),
                          Throughput);
                     Effective_Capacity :=
                       Min (Effective_Capacity,
                            Scale_Down (Raw_Capacity, Available, Required));
                  end if;
               end;
            end loop;

         end if;

         if Throughput < 1.0 then
            Installation.Log_Production
              ("throughput limited to "
               & Concorde.Real_Images.Approximate_Image
                 (Throughput * 100.0)
               & "%; effective capacity " & Image (Effective_Capacity));
         end if;

--              Conflict.Logging.Log
--                (Installation,
--                 " produces "
--                 & Conflict.Numbers.Image (Effective_Capacity)
--                 & " "
--                 & Conflict.Commodities.Name (Installation.Production)
--                 & " for "
--                 & Conflict.Numbers.Image (Production_Cost));

         if Effective_Capacity > Zero then

            for Input_Index in 1 .. Facility.Input_Count loop
               declare
                  Commodity : constant Commodity_Type :=
                                Facility.Input_Commodity (Input_Index);
                  Required  : constant Quantity_Type :=
                                Facility.Input_Quantity (Input_Index)
                                * Effective_Capacity;
                  Price_Per : constant Price_Type :=
                                Installation.Get_Average_Price (Commodity);
                  Cost      : constant Money_Type :=
                                Money.Total (Price_Per, Required);
               begin
                  Production_Cost := Production_Cost + Cost;
                  Installation.Remove_Quantity (Commodity, Required);
               end;
            end loop;

            if Facility.Is_Farm then
               Installation.Log_Production
                 ("produces "
                  & Image (Effective_Capacity)
                  & " "
                  & Facility.Output.Name
                  & " for "
                  & Image (Production_Cost));

               Installation.Add_Quantity
                 (Facility.Output,
                  Effective_Capacity,
                  Production_Cost);

            elsif Facility.Has_Output then
               Installation.Log_Production
                 ("produces "
                  & Image (Effective_Capacity)
                  & " "
                  & Facility.Output.Name
                  & " for "
                  & Image (Production_Cost));

               Installation.Add_Quantity
                 (Facility.Output,
                  Effective_Capacity,
                  Production_Cost);

            elsif Facility.Is_Resource_Generator then

               declare
                  Resource : Concorde.Commodities.Commodity_Type;
                  Concentration : Unit_Real;
                  Accessibility : Unit_Real;
                  Factor        : Non_Negative_Real;
               begin
                  Concorde.Worlds.Get_Sector_Resource
                    (Installation.Current_Location,
                     Resource, Concentration, Accessibility);

                  Factor :=
                    (Accessibility + Concentration);
                  Effective_Capacity :=
                    Scale (Effective_Capacity, Factor);

                  Installation.Log_Production
                    ("generates "
                     & Image (Effective_Capacity)
                     & " "
                     & Resource.Name
                     & " for "
                     & Image (Production_Cost));

                  if Effective_Capacity + Installation.Total_Quantity
                    > Installation.Maximum_Quantity
                  then
                     declare
                        Lose : constant Quantity_Type :=
                                 Effective_Capacity
                                   + Installation.Total_Quantity
                                 - Installation.Maximum_Quantity;
                     begin
                        Installation.Log_Production
                          ("loses " & Image (Lose) & " due to full storage");
                        Effective_Capacity := Effective_Capacity - Lose;
                     end;
                  end if;

                  Installation.Add_Quantity
                    (Resource,
                     Effective_Capacity, Production_Cost);
               end;
            end if;
         end if;

--              Conflict.Commodities.Add
--                (Installation.Reference, Installation.Production,
--                 Effective_Capacity, Production_Cost);
--           end if;
      end;

   end Execute_Production;

end Concorde.Installations.Production;
