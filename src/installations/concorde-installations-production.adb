--  with Concorde.Money;
with Concorde.Quantities;

with Concorde.Commodities;

package body Concorde.Installations.Production is

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production
     (System : Concorde.Systems.Root_Star_System_Type'Class;
      Installation : in out
        Concorde.Installations.Root_Installation_Type'Class)
   is
      use Concorde.Commodities;
      Facility : constant Concorde.Facilities.Facility_Type :=
                   Installation.Facility;
   begin

      Installation.Log_Production
        ("Executing production");

      if Facility.Has_Output
        and then Facility.Output.Is_Set (Virtual)
      then
         Installation.Set_Quantity (Facility.Output, Quantities.Zero);
      end if;

      declare
--           use Concorde.Money;
         use Concorde.Quantities;
         Raw_Capacity       : constant Quantity :=
                                Facility.Capacity_Quantity;
         Throughput         : Unit_Real := 1.0;
         Effective_Capacity : Quantity := Raw_Capacity;
--           Production_Cost    : Money_Type := Zero;
      begin
         for I in 1 .. Facility.Worker_Count loop
            declare
               Commodity : constant Commodity_Type :=
                             Facility.Worker_Skill (I).Commodity;
               Available : constant Quantity :=
                             Installation.Get_Quantity (Commodity);
               Required  : constant Quantity :=
                             Facility.Worker_Quantity (I);
--                 Cost      : constant Money_Type :=
--                               Conflict.Commodities.Value
--                                 (Installation.Reference, Commodity);
            begin
               if Available < Required then
                  Throughput :=
                    Unit_Real'Min
                      (To_Real (Available) / To_Real (Required),
                       Throughput);
                  Installation.Log_Production
                    (Image (Available) & " of "
                     & Image (Required)
                     & " " & Commodity.Name
                     & ": throughput = "
                     & Lui.Approximate_Image (Throughput * 100.0)
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
                  Required : constant Quantity :=
                                Facility.Input_Quantity (Input_Index);
                  Available : constant Quantity :=
                                Installation.Get_Quantity (Commodity);
               begin
                  if Available < Required then
                     Throughput :=
                       Unit_Real'Min
                         (To_Real (Available) / To_Real (Required),
                          Throughput);
                  end if;
               end;
            end loop;

         end if;

         if Throughput < 1.0 then
            Installation.Log_Production
              ("throughput limited to "
               & Lui.Approximate_Image (Throughput * 100.0)
               & "%");
            Effective_Capacity := Scale (Raw_Capacity, Throughput);

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
                  Required  : constant Quantity :=
                                Facility.Input_Quantity (Input_Index)
                                * Effective_Capacity;
               begin
                  Installation.Remove_Quantity (Commodity, Required);
               end;
            end loop;

            if Facility.Has_Output then
               Installation.Log_Production
                 ("produces "
                  & Image (Effective_Capacity)
                  & " "
                  & Facility.Output.Name);

               Installation.Add_Quantity
                 (Facility.Output,
                  Effective_Capacity);

            elsif Facility.Is_Resource_Generator then

               Effective_Capacity :=
                 Scale (Effective_Capacity, System.Resource_Accessibility);
               Effective_Capacity :=
                 Scale (Effective_Capacity, System.Resource_Concentration);

               Installation.Log_Production
                 ("generates "
                  & Image (Effective_Capacity)
                  & " "
                  & System.Resource.Name);

               Installation.Add_Quantity
                 (System.Resource,
                  Effective_Capacity);

            end if;
         end if;

--              Conflict.Commodities.Add
--                (Installation.Reference, Installation.Production,
--                 Effective_Capacity, Production_Cost);
--           end if;
      end;

   end Execute_Production;

end Concorde.Installations.Production;
