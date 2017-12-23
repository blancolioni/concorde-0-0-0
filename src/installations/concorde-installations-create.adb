with Concorde.Calendar;
with Concorde.Random;
with WL.Quantities;

with Concorde.Objects.Queues;

with Concorde.Government;

with Concorde.Managers.Installations;

package body Concorde.Installations.Create is

   ------------
   -- Create --
   ------------

   function Create
     (Location      : Concorde.Locations.Object_Location;
      Market        : access constant Concorde.Trades.Trade_Interface'Class;
      Facility      : Concorde.Facilities.Facility_Type;
      Cash          : WL.Money.Money_Type;
      Owner         : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Size          : WL.Quantities.Quantity_Type)
      return Installation_Type
   is

      use type Concorde.Calendar.Time;

      procedure Initialise
        (Installation : in out Root_Installation_Type'Class);

      ----------------
      -- Initialise --
      ----------------

      procedure Initialise
        (Installation : in out Root_Installation_Type'Class)
      is
         use WL.Quantities;
         use all type Concorde.Facilities.Facility_Class;
         Storage : Quantity_Type := Zero;
      begin
         for I in 1 .. Facility.Input_Count loop
            for J in 1 .. Facility.Input_Choice_Count (I) loop
               Storage := Storage +
                 Scale
                   (Facility.Input_Choice_Quantity (Size, I, J),
                    10.0);
            end loop;
         end loop;

         if Facility.Class = Port then
            Storage := Scale (Size, 1000.0);
         elsif Facility.Input_Count = 0 then
            Storage := Scale (Size, 10.0);
         end if;

         Installation.New_Agent
           (Location       => Location,
            Government     => Concorde.Government.Get_Government (Location),
            Market         => Market,
            Cash           => Cash,
            Stock_Capacity => Storage);
         Installation.Facility := Facility;
         Installation.Owner := Owner;
         Installation.Size := Size;
         Installation.Set_Guarantor (Owner);
      end Initialise;

   begin
      return Installation : constant Installation_Type :=
        Db.Create (Initialise'Access)
      do
         Installation.Save_Agent;
         Concorde.Managers.Installations.Create_Manager
           (Installation).Activate;
         Concorde.Objects.Queues.Next_Event
           (Installation,
            Concorde.Calendar.Clock
            + Duration (Concorde.Random.Unit_Random * 86_400.0));
      end return;
   end Create;

end Concorde.Installations.Create;
