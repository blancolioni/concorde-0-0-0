with Xi.Float_Images;

with Concorde.Money;

with Concorde.Roman_Images;

package body Concorde.Ships.Vessels.Create is

   -------------------------
   -- Create_Start_Vessel --
   -------------------------

   function Create_Start_Vessel
     (Owner       : Concorde.Factions.Faction_Type;
      Community   : not null access constant
        Concorde.People.Communities.Root_Community_Type'Class;
      Name        : String;
      Design_Name : String;
      Suffix      : Natural := 0)
      return Ship_Type
   is

      procedure Create
        (Vessel : in out Root_Vessel_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create
        (Vessel : in out Root_Vessel_Type'Class)
      is

         Design : constant Concorde.Ships.Designs.Design_Type :=
                    (if Concorde.Ships.Designs.Exists (Design_Name)
                     then Concorde.Ships.Designs.Get (Design_Name)
                     else raise Constraint_Error with
                       "no such ship design: " & Design_Name);
      begin
         Vessel.New_Agent
           (Location       =>
              Concorde.Locations.Geosynchronous_Orbit
                (Community.World),
            Government     => Community.Government,
            Market         => Community.Market,
            Cash           => Concorde.Money.To_Money (100_000.0),
            Stock_Capacity => Design.Cargo_Capacity);

         Vessel.Design := Design;

         if Name = "" then
            if Suffix = 0 then
               Vessel.Set_Name (Owner.Name);
            else
               Vessel.Set_Name
                 (Owner.Name & " "
                  & Concorde.Roman_Images.Roman_Image (Suffix));
            end if;
         else
            if Suffix = 0 then
               Vessel.Set_Name (Name);
            else
               Vessel.Set_Name
                 (Name & " "
                  & Concorde.Roman_Images.Roman_Image (Suffix));
            end if;
         end if;
         Vessel.Owner := Owner;
         Vessel.Set_Guarantor (Owner);
         Vessel.Is_Alive := True;

         for I in 1 .. Vessel.Design.Get_Module_Count loop
            Vessel.Modules.Append
              (Ship_Module_Record'
                 (Stock     => <>,
                  Heat      => 0.0,
                  Condition => 1.0));
         end loop;

         declare
            Id : constant String :=
                   "00000" & Memor.To_String (Vessel.Reference);
         begin
            Vessel.Identity := "1" & Id (Id'Last - 4 .. Id'Last);
         end;

         Vessel.Log
           (Community.World.Name & ": new ship: " & Vessel.Name);

         declare
            Fuel : Concorde.Commodities.Root_Stock_Type;

            procedure Log_Fuel (Item : Concorde.Commodities.Commodity_Type);

            --------------
            -- Log_Fuel --
            --------------

            procedure Log_Fuel (Item : Concorde.Commodities.Commodity_Type) is
            begin
               Vessel.Log
                 ("loading "
                  & Xi.Float_Images.Image
                    (Non_Negative_Real
                         (Concorde.Quantities.To_Real
                              (Fuel.Get_Quantity (Item)))
                     * Item.Unit_Mass / 1000.0)
                  & "t " & Item.Name);
            end Log_Fuel;

         begin
            Fuel.Create_Stock
              (Concorde.Quantities.To_Quantity (999_999.0), True);
            Design.Get_Fuel_Requirements (Fuel);
            Fuel.Scan_Stock (Log_Fuel'Access);
            Vessel.Add (Fuel);
         end;

         Owner.Update.New_Ship;

         declare
            Full_Cargo_Mass : constant Non_Negative_Real :=
                                Non_Negative_Real
                                  (Concorde.Quantities.To_Real
                                     (Vessel.Cargo_Capacity))
                                * 1000.0;
         begin
            Vessel.Log
              ("mass: "
               & Xi.Float_Images.Image (Vessel.Current_Mass / 1_000.0)
               & "t"
               & "; max thrust: "
               & Xi.Float_Images.Image (Vessel.Maximum_Thrust / 1_000.0)
               & "kN"
               & "; hold: "
               & Concorde.Quantities.Image (Vessel.Cargo_Capacity)
               & "m" & Character'Val (16#C2#) & Character'Val (16#B3#)
               & "; acceleration (empty): "
               & Xi.Float_Images.Image
                 (Vessel.Maximum_Thrust / Vessel.Current_Mass / 9.81) & "g"
               & "; acceleration (full): "
               & Xi.Float_Images.Image
                 (Vessel.Maximum_Thrust
                  / (Vessel.Current_Mass + Full_Cargo_Mass) / 9.81) & "g");
         end;

      end Create;

      Vessel : constant Vessel_Type := Db.Create (Create'Access);
   begin
      Community.Update.Add_Ship (Vessel);
      Community.World.System.Update.Add_Ship (Vessel);
      return Ship_Type (Vessel);
   end Create_Start_Vessel;

end Concorde.Ships.Vessels.Create;
