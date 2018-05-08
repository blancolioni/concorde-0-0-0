with WL.Money;

with Concorde.Factions.Logging;
with Concorde.Roman_Images;

package body Concorde.Ships.Vessels.Create is

   -------------------------
   -- Create_Start_Vessel --
   -------------------------

   function Create_Start_Vessel
     (Owner       : Concorde.Factions.Faction_Type;
      Name        : String;
      World       : Concorde.Worlds.World_Type;
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
         use WL.Quantities;

         Design : constant Concorde.Ships.Designs.Design_Type :=
                    Concorde.Ships.Designs.Get (Design_Name);
      begin
         Vessel.New_Agent
           (Location       =>
              Concorde.Locations.Geosynchronous_Orbit (World),
            Government     => Owner.Capital.Government,
            Market         => World.Market,
            Cash           => WL.Money.To_Money (1_000_000.0),
            Stock_Capacity => Design.Cargo_Capacity);

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

         declare
            Id : constant String :=
                   "00000" & Memor.To_String (Vessel.Reference);
         begin
            Vessel.Identity := "1" & Id (Id'Last - 4 .. Id'Last);
         end;

         Owner.Update.New_Ship;

         Concorde.Factions.Logging.Log
           (Owner, World.Name & ": new ship: " & Vessel.Name);
      end Create;

      Vessel : constant Vessel_Type := Db.Create (Create'Access);
   begin
      World.System.Update.Add_Ship (Vessel);
      return Ship_Type (Vessel);
   end Create_Start_Vessel;

end Concorde.Ships.Vessels.Create;
