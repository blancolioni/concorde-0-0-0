with Concorde.Calendar;
with Concorde.Objects.Queues;
with Concorde.Quantities;
with Concorde.Random;
with Concorde.Real_Images;

with Concorde.Managers.Industries;

package body Concorde.Industries.Create is

   ------------------
   -- New_Industry --
   ------------------

   function New_Industry
     (Market          : not null access constant
        Concorde.Trades.Trade_Interface'Class;
      Government      : not null access constant
        Concorde.Government.Root_Government_Type'Class;
      Owner           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Production      : String;
      Size            : Non_Negative_Real;
      Cash            : Concorde.Money.Money_Type)
      return Industry_Type
   is

      procedure Create (Industry : in out Root_Industry_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Industry : in out Root_Industry_Type'Class) is
         use Concorde.Quantities;
      begin
         if Production = "" then
            raise Constraint_Error with
              "empty production name";
         end if;

         if not Concorde.Production.Exists (Production) then
            raise Constraint_Error with
              "no such production: " & Production;
         end if;

         Industry.New_Agent
           (Location       => Concorde.Locations.Nowhere,
            Government     => Government,
            Market         => Market,
            Cash           => Cash,
            Stock_Capacity => To_Quantity (Size * 10.0));
         Industry.Owner := Owner;
         Industry.Production :=
           Concorde.Production.Get (Production);
         Industry.Size := Size;

      end Create;

      use type Concorde.Calendar.Time;
   begin
      return Industry : constant Industry_Type := Db.Create (Create'Access) do
         Industry.Save_Agent;
         Concorde.Managers.Industries.Create_Manager (Industry).Activate;
         Concorde.Objects.Queues.Next_Event
           (Industry,
            Concorde.Calendar.Clock
            + Duration (Concorde.Random.Unit_Random * 86_400.0));
         Industry.Log
           (Industry.Production.Name & " "
            & Concorde.Real_Images.Approximate_Image (Industry.Size));
      end return;
   end New_Industry;

end Concorde.Industries.Create;
