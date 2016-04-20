package body Concorde.Ships.Flight is

   ----------------------
   -- Add_Engine_Mount --
   ----------------------

   procedure Add_Engine_Mount
     (Ship         : in out Root_Newtonian_Ship'Class;
      Mount        : Mounted_Module;
      Location     : Newton.Flight.Vector_3;
      Orientation  : Newton.Flight.Matrix_3;
      Max_Thrust   : Non_Negative_Real;
      Delta_Thrust : Unit_Real)
   is
   begin
      Ship.Engines.Append
        ((Mount, Location, Orientation, Max_Thrust, 0.0, 0.0, Delta_Thrust));
   end Add_Engine_Mount;

   ---------------------------
   -- Create_Newtonian_Ship --
   ---------------------------

   function Create_Newtonian_Ship
     (From_Ship   : Concorde.Ships.Ship_Type;
      Location    : Newton.Flight.Vector_3;
      Velocity    : Newton.Flight.Vector_3;
      Orientation : Newton.Flight.Matrix_3)
      return Newtonian_Ship
   is
      use type Concorde.Components.Component_Class;
      Result : constant Newtonian_Ship := new Root_Newtonian_Ship;
   begin
      Result.Ship := From_Ship;
      Result.Mass := From_Ship.Current_Mass;
      Result.Location := Location;
      Result.Velocity := Velocity;
      Result.Orientation := Orientation;
      return Result;
   end Create_Newtonian_Ship;

   ------------
   -- Engine --
   ------------

   overriding function Engine
     (Ship  : Root_Newtonian_Ship;
      Index : Positive)
      return Newton.Flight.Engine_Component'Class
   is
   begin
      return Ship.Engines.Element (Index);
   end Engine;

   ------------------
   -- Engine_Count --
   ------------------

   overriding function Engine_Count
     (Ship : Root_Newtonian_Ship)
      return Natural
   is
   begin
      return Ship.Engines.Last_Index;
   end Engine_Count;

   ------------------
   -- Engine_Mount --
   ------------------

   function Engine_Mount
     (Ship         : Root_Newtonian_Ship'Class;
      Engine_Index : Positive)
      return Mounted_Module
   is
   begin
      return Ship.Engines.Element (Engine_Index).Mount;
   end Engine_Mount;

   --------------
   -- Location --
   --------------

   overriding function Location
     (Ship : Root_Newtonian_Ship)
      return Newton.Flight.Vector_3
   is
   begin
      return Ship.Location;
   end Location;

   -----------------
   -- Orientation --
   -----------------

   overriding function Orientation
     (Ship : Root_Newtonian_Ship)
      return Newton.Flight.Matrix_3
   is
   begin
      return Ship.Orientation;
   end Orientation;

   ----------------------
   -- Select_Component --
   ----------------------

   procedure Select_Component
     (Ship   : in out Root_Newtonian_Ship'Class;
      Index  : Natural;
      Toggle : Boolean := False)
   is
   begin
      if Ship.Selected_Index = Index and then Toggle then
         Ship.Selected_Index := 0;
      else
         Ship.Selected_Index := Index;
      end if;
   end Select_Component;

   ------------------------------
   -- Selected_Component_Index --
   ------------------------------

   function Selected_Component_Index
     (Ship : Root_Newtonian_Ship'Class)
      return Natural
   is
   begin
      return Ship.Selected_Index;
   end Selected_Component_Index;

   ------------------------------
   -- Set_Centre_Of_Mass_Force --
   ------------------------------

   overriding procedure Set_Centre_Of_Mass_Force
     (Ship  : in out Root_Newtonian_Ship;
      Force : Newton.Flight.Vector_3)
   is
   begin
      Ship.CM_Force := Force;
   end Set_Centre_Of_Mass_Force;

   ----------------------
   -- Set_Engine_Power --
   ----------------------

   overriding procedure Set_Engine_Power
     (Ship    : in out Root_Newtonian_Ship;
      Index   : Positive;
      Setting : Unit_Real)
   is
   begin
      Ship.Engines (Index).Set_Power (Setting);
   end Set_Engine_Power;

   ------------------
   -- Set_Location --
   ------------------

   overriding procedure Set_Location
     (Ship     : in out Root_Newtonian_Ship;
      Location : Newton.Flight.Vector_3)
   is
   begin
      Ship.Location := Location;
   end Set_Location;

   ------------------
   -- Set_Location --
   ------------------

   overriding procedure Set_Location
     (Engine   : in out Ship_Engine;
      Location : Newton.Flight.Vector_3)
   is
   begin
      Engine.Location := Location;
   end Set_Location;

   ---------------------
   -- Set_Orientation --
   ---------------------

   overriding procedure Set_Orientation
     (Engine      : in out Ship_Engine;
      Orientation : Newton.Flight.Matrix_3)
   is
   begin
      Engine.Orientation := Orientation;
   end Set_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   overriding procedure Set_Orientation
     (Ship        : in out Root_Newtonian_Ship;
      Orientation : Newton.Flight.Matrix_3)
   is
   begin
      Ship.Orientation := Orientation;
   end Set_Orientation;

   ---------------
   -- Set_Power --
   ---------------

   overriding procedure Set_Power
     (Engine : in out Ship_Engine;
      Power  : Unit_Real)
   is
   begin
      Engine.Power := Power;
   end Set_Power;

   -------------------------
   -- Set_Radial_Velocity --
   -------------------------

   overriding procedure Set_Radial_Velocity
     (Ship     : in out Root_Newtonian_Ship;
      Velocity : Newton.Flight.Vector_3)
   is
   begin
      Ship.Radial_Velocity := Velocity;
   end Set_Radial_Velocity;

   ------------------
   -- Set_Throttle --
   ------------------

   overriding procedure Set_Throttle
     (Engine   : in out Ship_Engine;
      Throttle : Unit_Real)
   is
   begin
      Engine.Throttle := Throttle;
   end Set_Throttle;

   ----------------
   -- Set_Torque --
   ----------------

   overriding procedure Set_Torque
     (Ship   : in out Root_Newtonian_Ship;
      Torque : Newton.Flight.Vector_3)
   is
   begin
      Ship.Torque := Torque;
   end Set_Torque;

   ------------------
   -- Set_Velocity --
   ------------------

   overriding procedure Set_Velocity
     (Ship     : in out Root_Newtonian_Ship;
      Velocity : Newton.Flight.Vector_3)
   is
   begin
      Ship.Velocity := Velocity;
   end Set_Velocity;

   ----------
   -- Ship --
   ----------

   function Ship
     (Newton_Ship : Root_Newtonian_Ship'Class)
      return Ship_Type
   is
   begin
      return Newton_Ship.Ship;
   end Ship;

   ---------------------
   -- Throttle_Engine --
   ---------------------

   overriding procedure Throttle_Engine
     (Ship    : in out Root_Newtonian_Ship;
      Index   : Positive;
      Setting : Unit_Real)
   is
   begin
      Ship.Engines (Index).Set_Throttle (Setting);
   end Throttle_Engine;

end Concorde.Ships.Flight;
