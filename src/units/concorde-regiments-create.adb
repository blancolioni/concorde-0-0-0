package body Concorde.Regiments.Create is

   ------------------
   -- New_Regiment --
   ------------------

   function New_Regiment
     (Pop  : Concorde.People.Pops.Pop_Type;
      Unit : Concorde.Units.Unit_Type)
      return Regiment_Type
   is

      procedure Create (Regiment : in out Root_Regiment_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Regiment : in out Root_Regiment_Type'Class) is
      begin
         Regiment.Unit := Unit;
         Regiment.Pop  := Pop;
         Regiment.Size := 1000;
         Regiment.Morale := 1.0;
         Regiment.Organisation := 1.0;
         Regiment.Loyalty := 1.0;
      end Create;

   begin
      return Db.Create (Create'Access);
   end New_Regiment;

end Concorde.Regiments.Create;
