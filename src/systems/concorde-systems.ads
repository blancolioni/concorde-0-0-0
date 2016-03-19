limited with Concorde.Empires;

with Concorde.Dates;
with Concorde.Objects;

package Concorde.Systems is

   type Root_Star_System_Type is
     new Concorde.Objects.Root_Named_Object_Type with private;

   function Index (System : Root_Star_System_Type'Class) return Positive;
   function X (System : Root_Star_System_Type'Class) return Real;
   function Y (System : Root_Star_System_Type'Class) return Real;

   function Owner
     (System : Root_Star_System_Type'Class)
      return access Concorde.Empires.Root_Empire_Type'Class;

   function Production (System : Root_Star_System_Type'Class)
                        return Non_Negative_Real;

   function Capacity (System : Root_Star_System_Type'Class)
                      return Non_Negative_Real;

   function Fleets (System : Root_Star_System_Type'Class)
                    return Natural;

   function Capital (System : Root_Star_System_Type'Class)
                     return Boolean;

   function Last_Battle (System : Root_Star_System_Type'Class)
                         return Concorde.Dates.Date_Type;

   procedure Set_Fleets
     (System     : in out Root_Star_System_Type'Class;
      New_Fleets : Natural);

   procedure Set_Owner
     (System : in out Root_Star_System_Type'Class;
      New_Owner : not null access
        Concorde.Empires.Root_Empire_Type'Class);

   procedure Set_Capital
     (System : in out Root_Star_System_Type'Class);

   procedure Set_Production
     (System : in out Root_Star_System_Type'Class;
      New_Production : Non_Negative_Real);

   procedure Set_Capacity
     (System : in out Root_Star_System_Type'Class;
      New_Capacity : Non_Negative_Real);

   type Star_System_Type is access constant Root_Star_System_Type'Class;

   procedure Attacked
     (System   : in out Root_Star_System_Type'Class;
      Attacker : Star_System_Type);

   function Last_Attacker
     (From : Root_Star_System_Type'Class)
      return Star_System_Type;

   function Distance
     (System_1, System_2 : Star_System_Type)
      return Non_Negative_Real;

   type Star_System_Access is access all Root_Star_System_Type'Class;

private

   type Root_Star_System_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Index         : Positive;
         X, Y          : Real;
         Production    : Non_Negative_Real;
         Capacity      : Non_Negative_Real;
         Progress      : Non_Negative_Real;
         Fleets        : Natural;
         Capital       : Boolean;
         Last_Battle   : Concorde.Dates.Date_Type;
         Last_Attacker : Star_System_Type;
         Owner         : access Concorde.Empires.Root_Empire_Type'Class;
      end record;

end Concorde.Systems;
