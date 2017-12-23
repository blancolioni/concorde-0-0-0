private with Memor;
private with Memor.Database;

with WL.Money;
with WL.Quantities;

with Concorde.Commodities;
with Concorde.Objects;

with Concorde.People.Groups;

package Concorde.Facilities is

   type Facility_Class is
     (Colony_Hub, Port, Consulate, Trade_Centre, Corporate_HQ, Orbital_Dock,
      Factory, Resource_Generator, Farm, Service_Facility,
      Artisan);

   type Facility_Flag is
     (Medical,
      Fitness,
      Entertainment,
      Education,
      Virtual);

   type Process_Effect is (Input, Throughput, Output);

   type Process_Efficiency is array (Process_Effect) of Non_Negative_Real;

   type Process_Activity is (Work, Manage);

   type Root_Facility_Type is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   function Class
     (Facility : Root_Facility_Type'Class)
      return Facility_Class;

   function Resource_Name
     (Facility : Root_Facility_Type'Class)
      return String;

   function Is_Set
     (Facility : Root_Facility_Type'Class;
      Flag     : Facility_Flag)
      return Boolean;

   function Workforce
     (Facility : Root_Facility_Type'Class)
      return WL.Quantities.Quantity_Type;

   function Base_Service_Charge
     (Facility : Root_Facility_Type'Class)
      return WL.Money.Price_Type;

   function Input_Count
     (Facility : Root_Facility_Type'Class)
      return Natural;

   function Simple_Input
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Boolean;

   function Choice_Input
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Boolean;

   function Input_Commodity
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Concorde.Commodities.Commodity_Type
     with Pre => Index <= Facility.Input_Count
     and then Facility.Simple_Input (Index);

   function Input_Quantity
     (Facility        : Root_Facility_Type'Class;
      Production_Size : WL.Quantities.Quantity_Type;
      Index           : Positive)
      return WL.Quantities.Quantity_Type
     with Pre => Index <= Facility.Input_Count
     and then Facility.Simple_Input (Index);

   function Input_Choice_Count
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Positive
     with Pre => Index <= Facility.Input_Count,
     Post => Facility.Choice_Input (Index)
     or else Input_Choice_Count'Result = 1;

   function Input_Choice_Commodity
     (Facility     : Root_Facility_Type'Class;
      Index        : Positive;
      Choice_Index : Positive)
      return Concorde.Commodities.Commodity_Type
     with Pre => Index <= Facility.Input_Count
     and then (Facility.Choice_Input (Index) or else Choice_Index = 1)
     and then Choice_Index <= Facility.Input_Choice_Count (Index);

   function Input_Choice_Quantity
     (Facility        : Root_Facility_Type'Class;
      Production_Size : WL.Quantities.Quantity_Type;
      Index           : Positive;
      Choice_Index    : Positive)
      return WL.Quantities.Quantity_Type
     with Pre => Index <= Facility.Input_Count
     and then (Facility.Choice_Input (Index) or else Choice_Index = 1)
     and then Choice_Index <= Facility.Input_Choice_Count (Index);

   function Owner_Pop
     (Facility : Root_Facility_Type'Class)
      return Concorde.People.Groups.Pop_Group;

   function Owner_Effect
     (Facility : Root_Facility_Type'Class)
      return Process_Effect;

   function Owner_Effect_Factor
     (Facility : Root_Facility_Type'Class)
      return Real;

   function Owner_Activity
     (Facility : Root_Facility_Type'Class)
      return Process_Activity;

   function Pop_Group_Count
     (Facility : Root_Facility_Type'Class)
      return Natural;

   function Pop_Group
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Concorde.People.Groups.Pop_Group
     with Pre => Index <= Facility.Pop_Group_Count;

   function Pop_Group_Activity
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Process_Activity
     with Pre => Index <= Facility.Pop_Group_Count;

   function Pop_Group_Quantity
     (Facility        : Root_Facility_Type'Class;
      Production_Size : WL.Quantities.Quantity_Type;
      Index           : Positive)
      return WL.Quantities.Quantity_Type
     with Pre => Index <= Facility.Pop_Group_Count;

   function Pop_Group_Effect
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Process_Effect
     with Pre => Index <= Facility.Pop_Group_Count;

   function Is_Resource_Generator
     (Facility : Root_Facility_Type'Class)
      return Boolean
   is (Facility.Class = Resource_Generator);

   function Is_Farm
     (Facility : Root_Facility_Type'Class)
      return Boolean
   is (Facility.Class = Farm);

   function Has_Output
     (Facility : Root_Facility_Type'Class)
      return Boolean;

   function Output
     (Facility : Root_Facility_Type'Class)
      return Concorde.Commodities.Commodity_Type
     with Pre => Facility.Has_Output;

   function Base_Output_Quantity
     (Facility : Root_Facility_Type'Class;
      Size     : WL.Quantities.Quantity_Type;
      Factor   : Non_Negative_Real)
      return WL.Quantities.Quantity_Type;

   function Can_Produce
     (Facility  : Root_Facility_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Boolean;

   type Facility_Type is access constant Root_Facility_Type'Class;

   function Get (Name : String) return Facility_Type;

   type Array_Of_Facilities is
     array (Positive range <>) of Facility_Type;

   function Get_By_Production
     (Output : Concorde.Commodities.Commodity_Type)
      return Array_Of_Facilities;

   function Get_By_Production
     (Output : Concorde.Commodities.Root_Commodity_Type'Class)
      return Array_Of_Facilities;

   function Get_By_Class
     (Class : Facility_Class)
      return Array_Of_Facilities;

   function All_Facilities return Array_Of_Facilities;

   function Colony_Hub return Facility_Type;

   function Resource_Generator
     (Resource : Concorde.Commodities.Commodity_Type)
      return Facility_Type
     with Pre => Concorde.Commodities."="
       (Resource.Class, Concorde.Commodities.Resource);

private

   type Array_Of_Flags is array (Facility_Flag) of Boolean;

   type Input_Record_Class is (Simple, Choice);

   type Input_Record (Class : Input_Record_Class);

   type Input_Record_Access is access Input_Record;

   type Array_Of_Inputs is array (Positive range <>) of Input_Record_Access;

   type Input_Record (Class : Input_Record_Class) is
      record
         case Class is
            when Simple =>
               Commodity : Concorde.Commodities.Commodity_Type;
               Quantity  : Float;
            when Choice =>
               Choices   : access Array_Of_Inputs;
         end case;
      end record;

   type Worker_Record is
      record
         Group      : Concorde.People.Groups.Pop_Group;
         Activity   : Process_Activity;
         Effect     : Process_Effect;
         Proportion : Unit_Real;
      end record;

   type Array_Of_Workers is
     array (Positive range <>) of Worker_Record;

   type Root_Facility_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Tag                 : access String;
         Resource_Name       : access String;
         Class               : Facility_Class;
         Flags               : Array_Of_Flags;
         Power               : WL.Quantities.Quantity_Type;
         Workforce           : WL.Quantities.Quantity_Type;
         Turnaround          : Duration;
         Commodity_Flags     : Concorde.Commodities.Array_Of_Flags;
         Owner_Group         : Concorde.People.Groups.Pop_Group;
         Owner_Activity      : Process_Activity;
         Owner_Effect        : Process_Effect;
         Owner_Factor        : Real;
         Inputs              : access Array_Of_Inputs;
         Workers             : access Array_Of_Workers;
         Output              : Concorde.Commodities.Commodity_Type;
         Output_Value        : Non_Negative_Real;
         Base_Service_Charge : WL.Money.Price_Type;
      end record;

   overriding function Object_Database
     (Item : Root_Facility_Type)
      return Memor.Memor_Database;

   overriding function Identifier
     (Item : Root_Facility_Type)
      return String
   is (Item.Tag.all);

   package Db is
     new Memor.Database
       ("facility", Root_Facility_Type, Facility_Type);

   function Resource_Name
     (Facility : Root_Facility_Type'Class)
      return String
   is (if Facility.Resource_Name = null
       then Facility.Identifier
       else Facility.Resource_Name.all);

   function Workforce
     (Facility : Root_Facility_Type'Class)
      return WL.Quantities.Quantity_Type
   is (Facility.Workforce);

   function Simple_Input
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Boolean
   is (Facility.Inputs (Index).Class = Simple);

   function Choice_Input
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Boolean
   is (Facility.Inputs (Index).Class = Choice);

   function Input_Quantity
     (Facility : Root_Facility_Type'Class;
      Production_Size : WL.Quantities.Quantity_Type;
      Index    : Positive)
      return WL.Quantities.Quantity_Type
   is (WL.Quantities.Scale
       (Production_Size, Facility.Inputs (Index).Quantity));

   function Input_Choice_Count
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Positive
   is (if Simple_Input (Facility, Index) then 1
       else Facility.Inputs (Index).Choices'Length);

   function Input_Choice_Commodity
     (Facility     : Root_Facility_Type'Class;
      Index        : Positive;
      Choice_Index : Positive)
      return Concorde.Commodities.Commodity_Type
   is (if Simple_Input (Facility, Index)
       then Input_Commodity (Facility, Index)
       else Facility.Inputs (Index).Choices (Choice_Index).Commodity);

   function Input_Choice_Quantity
     (Facility        : Root_Facility_Type'Class;
      Production_Size : WL.Quantities.Quantity_Type;
      Index           : Positive;
      Choice_Index    : Positive)
      return WL.Quantities.Quantity_Type
   is (if Simple_Input (Facility, Index)
       then Input_Quantity (Facility, Production_Size, Index)
       else WL.Quantities.Scale
       (Production_Size,
        Facility.Inputs (Index).Choices (Choice_Index).Quantity));

   function Base_Output_Quantity
     (Facility : Root_Facility_Type'Class;
      Size     : WL.Quantities.Quantity_Type;
      Factor   : Non_Negative_Real)
      return WL.Quantities.Quantity_Type
   is (WL.Quantities.Scale
       (Size,
        Float (Facility.Output_Value * Factor)));

   function Owner_Pop
     (Facility : Root_Facility_Type'Class)
      return Concorde.People.Groups.Pop_Group
   is (Facility.Owner_Group);

   function Owner_Effect
     (Facility : Root_Facility_Type'Class)
      return Process_Effect
   is (Facility.Owner_Effect);

   function Owner_Effect_Factor
     (Facility : Root_Facility_Type'Class)
      return Real
   is (Facility.Owner_Factor);

   function Owner_Activity
     (Facility : Root_Facility_Type'Class)
      return Process_Activity
   is (Facility.Owner_Activity);

   function Pop_Group_Count
     (Facility : Root_Facility_Type'Class)
      return Natural
   is (Facility.Workers'Length);

   function Pop_Group
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Concorde.People.Groups.Pop_Group
   is (Facility.Workers (Index).Group);

   function Pop_Group_Activity
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Process_Activity
   is (Facility.Workers (Index).Activity);

   function Pop_Group_Quantity
     (Facility        : Root_Facility_Type'Class;
      Production_Size : WL.Quantities.Quantity_Type;
      Index           : Positive)
      return WL.Quantities.Quantity_Type
   is (WL.Quantities.Scale
       (Production_Size,
        Float (Facility.Workers (Index).Proportion)));

   function Pop_Group_Effect
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Process_Effect
   is (Facility.Workers (Index).Effect);

end Concorde.Facilities;
