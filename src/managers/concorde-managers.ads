private with WL.Heaps;

with Concorde.Calendar;
with Concorde.Events;
with Concorde.Objects;
with Concorde.Work;

package Concorde.Managers is

   type Root_Manager_Type is abstract tagged private;

   function Description
     (Manager : Root_Manager_Type)
      return String
      is abstract;

   procedure Deactivate (Manager : not null access Root_Manager_Type'Class);
   procedure Activate (Manager : not null access Root_Manager_Type'Class);

   procedure On_Activated
     (Manager : in out Root_Manager_Type)
   is null;

   procedure Add_Work_Item
     (Manager : in out Root_Manager_Type'Class;
      Item    : not null access constant
        Concorde.Work.Root_Work_Item'Class);

   type Manager_Type is access all Root_Manager_Type'Class;

   type Managed_Interface is limited interface;

   function Manager (Managed : Managed_Interface) return Manager_Type
                     is abstract;

   procedure Set_Manager
     (Managed : in out Managed_Interface;
      Manager : Manager_Type)
   is abstract;

private

   package Work_Item_Queue is
     new WL.Heaps
       (Key_Type     => Concorde.Work.Work_Priority,
        Element_Type => Concorde.Work.Work_Item,
        "<"          => Concorde.Work.Higher,
        "="          => Concorde.Work."=");

   type Root_Manager_Type is abstract tagged
      record
         Active       : Boolean := True;
         Object       : access constant
           Concorde.Objects.Root_Object_Type'Class;
         Time         : Concorde.Calendar.Time;
         Current_Work : Concorde.Work.Work_Item;
         Work_Queue   : Work_Item_Queue.Heap;
      end record;

   type Object_Activated_Handler is
     new Concorde.Objects.Object_Handler_Interface with
      record
         Manager : Manager_Type;
      end record;

   overriding procedure Handle
     (Handler : in out Object_Activated_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

end Concorde.Managers;
