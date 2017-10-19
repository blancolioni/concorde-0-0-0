private with Ada.Containers.Doubly_Linked_Lists;

with WL.Quantities;

package Concorde.Owners is

   type Owner_Interface is limited interface;

   type Owned_Interface is limited interface;

   function Owner_Count (Owned : Owned_Interface'Class) return Natural;

   function Shares
     (Owned : Owned_Interface;
      Owner : not null access constant Owner_Interface'Class)
      return WL.Quantities.Quantity
      is abstract;

   procedure Move_Shares
     (Owned : in out Owned_Interface;
      From  : access constant Owner_Interface'Class;
      To    : access constant Owner_Interface'Class;
      Count : WL.Quantities.Quantity)
   is abstract;

   function Total_Shares
     (Owned : Owned_Interface)
      return WL.Quantities.Quantity
      is abstract;

   type Owned_Shares_Type is
     new Owned_Interface with private;

   overriding function Shares
     (Owned : Owned_Shares_Type;
      Owner : not null access constant Owner_Interface'Class)
      return WL.Quantities.Quantity;

   overriding function Total_Shares
     (Owned : Owned_Shares_Type)
      return WL.Quantities.Quantity;

   overriding procedure Move_Shares
     (Owned : in out Owned_Shares_Type;
      From  : access constant Owner_Interface'Class;
      To    : access constant Owner_Interface'Class;
      Count : WL.Quantities.Quantity);

private

   type Owner_Access is access all Owner_Interface'Class;

   type Share_Record is
      record
         Owner  : Owner_Access;
         Shares : WL.Quantities.Quantity;
      end record;

   package List_Of_Share_Records is
     new Ada.Containers.Doubly_Linked_Lists (Share_Record);

   type Owned_Shares_Type is
     new Owned_Interface with
      record
         Share_List : List_Of_Share_Records.List;
      end record;

end Concorde.Owners;
