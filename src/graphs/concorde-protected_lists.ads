private with Ada.Containers.Doubly_Linked_Lists;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Concorde.Protected_Lists is

   type List is tagged limited private;

   function Contains
     (Container : in out List;
      Item      : Element_Type)
      return Boolean;

   procedure Iterate
     (Container : in out List;
      Process   : not null access
        procedure (Element : Element_Type));

   procedure Add_If_Missing
     (Container : in out List;
      Item      : in     Element_Type;
      Added     :    out Boolean);

   procedure Delete_If_Present
     (Container : in out List;
      Item      : in     Element_Type;
      Deleted   :    out Boolean);

   procedure Delete_Matching
     (Container : in out List;
      Match     : not null access
        function (Item : Element_Type) return Boolean);

private

   package List_Of_Elements is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type, "=");

   protected type List_Lock is
      entry Shared;
      entry Exclusive;
      procedure Unlock;
   private
      Shared_Lock_Count : Natural := 0;
      Exclusive_Locked  : Boolean := False;
   end List_Lock;

   type List is tagged limited
      record
         Lock       : List_Lock;
         Inner_List : List_Of_Elements.List;
      end record;

end Concorde.Protected_Lists;
