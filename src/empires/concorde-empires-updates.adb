with Concorde.AI.Default;
with Concorde.Galaxy;

with Concorde.Empires.Logging;

package body Concorde.Empires.Updates is

   type Move_Order is
      record
         To       : Concorde.Systems.Star_System_Type;
         Pressure : Non_Negative_Real;
      end record;

   package List_Of_Move_Orders is
     new Ada.Containers.Doubly_Linked_Lists (Move_Order);

   type System_Orders is
      record
         Moves               : List_Of_Move_Orders.List;
         Total_Move_Pressure : Non_Negative_Real := 0.0;
         Start_Ships         : Natural := 0;
      end record;

   type Array_Of_System_Orders is
     array (Positive range <>) of System_Orders;

   procedure Add_Move_Orders
     (Empire : Empire_Type;
      Target : Concorde.Systems.Star_System_Type;
      Orders : in out Array_Of_System_Orders)
     with Unreferenced;

   procedure Update_Empire
     (Empire : Empire_Type;
      Owned  : Concorde.Galaxy.Array_Of_Star_Systems);

   ---------------------
   -- Add_Move_Orders --
   ---------------------

   procedure Add_Move_Orders
     (Empire : Empire_Type;
      Target : Concorde.Systems.Star_System_Type;
      Orders : in out Array_Of_System_Orders)
   is
      use Concorde.Galaxy;

      type Queue_Element is
         record
            System   : Concorde.Systems.Star_System_Type;
            Pressure : Non_Negative_Real;
         end record;

      package Queue_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Queue_Element);

      Queue : Queue_Lists.List;

      Seen : array (Orders'Range) of Boolean :=
               (others => False);

   begin
      Logging.Log (Empire, "adding move orders for target " & Target.Name);

      Queue.Append ((Target, 1.0));

      while not Queue.Is_Empty loop
         declare
            Front : constant Queue_Element := Queue.First_Element;
            Ns    : constant Array_Of_Star_Systems :=
                      Neighbours (Front.System);
         begin
            Queue.Delete_First;

            if not Seen (Front.System.Index) then
               Seen (Front.System.Index) := True;

               Logging.Log (Empire, "   checking " & Front.System.Name);

               for N of Ns loop
                  if not Seen (N.Index)
                    and then N.Owner = Empire
                  then
                     Logging.Log
                       (Empire,
                        "    moving from "
                        & N.Name & " to "
                        & Front.System.Name
                        & " with pressure"
                        & Natural'Image
                          (Natural (Front.Pressure * 100.0))
                        & "%");
                     Orders (N.Index).Moves.Append
                       ((Front.System, Front.Pressure));
                     Orders (N.Index).Total_Move_Pressure :=
                       Orders (N.Index).Total_Move_Pressure + Front.Pressure;
                     Queue.Append ((N, Front.Pressure / 2.0));
                  end if;
               end loop;
            end if;
         end;
      end loop;
   end Add_Move_Orders;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      for Empire of Vector loop
         if Empire.AI = null then
            Empire.AI := Concorde.AI.Default.Default_AI;
         end if;
         Empire.AI.Start (Empire);
      end loop;
   end Start;

   -------------------
   -- Update_Empire --
   -------------------

   procedure Update_Empire
     (Empire : Empire_Type;
      Owned  : Concorde.Galaxy.Array_Of_Star_Systems)
   is
      pragma Unreferenced (Owned);
   begin

      Empire.AI.Update_Focus;

   end Update_Empire;

   --------------------
   -- Update_Empires --
   --------------------

   procedure Update_Empires is
   begin
      for Empire of Vector loop
         Empire.Max_Ships := 0;
      end loop;

      for I in 1 .. Concorde.Galaxy.System_Count loop
         declare
            System : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Galaxy.Get_System (I);
            Empire : constant Empire_Type := System.Owner;
         begin
            if Empire /= null then
               Empire.Max_Ships := Empire.Max_Ships
                 + Natural (System.Capacity);
            end if;
         end;
      end loop;

      for Empire of Vector loop
         declare
            function Owned
              (System : Concorde.Systems.Star_System_Type)
               return Boolean
            is (System.Owner = Empire);
         begin
            Update_Empire
              (Empire, Concorde.Galaxy.Get_Systems (Owned'Access));
         end;
      end loop;
   end Update_Empires;

end Concorde.Empires.Updates;
