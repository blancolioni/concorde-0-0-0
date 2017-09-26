with Concorde.Galaxy;

with Concorde.Empires.History;
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
     (Empire : in out Root_Empire_Type'Class);

   procedure Update_System_Flags
     (System : in out Concorde.Systems.Root_Star_System_Type'Class);

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
      null;
   end Start;

   -------------------
   -- Update_Empire --
   -------------------

   procedure Update_Empire
     (Empire : in out Root_Empire_Type'Class)
   is
      pragma Unreferenced (Empire);
   begin
      null;
   end Update_Empire;

   --------------------
   -- Update_Empires --
   --------------------

   procedure Update_Empires is

      procedure On_Update_Start (Empire : in out Root_Empire_Type'Class);

      procedure Update_System_Owner
        (System : in out Concorde.Systems.Root_Star_System_Type'Class);

      ---------------------
      -- On_Update_Start --
      ---------------------

      procedure On_Update_Start (Empire : in out Root_Empire_Type'Class) is
      begin
         Empire.Border_Change := False;
      end On_Update_Start;

      -------------------------
      -- Update_System_Owner --
      -------------------------

      procedure Update_System_Owner
        (System : in out Concorde.Systems.Root_Star_System_Type'Class)
      is null;

   begin
      Db.Iterate (On_Update_Start'Access);

      declare
         function Has_Owner
           (System : Concorde.Systems.Root_Star_System_Type'Class)
            return Boolean
         is (System.Owned);
      begin
         Concorde.Systems.Update_Systems
           (Has_Owner'Access,
            Update_System_Owner'Access);
      end;

      Concorde.Systems.Update_Systems
        (Update_System_Flags'Access);

      Db.Iterate (Update_Empire'Access);

      History.Update_History;

   end Update_Empires;

   -------------------------
   -- Update_System_Flags --
   -------------------------

   procedure Update_System_Flags
     (System : in out Concorde.Systems.Root_Star_System_Type'Class)
   is

      procedure Clear_System_Flags
        (Empire : in out Root_Empire_Type'Class);

      procedure Update_Owner
        (Owner : in out Root_Empire_Type'Class);

      ------------------------
      -- Clear_System_Flags --
      ------------------------

      procedure Clear_System_Flags
        (Empire : in out Root_Empire_Type'Class)
      is null;
--        begin
--           Empire.Clear_System_Flags (System);
--        end Clear_System_Flags;

      ------------------
      -- Update_Owner --
      ------------------

      procedure Update_Owner
        (Owner : in out Root_Empire_Type'Class)
      is null;
--        begin
--           Owner.Update_System_Owner (System);
--        end Update_Owner;

   begin
      Db.Iterate (Clear_System_Flags'Access);

      if System.Owned then
         Db.Update (System.Owner.Reference, Update_Owner'Access);
      end if;

   end Update_System_Flags;

end Concorde.Empires.Updates;
