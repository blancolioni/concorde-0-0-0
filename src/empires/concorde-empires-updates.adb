with WL.Random;

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
         Start_Fleets        : Natural;
      end record;

   type Array_Of_System_Orders is
     array (Positive range <>) of System_Orders;

   procedure Add_Move_Orders
     (Empire : Empire_Type;
      Target : Concorde.Systems.Star_System_Type;
      Orders : in out Array_Of_System_Orders);

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
      Orders : Array_Of_System_Orders (1 .. Concorde.Galaxy.System_Count);
   begin

      Empire.AI.Update_Focus;

      for I in Orders'Range loop
         Orders (I).Start_Fleets := Galaxy.Get_System (I).Fleets;
      end loop;

      declare
         procedure Go (Focus : Concorde.Systems.Star_System_Type);

         --------
         -- Go --
         --------

         procedure Go (Focus : Concorde.Systems.Star_System_Type) is
         begin
            Add_Move_Orders (Empire, Focus, Orders);
         end Go;

      begin
         Empire.Focus_List.Iterate (Go'Access);
      end;

      for I in Orders'Range loop
         declare
            Order  : System_Orders renames Orders (I);
            System : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Galaxy.Get_System (I);
         begin
            if System.Owner = Empire and then Order.Start_Fleets > 0 then
               Logging.Log
                 (Empire, "move orders for " & System.Name
                  & " with" & Natural'Image (Order.Start_Fleets)
                  & " fleets");
               declare
                  Remaining_Fleets   : Natural := Order.Start_Fleets;
                  Remaining_Pressure : Non_Negative_Real :=
                                         Order.Total_Move_Pressure;
                  Remaining_Moves    : List_Of_Move_Orders.List;
                  Total_Fleets       : constant Non_Negative_Real :=
                                         Non_Negative_Real
                                           (Order.Start_Fleets);
               begin
                  for Move of Order.Moves loop
                     declare
                        Relative_Pressure : constant Non_Negative_Real :=
                                              Move.Pressure
                                                / Order.Total_Move_Pressure;
                        Move_Pressure     : constant Real :=
                                              Relative_Pressure
                                                * Total_Fleets;
                        Fleets_Moved      : constant Natural :=
                                              Natural
                                                (Real'Truncation
                                                   (Move_Pressure));
                        Used_Relative     : constant Non_Negative_Real :=
                                              Real (Fleets_Moved)
                                              / Total_Fleets;
                        Used_Pressure     : constant Non_Negative_Real :=
                                              Used_Relative
                                                * Order.Total_Move_Pressure;
                     begin
                        if Relative_Pressure > 0.01 then
                           Logging.Log
                             (Empire,
                              "   pressure"
                              & Natural'Image
                                (Natural (Relative_Pressure * 100.0))
                              & "% to " & Move.To.Name);
                        end if;

                        if Fleets_Moved > 0 then
                           Concorde.Galaxy.Move_Fleets
                             (System, Move.To, Fleets_Moved);
                           Remaining_Fleets :=
                             Remaining_Fleets - Fleets_Moved;

                           Remaining_Pressure :=
                             Remaining_Pressure - Used_Pressure;

                           Remaining_Moves.Append
                             ((Move.To, Move.Pressure - Used_Pressure));

                           Logging.Log
                             (Empire,
                              "    move"
                              & Fleets_Moved'Img
                              & " fleets to " & Move.To.Name);
                        end if;

                     end;
                  end loop;

                  --  move remaining fleets stochastically
                  for I in 1 .. Remaining_Fleets loop
                     declare
                        X : Real :=
                              Real (WL.Random.Random_Number (0, 999))
                              * Remaining_Pressure / 1000.0;
                     begin
                        for Move of Remaining_Moves loop
                           if X <= Move.Pressure then
                              Concorde.Galaxy.Move_Fleets
                                (System, Move.To, 1);
                              Logging.Log
                                (Empire,
                                 "    move 1 fleet to "
                                 & Move.To.Name);
                              exit;
                           end if;
                           X := X - Move.Pressure;
                        end loop;
                     end;
                  end loop;

               end;
            end if;
         end;
      end loop;

   end Update_Empire;

   --------------------
   -- Update_Empires --
   --------------------

   procedure Update_Empires is
   begin
      for Empire of Vector loop
         Empire.Max_Fleets := 0;
         Empire.Current_Fleets := 0;
      end loop;

      for I in 1 .. Concorde.Galaxy.System_Count loop
         declare
            System : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Galaxy.Get_System (I);
            Empire : constant Empire_Type := System.Owner;
         begin
            if Empire /= null then
               Empire.Max_Fleets := Empire.Max_Fleets
                 + Natural (System.Capacity);
               Empire.Current_Fleets := Empire.Current_Fleets
                 + System.Fleets;
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
