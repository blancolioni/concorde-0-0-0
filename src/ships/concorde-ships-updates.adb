with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Galaxy.Ships;
with Concorde.Factions;
with Concorde.Systems;

with Concorde.Factions.Logging;

package body Concorde.Ships.Updates is

   function Is_Alive (Ship : Ship_Type) return Boolean
   is (Ship.Alive);

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists
       (Memor.Database_Reference, Memor."=");

   procedure Update_Ship (Ship : Ship_Type)
     with Pre => Ship.Alive;

   -----------------------
   -- Delete_Dead_Ships --
   -----------------------

   procedure Delete_Dead_Ships is
      List : List_Of_References.List;

      procedure Add_Dead_Ship
        (Ship : Root_Ship_Type'Class);

      procedure Remove_Dead_Ships
        (System : Concorde.Systems.Star_System_Type);

      -------------------
      -- Add_Dead_Ship --
      -------------------

      procedure Add_Dead_Ship
        (Ship : Root_Ship_Type'Class)
      is
      begin
         if not Ship.Alive then
            Concorde.Factions.Logging.Log
              (Ship.Owner,
               Ship.Short_Description & " destroyed");
            List.Append (Ship.Reference);
         end if;
      end Add_Dead_Ship;

      -----------------------
      -- Remove_Dead_Ships --
      -----------------------

      procedure Remove_Dead_Ships
        (System : Concorde.Systems.Star_System_Type)
      is
      begin
         System.Update.Remove_Dead_Ships;
      end Remove_Dead_Ships;

   begin

      Concorde.Systems.Scan_Systems (Remove_Dead_Ships'Access);

      Concorde.Ships.Db.Scan (Add_Dead_Ship'Access);

      for Reference of List loop
         Concorde.Ships.Db.Delete (Reference);
      end loop;
   end Delete_Dead_Ships;

   -----------------
   -- Update_Ship --
   -----------------

   procedure Update_Ship (Ship : Ship_Type) is

   begin

      if Ship.Has_Destination then
         Concorde.Factions.Logging.Log
           (Ship.Owner,
            Ship.Short_Description
            & " on its way to "
            & Ship.Destination.Name
            & " (distance"
            & Natural'Image
              (Ship.Owner.Path_Length
                   (Ship.Current_System, Ship.Destination.System))
            & ")");

         Concorde.Galaxy.Ships.Move_Ship (Ship);
      end if;
   end Update_Ship;

   ------------------
   -- Update_Ships --
   ------------------

   procedure Update_Ship_Movement is
   begin
      Concorde.Ships.Db.Scan (Is_Alive'Access, Update_Ship'Access);
   end Update_Ship_Movement;

   ------------------------
   -- Update_Ship_Orders --
   ------------------------

   procedure Update_Ship_Orders is

      function Alive_At_Destination
        (Ship : Root_Ship_Type'Class)
         return Boolean
      is (Ship.Alive and then not Ship.Has_Destination);

      procedure Update (Ship : in out Root_Ship_Type'Class);

      ------------
      -- Update --
      ------------

      procedure Update (Ship : in out Root_Ship_Type'Class) is
         use List_Of_Orders;
         Cycle_List : List;
         Position : Cursor := Ship.Orders.First;
      begin
         while Has_Element (Position) loop
            declare
               use type Memor.Database_Reference;
               use type WL.Quantities.Quantity_Type;
               Order : constant Ship_Order_Record := Element (Position);
            begin

               if Ship.Orbiting (Order.World) then
                  case Order.Order is
                     when No_Order =>
                        Ship.Orders.Delete (Position);
                        Position := Ship.Orders.First;
                     when Trade =>
                        declare
                           use WL.Quantities;
                           Finished : Boolean := True;

                           procedure Check_Buys
                             (Commodity : Commodities.Commodity_Type);

                           procedure Check_Sells
                             (Commodity : Commodities.Commodity_Type);

                           ----------------
                           -- Check_Buys --
                           ----------------

                           procedure Check_Buys
                             (Commodity : Commodities.Commodity_Type)
                           is
                           begin
                              if Ship.Get_Quantity (Commodity)
                                < Ship.Buy_Requirements.Get_Quantity
                                (Commodity)
                              then
                                 Finished := False;
                              end if;
                           end Check_Buys;

                           -----------------
                           -- Check_Sells --
                           -----------------

                           procedure Check_Sells
                             (Commodity : Commodities.Commodity_Type)
                           is
                           begin
                              if Ship.Buy_Requirements.Get_Quantity
                                (Commodity)
                                = Zero
                              then
                                 Finished := False;
                              end if;
                           end Check_Sells;

                        begin
                           if Ship.Total_Quantity < Ship.Hold_Quantity then
                              Ship.Buy_Requirements.Scan_Stock
                                (Check_Buys'Access);
                           end if;
                           Ship.Scan_Stock
                             (Check_Sells'Access);

                           if Finished then
                              Ship.Log_Trade
                                ("Finished trading at "
                                 & Order.World.Name);
                              Ship.Buy_Requirements.Clear_Stock;
                              if Ship.Cycle_Orders then
                                 Cycle_List.Append (Order);
                              end if;
                              Ship.Orders.Delete (Position);
                              Position := Ship.Orders.First;
                           else
                              exit;
                           end if;
                        end;

                     when Buy =>
                        if Ship.Total_Quantity = Ship.Hold_Quantity
                          or else Ship.Get_Quantity (Order.Commodity)
                          >= Order.Quantity
                        then
                           Ship.Log_Trade ("Finished buying "
                                           & Order.Commodity.Name);
                           if Ship.Cycle_Orders then
                              Cycle_List.Append (Order);
                           end if;
                           Ship.Orders.Delete (Position);
                           Position := Ship.Orders.First;
                        else
                           exit;
                        end if;
                     when Sell =>
                        if Ship.Get_Quantity (Order.Commodity)
                          = Quantities.Zero
                        then
                           Ship.Log_Trade ("Finished selling "
                                           & Order.Commodity.Name);
                           if Ship.Cycle_Orders then
                              Cycle_List.Append (Order);
                           end if;
                           Ship.Orders.Delete (Position);
                           Position := Ship.Orders.First;
                        else
                           exit;
                        end if;
                     when Colonise =>
                        exit;
                  end case;
               else
                  Ship.Log_Trade
                    ("Finished trading; heading to "
                     & Order.World.Name);
                  Ship.Dest_World := Order.World;
                  Ship.Dest_System := Order.World.System;
                  exit;
               end if;
            end;
         end loop;

         for Order of Cycle_List loop
            Ship.Orders.Append (Order);
         end loop;

      end Update;

   begin
      Concorde.Ships.Db.Iterate (Alive_At_Destination'Access, Update'Access);
   end Update_Ship_Orders;

end Concorde.Ships.Updates;
