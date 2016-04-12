with Ada.Containers.Doubly_Linked_Lists;

with Concorde.AI;
with Concorde.Galaxy.Ships;
with Concorde.Empires;
with Concorde.Systems;

with Concorde.Empires.Logging;

with Concorde.Empires.Db;
with Concorde.Ships.Db;
with Concorde.Systems.Db;

package body Concorde.Ships.Updates is

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists
       (Memor.Database_Reference, Memor."=");

   procedure Update_Ship (Ship : in out Root_Ship_Type'Class)
     with Pre => Ship.Alive;

   -----------------------
   -- Delete_Dead_Ships --
   -----------------------

   procedure Delete_Dead_Ships is
      List : List_Of_References.List;

      procedure Add_Dead_Ship
        (Ship : Root_Ship_Type'Class);

      procedure Remove_Dead_Ships
        (System : in out Concorde.Systems.Root_Star_System_Type'Class);

      -------------------
      -- Add_Dead_Ship --
      -------------------

      procedure Add_Dead_Ship
        (Ship : Root_Ship_Type'Class)
      is
      begin
         if not Ship.Alive then
            Concorde.Empires.Logging.Log
              (Ship.Owner,
               Ship.Short_Description & " destroyed");
            List.Append (Ship.Reference);
         end if;
      end Add_Dead_Ship;

      -----------------------
      -- Remove_Dead_Ships --
      -----------------------

      procedure Remove_Dead_Ships
        (System : in out Concorde.Systems.Root_Star_System_Type'Class)
      is
      begin
         System.Remove_Dead_Ships;
      end Remove_Dead_Ships;

   begin

      Concorde.Systems.Db.Iterate (Remove_Dead_Ships'Access);

      Concorde.Ships.Db.Scan (Add_Dead_Ship'Access);

      for Reference of List loop
         Concorde.Ships.Db.Delete (Reference);
      end loop;
   end Delete_Dead_Ships;

   -----------------
   -- Update_Ship --
   -----------------

   procedure Update_Ship (Ship : in out Root_Ship_Type'Class) is

   begin

      declare
         procedure Order
           (Empire : in out Concorde.Empires.Root_Empire_Type'Class);

         -----------
         -- Order --
         -----------

         procedure Order
           (Empire : in out Concorde.Empires.Root_Empire_Type'Class)
         is
         begin
            Ship.Owner.AI.Order_Ship
              (Empire, Ship);
         end Order;

      begin
         Concorde.Empires.Db.Update (Ship.Owner.Reference, Order'Access);
      end;

      if Ship.Has_Destination then
         Concorde.Empires.Logging.Log
           (Ship.Owner,
            Ship.Short_Description
            & " on its way to "
            & Ship.Destination.Name
            & " (distance"
            & Natural'Image
              (Ship.Owner.Path_Length (Ship.System, Ship.Destination))
            & ")");

         Concorde.Galaxy.Ships.Move_Ship (Ship);
      end if;
   end Update_Ship;

   ------------------
   -- Update_Ships --
   ------------------

   procedure Update_Ship_Movement is
      function Is_Alive (Ship : Root_Ship_Type'Class) return Boolean
      is (Ship.Alive);
   begin
      Concorde.Ships.Db.Iterate (Is_Alive'Access, Update_Ship'Access);
   end Update_Ship_Movement;

end Concorde.Ships.Updates;
