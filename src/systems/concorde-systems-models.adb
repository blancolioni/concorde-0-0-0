with Lui.Rendering;
with Lui.Tables;

with Concorde.Hash_Table;
with Concorde.Watchers;

with Concorde.Empires;

with Concorde.Systems.Db;

package body Concorde.Systems.Models is

   subtype Ship_Column is Integer range 1 .. 4;

   type Ship_Table is
     new Lui.Tables.Root_Model_Table with
      record
         System : Star_System_Type;
      end record;

   overriding function Heading_Column_Text
     (Table : Ship_Table;
      Col   : Positive)
      return String
   is ((case Ship_Column (Col) is
           when 1 => "Id",
           when 2 => "Name",
           when 3 => "Owner",
           when 4 => "Destination"));

   overriding function Cell_Text
     (Table : Ship_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   overriding function Row_Count
     (Table : Ship_Table)
      return Natural
   is (Natural (Table.System.Ships.Length));

   type Root_Star_System_Model is
     new Lui.Models.Root_Object_Model
     and Concorde.Watchers.Watcher_Interface with
      record
         System       : Star_System_Type;
         Needs_Render : Boolean := True;
      end record;

   overriding procedure On_Object_Changed
     (Model  : in out Root_Star_System_Model;
      Object : Concorde.Watchers.Watched_Object_Interface'Class);

   overriding function Handle_Update
     (Model    : in out Root_Star_System_Model)
      return Boolean
   is (Model.Needs_Render);

   overriding procedure Render
     (Model    : in out Root_Star_System_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   type Star_System_Model_Access is
     access all Root_Star_System_Model'Class;

   package Model_Table is
     new Concorde.Hash_Table (Star_System_Model_Access);

   System_Models : Model_Table.Map;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Ship_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      use Concorde.Ships.Lists;
      Position : Cursor := Table.System.Ships.First;
   begin
      for I in 2 .. Row loop
         Next (Position);
      end loop;

      declare
         Ship : constant Concorde.Ships.Ship_Type :=
                  Element (Position);
      begin
         case Ship_Column (Col) is
            when 1 =>
               return Ship.Identifier;
            when 2 =>
               return Ship.Name;
            when 3 =>
               return Ship.Owner.Name;
            when 4 =>
               if Ship.Has_Destination then
                  return Ship.Destination.Name;
               else
                  return "-";
               end if;
         end case;
      end;
   end Cell_Text;

   -----------------------
   -- On_Object_Changed --
   -----------------------

   overriding procedure On_Object_Changed
     (Model  : in out Root_Star_System_Model;
      Object : Concorde.Watchers.Watched_Object_Interface'Class)
   is
      pragma Unreferenced (Object);
   begin
      Model.Needs_Render := True;
   end On_Object_Changed;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Root_Star_System_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
   begin
      Renderer.Draw_Image
        (Model.Width / 2 - 50, Model.Height / 2 - 50, 100, 100,
         "planets/terrestrial-planet");
      Model.Needs_Render := False;
   end Render;

   ------------------
   -- System_Model --
   ------------------

   function System_Model
     (System : Star_System_Type)
      return Lui.Models.Object_Model
   is
      Result : Star_System_Model_Access;

      procedure Watch (System : in out Root_Star_System_Type'Class);

      -----------
      -- Watch --
      -----------

      procedure Watch (System : in out Root_Star_System_Type'Class) is
      begin
         System.Add_Watcher (Result);
      end Watch;

   begin
      if not System_Models.Contains (System.Name) then
         declare
            S_Table : Ship_Table;
            S       : Lui.Tables.Model_Table;
         begin
            S_Table.System := System;
            S_Table.Initialise ("Ships", 0, Ship_Column'Last);
            S := new Ship_Table'(S_Table);
            Result := new Root_Star_System_Model;
            Result.Initialise (System.Name, Tables => (1 => S));
            Db.Update (System.Reference, Watch'Access);
         end;

         System_Models.Insert (System.Name, Result);
      else
         Result := System_Models.Element (System.Name);
      end if;

      return Lui.Models.Object_Model (Result);
   end System_Model;

end Concorde.Systems.Models;
