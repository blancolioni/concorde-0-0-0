with Lui.Rendering;

with Concorde.Hash_Table;

package body Concorde.Systems.Models is

   type Root_Star_System_Model is
     new Lui.Models.Root_Object_Model with
      record
         System : Star_System_Type;
      end record;

   overriding procedure Render
     (Model    : in out Root_Star_System_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   package Model_Table is
     new Concorde.Hash_Table (Lui.Models.Object_Model, Lui.Models."=");

   System_Models : Model_Table.Map;

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
   end Render;

   ------------------
   -- System_Model --
   ------------------

   function System_Model
     (System : Star_System_Type)
      return Lui.Models.Object_Model
   is
      Result : Lui.Models.Object_Model;
   begin
      if not System_Models.Contains (System.Name) then
         Result := new Root_Star_System_Model;
         Result.Initialise (System.Name);
         System_Models.Insert (System.Name, Result);
      else
         Result := System_Models.Element (System.Name);
      end if;

      return Result;
   end System_Model;

end Concorde.Systems.Models;
