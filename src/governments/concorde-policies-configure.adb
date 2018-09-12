with Ada.Text_IO;
with Ada.Directories;

with Concorde.Configure;

with Concorde.Network.Expressions.Parser;

package body Concorde.Policies.Configure is

   procedure Configure_Policy (Path : String);

   ------------------------
   -- Configure_Policies --
   ------------------------

   procedure Configure_Policies is
      use Ada.Directories;

      procedure Configure
        (Item : Directory_Entry_Type);

      ---------------
      -- Configure --
      ---------------

      procedure Configure
        (Item : Directory_Entry_Type)
      is
      begin
         Configure_Policy (Full_Name (Item));
      end Configure;

   begin
      Search
        (Directory => Concorde.Configure.Directory_Path ("policies"),
         Pattern   => "*.policy",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Configure'Access);
   end Configure_Policies;

   ----------------------
   -- Configure_Policy --
   ----------------------

   procedure Configure_Policy (Path : String) is

      procedure Create (Policy : in out Root_Policy_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Policy : in out Root_Policy_Type'Class) is
         pragma Unreferenced (Policy);

         procedure Configure_Field
           (Field_Name  : String;
            Field_Value : Concorde.Network.Expressions.Expression_Type);

         ---------------------
         -- Configure_Field --
         ---------------------

         procedure Configure_Field
           (Field_Name  : String;
            Field_Value : Concorde.Network.Expressions.Expression_Type)
         is
            pragma Unreferenced (Field_Value);
         begin
            Ada.Text_IO.Put_Line
              ("field: " & Field_Name);
         end Configure_Field;

      begin
         Concorde.Network.Expressions.Parser.Parse_Configuration
           (Path, Configure_Field'Access);
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Policy;

end Concorde.Policies.Configure;
