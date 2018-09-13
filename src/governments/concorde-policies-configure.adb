with Ada.Directories;
with Ada.Text_IO;

with Concorde.Calendar;
with Concorde.Configure;

with Concorde.Network.Expressions.Parser;
with Concorde.Network.Nodes.Configure;

package body Concorde.Policies.Configure is

   type Policy_Node_Type is
     new Concorde.Network.Nodes.Root_Node_Type with
      record
         null;
      end record;

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

         Node : Policy_Node_Type;

         Effect        : Boolean := False;
         Effect_Target : Boolean := False;
         Effect_Base   : Concorde.Network.Expressions.Expression_Type;
         Effect_Delay  : Non_Negative_Real;

         procedure On_Enter
           (Field_Name  : String);

         procedure On_Leave
           (Field_Name  : String);

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
         begin
            if Effect then
               if Effect_Target then
                  if Field_Name = "base" then
                     Effect_Base := Field_Value;
                  elsif Field_Name = "delay" then
                     Effect_Delay :=
                       Concorde.Network.Expressions.Evaluate (Field_Value);
                  else
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "warning: unknown field '" & Field_Name & "'"
                        & " in policy configuration for "
                        & Policy.Identifier);
                  end if;
               else
                  Concorde.Network.Nodes.Configure.Add_Effect
                    (From       => Node,
                     To         => Field_Name,
                     Expression => Field_Value,
                     Wait       => 0.0);
                  Ada.Text_IO.Put_Line
                    ("effect: " & Field_Name);
               end if;
            end if;
         end Configure_Field;

         --------------
         -- On_Enter --
         --------------

         procedure On_Enter
           (Field_Name  : String)
         is
         begin
            if Field_Name = "effects" then
               Effect := True;
            elsif Effect then
               Effect_Target := True;
            end if;
         end On_Enter;

         --------------
         -- On_Leave --
         --------------

         procedure On_Leave
           (Field_Name  : String)
         is
         begin
            if Field_Name = "effects" then
               Effect := False;
            elsif Effect_Target then
               Concorde.Network.Nodes.Configure.Add_Effect
                 (From       => Node,
                  To         => Field_Name,
                  Expression => Effect_Base,
                  Wait       => Concorde.Calendar.Days (Effect_Delay));
               Effect_Target := False;
            end if;
         end On_Leave;

         Id : constant String := Ada.Directories.Simple_Name (Path);

      begin
         Node.Initialise (Id);
         Policy.Set_Local_Tag (Id);

         Concorde.Network.Expressions.Parser.Parse_Configuration
           (Path        => Path,
            On_Enter    => On_Enter'Access,
            On_Leave    => On_Leave'Access,
            On_Config   => Configure_Field'Access);

         Policy.Node := new Policy_Node_Type'(Node);
         Concorde.Network.Nodes.Add_Node (Policy.Node);
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Policy;

end Concorde.Policies.Configure;
