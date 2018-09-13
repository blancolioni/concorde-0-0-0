with Ada.Directories;
with Ada.Text_IO;

with Concorde.Configure;

with Concorde.Network.Expressions.Parser;
with Concorde.Network.Metrics;

package body Concorde.Metrics.Configure is

   procedure Configure_Metric (Path : String);

   ----------------------
   -- Configure_Metric --
   ----------------------

   procedure Configure_Metric (Path : String) is

      procedure Create (Metric : in out Root_Metric_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Metric : in out Root_Metric_Type'Class) is

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
            if Field_Name = "type" then
               if Field_Value.Show = "money" then
                  Metric.Node :=
                    Concorde.Network.Metrics.New_Money_Metric
                      (Metric.Identifier);
               elsif Field_Name = "rating" then
                  null;
               else
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "warning: unknown metric type: "
                     & " in metric configuration for "
                     & Metric.Identifier);
               end if;
            else

               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "warning: unknown field '" & Field_Name & "'"
                  & " in metric configuration for "
                  & Metric.Identifier);
            end if;
         end Configure_Field;

         --------------
         -- On_Enter --
         --------------

         procedure On_Enter
           (Field_Name  : String)
         is null;

         --------------
         -- On_Leave --
         --------------

         procedure On_Leave
           (Field_Name  : String)
         is null;

         Id : constant String := Ada.Directories.Base_Name (Path);

      begin
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "new metric: " & Id);

         Metric.Set_Local_Tag (Id);
         Metric.Active := True;

         Concorde.Network.Expressions.Parser.Parse_Configuration
           (Path        => Path,
            On_Enter    => On_Enter'Access,
            On_Leave    => On_Leave'Access,
            On_Config   => Configure_Field'Access);

         declare
            use type Concorde.Network.Nodes.Node_Type;
         begin
            if Metric.Node = null then
               Metric.Node := Concorde.Network.Metrics.New_Rating_Metric (Id);
            end if;
         end;

         Concorde.Network.Nodes.Add_Node (Metric.Node);
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Metric;

   -----------------------
   -- Configure_Metrics --
   -----------------------

   procedure Configure_Metrics is
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
         Configure_Metric (Full_Name (Item));
      end Configure;

   begin
      Search
        (Directory => Concorde.Configure.Directory_Path ("metrics"),
         Pattern   => "*.metric",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Configure'Access);
   end Configure_Metrics;

end Concorde.Metrics.Configure;
