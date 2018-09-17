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

         use type Concorde.Network.Metrics.Metric_Type;

         Calculation : Boolean := False;

         Node        : Concorde.Network.Metrics.Metric_Type;

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
            if Calculation then
               declare
                  use Concorde.Network.Metrics;
                  Operator : constant Metric_Operator :=
                               (if Field_Name = "add"
                                then Add
                                elsif Field_Name = "multiply"
                                then Multiply
                                else raise Constraint_Error with
                                  "unknown operator: " & Field_Name);
               begin
                  if Node = null then
                     Node :=
                       Concorde.Network.Metrics.New_Rating_Metric
                         (Metric.Identifier);
                  end if;
                  Node.Add_Calculation (Operator, Field_Value);
               end;

            else

               if Field_Name = "type" then
                  if Node /= null then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "warning: ignoring type field: "
                        & "must come before calculation"
                        & " in metric configuration for "
                        & Metric.Identifier);
                  else
                     if Field_Value.Show = "money" then
                        Node :=
                          Concorde.Network.Metrics.New_Money_Metric
                            (Metric.Identifier);
                     elsif Field_Value.Show = "quantity" then
                        Node :=
                          Concorde.Network.Metrics.New_Quantity_Metric
                            (Metric.Identifier);
                     elsif Field_Value.Show = "rating" then
                        Node :=
                          Concorde.Network.Metrics.New_Rating_Metric
                            (Metric.Identifier);
                     else
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           "warning: unknown metric type: "
                           & Field_Value.Show
                           & " in metric configuration for "
                           & Metric.Identifier);
                        Node :=
                          Concorde.Network.Metrics.New_Rating_Metric
                            (Metric.Identifier);
                     end if;
                  end if;
               else
                  Node.Add_Field (Field_Name, Field_Value);
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
            if Field_Name = "calculation" then
               Calculation := True;
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "warning: unknown configuration group '" & Field_Name & "'"
                  & " in metric configuration for "
                  & Metric.Identifier);
            end if;
         end On_Enter;

         --------------
         -- On_Leave --
         --------------

         procedure On_Leave
           (Field_Name  : String)
         is
         begin
            if Field_Name = "calculation" then
               Calculation := False;
            end if;
         end On_Leave;

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

            if Node = null then
               Node := Concorde.Network.Metrics.New_Rating_Metric (Id);
            end if;
            Metric.Node := Concorde.Network.Nodes.Node_Type (Node);

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
