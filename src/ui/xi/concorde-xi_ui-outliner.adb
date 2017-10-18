package body Concorde.Xi_UI.Outliner is

   package Outliner_Element_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Outliner_Element);

   function To_List
     (Elements : Array_Of_Elements)
      return Outliner_Element_Lists.List;

   type Outliner_Row is
      record
         Identifier : Ada.Strings.Unbounded.Unbounded_String;
         Elements   : Outliner_Element_Lists.List;
         Tooltip    : Outliner_Element_Lists.List;
      end record;

   package Outliner_Row_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Outliner_Row);

   function Get_Row
     (Rows       : Outliner_Row_Lists.List;
      Identifier : String)
      return Outliner_Row_Lists.Cursor;

   type Category_Record is
      record
         Identifier : Ada.Strings.Unbounded.Unbounded_String;
         Visible    : Boolean := True;
         Expanded   : Boolean := True;
         Rows       : Outliner_Row_Lists.List;
      end record;

   package Category_Record_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Category_Record);

   Category_List    : Category_Record_Lists.List;
   Outliner_Changed : Boolean := False;

   function Get_Category
     (Identifier : String)
      return Category_Record_Lists.Cursor;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Category : String;
      Identity : String;
      Element  : Outliner_Element;
      Tooltip  : Array_Of_Elements)
   is
   begin
      Add_Item
        (Category => Category,
         Identity => Identity,
         Elements => (1 => Element),
         Tooltip  => Tooltip);
   end Add_Item;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Category : String;
      Identity : String;
      Elements : Array_Of_Elements;
      Tooltip  : Array_Of_Elements)
   is
      Category_Position : constant Category_Record_Lists.Cursor :=
                            Get_Category (Category);
   begin
      Category_List (Category_Position).Rows.Append
        (Outliner_Row'
           (Identifier => +Identity,
            Elements   => To_List (Elements),
            Tooltip    => To_List (Tooltip)));
      Outliner_Changed := True;
   end Add_Item;

   --------------
   -- Collapse --
   --------------

   procedure Collapse (Category : String) is
      Category_Position : constant Category_Record_Lists.Cursor :=
                            Get_Category (Category);
   begin
      if Category_List (Category_Position).Expanded then
         Category_List (Category_Position).Expanded := False;
         Outliner_Changed := True;
      end if;
   end Collapse;

   ------------
   -- Expand --
   ------------

   procedure Expand (Category : String) is
      Category_Position : constant Category_Record_Lists.Cursor :=
                            Get_Category (Category);
   begin
      if not Category_List (Category_Position).Expanded then
         Category_List (Category_Position).Expanded := True;
         Outliner_Changed := True;
      end if;
   end Expand;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category
     (Identifier : String)
      return Category_Record_Lists.Cursor
   is
      use Ada.Strings.Unbounded;
   begin
      for Category_Position in Category_List.Iterate loop
         if Category_Record_Lists.Element (Category_Position).Identifier
           = Identifier
         then
            return Category_Position;
         end if;
      end loop;
      Category_List.Append
        (Category_Record'
           (Identifier => +Identifier,
            Visible    => True,
            Expanded   => True,
            Rows       => <>));
      return Category_List.Last;
   end Get_Category;

   -------------
   -- Get_Row --
   -------------

   function Get_Row
     (Rows       : Outliner_Row_Lists.List;
      Identifier : String)
      return Outliner_Row_Lists.Cursor
   is
      use Ada.Strings.Unbounded;
   begin
      for Row in Rows.Iterate loop
         if Outliner_Row_Lists.Element (Row).Identifier = Identifier then
            return Row;
         end if;
      end loop;
      return Outliner_Row_Lists.No_Element;
   end Get_Row;

   ----------
   -- Hide --
   ----------

   procedure Hide (Category : String) is
      Category_Position : constant Category_Record_Lists.Cursor :=
                            Get_Category (Category);
   begin
      if Category_List (Category_Position).Visible then
         Category_List (Category_Position).Visible := False;
         Outliner_Changed := True;
      end if;
   end Hide;

   -----------------
   -- Remove_Item --
   -----------------

   procedure Remove_Item
     (Category : String;
      Identity : String)
   is
      Category_Position : constant Category_Record_Lists.Cursor :=
                            Get_Category (Category);
      Row_Position      : Outliner_Row_Lists.Cursor :=
                            Get_Row
                              (Category_List (Category_Position).Rows,
                               Identity);
   begin
      Category_List (Category_Position).Rows.Delete (Row_Position);
      Outliner_Changed := True;
   end Remove_Item;

   ------------
   -- Render --
   ------------

   procedure Render is
      use Ada.Strings.Unbounded;
   begin
      if Outliner_Changed then
         Outliner_Div.Delete_Children;
         for Category of Category_List loop
            declare
               Cat_Label : constant Xtk.Label.Xtk_Label :=
                             Xtk.Label.Xtk_New
                               (To_String (Category.Identifier));
            begin
               Cat_Label.Show_All;
               Outliner_Div.Add_Child (Cat_Label);
            end;
         end loop;
         Outliner_Changed := False;
      end if;
   end Render;

   ----------
   -- Show --
   ----------

   procedure Show (Category : String) is
      Category_Position : constant Category_Record_Lists.Cursor :=
                            Get_Category (Category);
   begin
      if not Category_List (Category_Position).Visible then
         Category_List (Category_Position).Visible := True;
         Outliner_Changed := True;
      end if;
   end Show;

   -------------
   -- To_List --
   -------------

   function To_List
     (Elements : Array_Of_Elements)
      return Outliner_Element_Lists.List
   is
   begin
      return List : Outliner_Element_Lists.List do
         for E of Elements loop
            List.Append (E);
         end loop;
      end return;
   end To_List;

   -----------------
   -- Update_Item --
   -----------------

   procedure Update_Item
     (Category : String;
      Identity : String;
      Element  : Outliner_Element;
      Tooltip  : Array_Of_Elements)
   is
   begin
      Update_Item
        (Category => Category,
         Identity => Identity,
         Elements => (1 => Element),
         Tooltip  => Tooltip);
   end Update_Item;

   -----------------
   -- Update_Item --
   -----------------

   procedure Update_Item
     (Category : String;
      Identity : String;
      Elements : Array_Of_Elements;
      Tooltip  : Array_Of_Elements)
   is
      Category_Position : constant Category_Record_Lists.Cursor :=
                            Get_Category (Category);
      Row_Position      : constant Outliner_Row_Lists.Cursor :=
                            Get_Row
                              (Category_List (Category_Position).Rows,
                               Identity);
   begin
      Category_List (Category_Position).Rows (Row_Position) :=
        Outliner_Row'
          (Identifier => +Identity,
           Elements   => To_List (Elements),
           Tooltip    => To_List (Tooltip));
      Outliner_Changed := True;
   end Update_Item;

end Concorde.Xi_UI.Outliner;
