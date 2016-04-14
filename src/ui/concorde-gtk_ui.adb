with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Glib;
with Glib.Error;

with Gtk.Box;
with Gtk.Builder;
with Gtk.Button;
with Gtk.Label;
with Gtk.Main;
with Gtk.Notebook;
with Gtk.Status_Bar;
with Gtk.Widget;
with Gtk.Window;

with Concorde.Dates;
with Concorde.Galaxy.Model;
with Concorde.Paths;
with Concorde.Updates;

with Concorde.Options;

with Lui.Models;
with Lui.Rendering;

with Lui.Gtk_UI;

package body Concorde.Gtk_UI is

   type Concorde_UI is new Lui.Gtk_UI.Lui_Gtk_Interface with
      record
         Models         : Lui.Models.Active_Model_List;
         Notebook       : Gtk.Notebook.Gtk_Notebook;
         Info_Boxes     : Gtk.Box.Gtk_Box;
         Date_Label     : Gtk.Label.Gtk_Label;
         Status_Bar     : Gtk.Status_Bar.Gtk_Status_Bar;
         Status_Context : Gtk.Status_Bar.Context_Id;
         Current_Status : Ada.Strings.Unbounded.Unbounded_String;
         Last_Date      : Concorde.Dates.Date_Type;
      end record;

   overriding procedure Append_Feature
     (To      : in out Concorde_UI;
      Feature : Lui.Lui_UI_Feature;
      Element : not null access Lui.Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   overriding procedure Select_Feature
     (To      : in out Concorde_UI;
      Feature : Lui.Lui_UI_Feature;
      Element : not null access Lui.Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is null;

   overriding procedure Clear_Features
     (To      : in out Concorde_UI;
      Feature : Lui.Lui_UI_Feature);

   overriding procedure Status_Message
     (To      : in out Concorde_UI;
      Message : String);

   overriding procedure On_Idle
     (State : in out Concorde_UI);

   procedure Destroy_Handler (W : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Step_Button_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Pause_Button_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Play_1_Button_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Play_2_Button_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   --------------------
   -- Append_Feature --
   --------------------

   overriding procedure Append_Feature
     (To      : in out Concorde_UI;
      Feature : Lui.Lui_UI_Feature;
      Element : not null access Lui.Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use Glib;
      use Lui;
   begin
      case Feature is
         when UI_Gadget =>
            null;
         when UI_Model =>
            To.Models.Append (Lui.Models.Object_Model (Element));
            To.Notebook.Append_Page (Top);
            To.Notebook.Set_Current_Page
              (To.Notebook.Get_N_Pages - 1);
         when UI_Table =>
            To.Info_Boxes.Add (Top);
      end case;
   end Append_Feature;

   --------------------
   -- Clear_Features --
   --------------------

   overriding procedure Clear_Features
     (To      : in out Concorde_UI;
      Feature : Lui.Lui_UI_Feature)
   is
      use Lui;
      use type Gtk.Widget.Gtk_Widget;
   begin
      case Feature is
         when UI_Gadget =>
            null;
         when UI_Model =>
            null;
         when UI_Table =>
            while To.Info_Boxes.Get_Child (0) /= null loop
               To.Info_Boxes.Remove (To.Info_Boxes.Get_Child (0));
            end loop;
      end case;
   end Clear_Features;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler (W : access Gtk.Widget.Gtk_Widget_Record'Class) is
      pragma Unreferenced (W);
   begin
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   -------------
   -- On_Idle --
   -------------

   overriding procedure On_Idle
     (State : in out Concorde_UI)
   is
      use Concorde.Dates;
      Current : constant Date_Type := Current_Date;
   begin
      if Current /= State.Last_Date then
         State.Date_Label.Set_Label (To_String (Current));
         State.Last_Date := Current;
      end if;
   end On_Idle;

   -----------------------------
   -- On_Pause_Button_Clicked --
   -----------------------------

   procedure On_Pause_Button_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      Concorde.Updates.Set_Update_Speed (0);
   end On_Pause_Button_Clicked;

   ------------------------------
   -- On_Play_1_Button_Clicked --
   ------------------------------

   procedure On_Play_1_Button_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      Concorde.Updates.Set_Update_Speed (1);
   end On_Play_1_Button_Clicked;

   ------------------------------
   -- On_Play_2_Button_Clicked --
   ------------------------------

   procedure On_Play_2_Button_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      Concorde.Updates.Set_Update_Speed (4);
   end On_Play_2_Button_Clicked;

   ----------------------------
   -- On_Step_Button_Clicked --
   ----------------------------

   procedure On_Step_Button_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      Concorde.Updates.Perform_Update
        (False, Concorde.Options.Check_Invariants);
   end On_Step_Button_Clicked;

   -----------
   -- Start --
   -----------

   procedure Start is
      Builder : Gtk.Builder.Gtk_Builder;
      UI_Path : constant String :=
                  Concorde.Paths.Config_Path & "/concorde.ui";
   begin

      Lui.Rendering.Set_Image_Path
        (Concorde.Paths.Config_Path & "/images");

      Gtk.Main.Init;

      Gtk.Builder.Gtk_New (Builder);

      Ada.Text_IO.Put_Line ("Loading: " & UI_Path);

      declare
         use type Glib.Guint;
         Error : aliased Glib.Error.GError;
         Result : constant Glib.Guint :=
                    Builder.Add_From_File
                      (Filename => UI_Path,
                       Error    => Error'Access);
      begin
         if Result = 0 then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Error opening GUI definition: " & UI_Path
               & ": " & Glib.Error.Get_Message (Error));
            return;
         end if;
      end;

      Ada.Text_IO.Put_Line ("done");

      declare
         Main_Window : constant Gtk.Window.Gtk_Window :=
                         Gtk.Window.Gtk_Window
                           (Builder.Get_Object ("Concorde_Main_Window"));
      begin
         Main_Window.On_Destroy (Destroy_Handler'Access);
         Main_Window.Set_Hide_Titlebar_When_Maximized (True);
         Main_Window.Show_All;
         Main_Window.Maximize;
      end;

      declare
         Main_Tab : constant Gtk.Notebook.Gtk_Notebook :=
                      Gtk.Notebook.Gtk_Notebook
                        (Builder.Get_Object ("Main_Tab"));
         Info_Boxes : constant Gtk.Box.Gtk_Box :=
                        Gtk.Box.Gtk_Box
                          (Builder.Get_Object ("Info_Vbox"));
         Date_Label : constant Gtk.Label.Gtk_Label :=
                        Gtk.Label.Gtk_Label
                          (Builder.Get_Object ("Date_Label"));
         Status_Bar : constant Gtk.Status_Bar.Gtk_Status_Bar :=
                        Gtk.Status_Bar.Gtk_Status_Bar
                          (Builder.Get_Object ("Status_Bar"));
         Models   : Lui.Models.Active_Model_List;
         UI         : constant Lui.Gtk_UI.Lui_Gtk :=
                        new Concorde_UI'
                          (Models         => Models,
                           Notebook       => Main_Tab,
                           Info_Boxes     => Info_Boxes,
                           Date_Label     => Date_Label,
                           Last_Date      => 0,
                           Status_Bar     => Status_Bar,
                           Status_Context =>
                             Status_Bar.Get_Context_Id ("star mouseover"),
                           Current_Status =>
                             Ada.Strings.Unbounded.Null_Unbounded_String);
      begin
         Main_Tab.Remove_Page (0);
         Lui.Gtk_UI.Start
           (Main => UI,
            Top  => Concorde.Galaxy.Model.Galaxy_Model);
      end;

      declare
         Pause  : constant Gtk.Button.Gtk_Button :=
                    Gtk.Button.Gtk_Button
                      (Builder.Get_Object ("Game_Pause"));
         Play_1 : constant Gtk.Button.Gtk_Button :=
                    Gtk.Button.Gtk_Button
                      (Builder.Get_Object ("Game_Play_1"));
         Play_2 : constant Gtk.Button.Gtk_Button :=
                    Gtk.Button.Gtk_Button
                      (Builder.Get_Object ("Game_Play_2"));
         Step  : constant Gtk.Button.Gtk_Button :=
                    Gtk.Button.Gtk_Button
                      (Builder.Get_Object ("Game_Step"));
      begin
         Pause.On_Clicked (On_Pause_Button_Clicked'Access);
         Play_1.On_Clicked (On_Play_1_Button_Clicked'Access);
         Play_2.On_Clicked (On_Play_2_Button_Clicked'Access);
         Step.On_Clicked (On_Step_Button_Clicked'Access);
      end;

      Gtk.Main.Main;

   end Start;

   --------------------
   -- Status_Message --
   --------------------

   overriding procedure Status_Message
     (To      : in out Concorde_UI;
      Message : String)
   is
      use Ada.Strings.Unbounded;
   begin
      if Message /= To.Current_Status then
         To.Current_Status := To_Unbounded_String (Message);
         declare
            Id      : Gtk.Status_Bar.Message_Id;
            pragma Unreferenced (Id);
         begin
            To.Status_Bar.Pop (To.Status_Context);

            if Message /= "" then
               Id :=
                 To.Status_Bar.Push
                   (To.Status_Context, Message);
            end if;
         end;
      end if;
   end Status_Message;

end Concorde.Gtk_UI;
