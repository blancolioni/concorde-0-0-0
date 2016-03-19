with Ada.Text_IO;

with Glib;
with Glib.Error;

with Gtk.Box;
with Gtk.Builder;
with Gtk.Label;
with Gtk.Main;
with Gtk.Notebook;
with Gtk.Widget;
with Gtk.Window;

with Concorde.Dates;
with Concorde.Galaxy.Model;
with Concorde.Paths;

with Lui.Models;

with Lui.Gtk_UI;

package body Concorde.Gtk_UI is

   type Concorde_UI is new Lui.Gtk_UI.Lui_Gtk_Interface with
      record
         Models     : Lui.Models.Active_Model_List;
         Notebook   : Gtk.Notebook.Gtk_Notebook;
         Info_Boxes : Gtk.Box.Gtk_Box;
         Date_Label : Gtk.Label.Gtk_Label;
         Last_Date  : Concorde.Dates.Date_Type;
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
      Message : String)
   is null;

   overriding procedure On_Idle
     (State : in out Concorde_UI);

   procedure Destroy_Handler (W : access Gtk.Widget.Gtk_Widget_Record'Class);

   --------------------
   -- Append_Feature --
   --------------------

   overriding procedure Append_Feature
     (To      : in out Concorde_UI;
      Feature : Lui.Lui_UI_Feature;
      Element : not null access Lui.Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use Lui;
   begin
      case Feature is
         when UI_Gadget =>
            null;
         when UI_Model =>
            To.Models.Append (Lui.Models.Object_Model (Element));
            To.Notebook.Append_Page (Top);
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

   -----------
   -- Start --
   -----------

   procedure Start is
      Builder : Gtk.Builder.Gtk_Builder;
      UI_Path : constant String :=
                  Concorde.Paths.Config_Path & "/concorde.ui";
   begin

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
         Models   : Lui.Models.Active_Model_List;
         UI       : constant Lui.Gtk_UI.Lui_Gtk :=
                      new Concorde_UI'
                        (Models => Models,
                         Notebook => Main_Tab,
                         Info_Boxes => Info_Boxes,
                         Date_Label => Date_Label,
                         Last_Date  => 0);
      begin
         Main_Tab.Remove_Page (0);
         Lui.Gtk_UI.Start
           (Main => UI,
            Top  => Concorde.Galaxy.Model.Galaxy_Model);
      end;

      Gtk.Main.Main;

   end Start;

end Concorde.Gtk_UI;
