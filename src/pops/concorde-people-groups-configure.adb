with Tropos.Reader;

with Concorde.Paths;

with Concorde.People.Groups.Db;

package body Concorde.People.Groups.Configure is

   procedure Configure_Pop_Group
     (Config : Tropos.Configuration);

   procedure Create_Pop_Group
     (Config : Tropos.Configuration);

   -------------------------
   -- Configure_Pop_Group --
   -------------------------

   procedure Configure_Pop_Group
     (Config : Tropos.Configuration)
   is
   begin
      null;
   end Configure_Pop_Group;

   --------------------------
   -- Configure_Pop_Groups --
   --------------------------

   procedure Configure_Pop_Groups is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Concorde.Paths.Config_File
                      ("pops/pop_group.txt"));
   begin
      for Group_Config of Config loop
         Create_Pop_Group (Group_Config);
      end loop;
      for Group_Config of Config loop
         Configure_Pop_Group (Group_Config);
      end loop;
   end Configure_Pop_Groups;

   ----------------------
   -- Create_Pop_Group --
   ----------------------

   procedure Create_Pop_Group
     (Config : Tropos.Configuration)
   is
      Name  : constant String := Config.Config_Name;

      procedure Create (Group : in out Root_Pop_Group'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Group : in out Root_Pop_Group'Class) is
      begin
         Group.Pop_Group_Id := new String'(Name);
         Group.Initial_Cash := Config.Get ("initial_cash", 0);
      end Create;

   begin
      Db.Create (Create'Access);
   end Create_Pop_Group;

end Concorde.People.Groups.Configure;
