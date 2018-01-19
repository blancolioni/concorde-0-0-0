with Concorde.People.Attributes.Configure;

package body Concorde.Offices.Configure is

   procedure Configure_Office
     (Config : Tropos.Configuration);

   ----------------------
   -- Configure_Office --
   ----------------------

   procedure Configure_Office
     (Config : Tropos.Configuration)
   is
      procedure Create (Office : in out Root_Office_Type'Class);

      procedure Create (Office : in out Root_Office_Type'Class) is
      begin
         Office.Set_Local_Tag (Config.Config_Name);

         declare
            Resp_Config : constant Tropos.Configuration :=
                            Config.Child ("responsibility");
         begin
            for R in Responsibility_Type loop
               Office.Responsibilities (R) :=
                 Resp_Config.Contains (Responsibility_Type'Image (R));
            end loop;

         end;

         Concorde.People.Attributes.Configure.Configure_Attributes
           (Container => Office.Effect,
            Config    => Config.Child ("effect"));

      end Create;

      Office : constant Office_Type :=
                 Db.Create (Create'Access);
   begin
      Office_Vector.Append (Office);
   end Configure_Office;

   -----------------------
   -- Configure_Offices --
   -----------------------

   procedure Configure_Offices
     (Config : Tropos.Configuration)
   is
   begin
      for Office_Config of Config loop
         Configure_Office (Office_Config);
      end loop;
   end Configure_Offices;

end Concorde.Offices.Configure;
