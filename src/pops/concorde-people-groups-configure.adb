with WL.Money;

with Tropos.Reader;

with Concorde.Configure;
with Concorde.Commodities.Configure;

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
                   (Path      =>
                      Concorde.Configure.Directory_Path
                        ("poptypes"),
                    Extension => "txt");
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

         procedure Configure_Needs
           (Name  : String;
            Level : Need_Level);

         ---------------------
         -- Configure_Needs --
         ---------------------

         procedure Configure_Needs
           (Name        : String;
            Level       : Need_Level)
         is
            Need_Config : constant Tropos.Configuration :=
                            (if Config.Contains (Name)
                             then Config.Child (Name)
                             else Tropos.Empty_Config);
         begin
            for Item of Need_Config loop
               declare
                  Commodity : constant Concorde.Commodities.Commodity_Type :=
                                Concorde.Commodities.Get
                                  (Item.Config_Name);
                  Value     : constant Float := Item.Value;
               begin
                  Group.Needs (Level).Append
                    (Need_Record'
                       (Commodity     => Commodity,
                        Need_Quantity =>
                          WL.Quantities.To_Quantity (Value * 1000.0),
                        Pop_Quantity  =>
                          WL.Quantities.To_Quantity (10_000.0)));
               end;
            end loop;
         end Configure_Needs;

      begin
         Group.Set_Local_Tag (Name);
         Group.Is_Artisan := Config.Get ("is_artisan");
         Group.Is_Slave := Config.Get ("is_slave");
         Group.Unemployment := Config.Get ("unemployment");

         Group.Wealth := Wealth_Level'Value (Config.Get ("strata"));
         Group.Initial_Cash_Factor :=
           (case Group.Wealth is
               when Poor => 1.0,
               when Middle => 5.0,
               when Rich   => 20.0);

         if Group.Unemployment then
            Group.Work_Commodity :=
              Concorde.Commodities.Configure.Create_From_Group
                (Name,
                 WL.Money.To_Price
                   (4.0 * Float (Group.Initial_Cash_Factor)));

         end if;

         Group.Max_Size :=
           WL.Quantities.To_Quantity
             (Config.Get ("max_size", 1.0e9));

         Configure_Needs ("life_needs", Basic);
         Configure_Needs ("everyday_needs", Daily);
         Configure_Needs ("luxury_needs", Desire);

      end Create;

   begin
      Db.Create (Create'Access);
   end Create_Pop_Group;

end Concorde.People.Groups.Configure;
