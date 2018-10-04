with Concorde.Commodities.Configure;

with Concorde.People.Groups;
with Concorde.Production;

with Concorde.Corporations.Create;
with Concorde.Government.Create;
with Concorde.Industries.Create;
with Concorde.People.Pops.Create;
with Concorde.Worlds;

with Concorde.Managers.Communities;

with Concorde.Objects.Queues;
with Concorde.Random;

package body Concorde.People.Communities.Create is

   -------------------
   -- New_Community --
   -------------------

   function New_Community
     (World         : not null access constant
        Concorde.Worlds.Root_World_Type'Class;
      Faction       : not null access constant
        Concorde.Factions.Root_Faction_Type'Class;
      Template      : Tropos.Configuration)
      return Community_Type
   is
      use Concorde.Quantities;

      Total_Pop : Quantity_Type := Zero;
      Total_Land : Non_Negative_Real := 0.0;

      procedure Create (Community : in out Root_Community_Type'Class);

      procedure Create_Pops (Community : in out Root_Community_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Community : in out Root_Community_Type'Class) is
      begin
         Community.Set_Name (World.Name);
         Community.World := Concorde.Worlds.World_Type (World);
         Community.Owner := Concorde.Factions.Faction_Type (Faction);
         Community.Occupation :=
           Real (Float'(Template.Get ("occupation", 0.1)));

         Total_Land :=
           Community.World.Surface_Area
             * (1.0 - Community.World.Hydrosphere)
           * Community.Occupation
           / 1.0e6;

         declare

            Commodities_Config : constant Tropos.Configuration :=
                                   Template.Child ("commodities");

            procedure Add_Local_Commodity
              (Commodity : Concorde.Commodities.Commodity_Type);

            -------------------------
            -- Add_Local_Commodity --
            -------------------------

            procedure Add_Local_Commodity
              (Commodity : Concorde.Commodities.Commodity_Type)
            is
               use Concorde.Money;
               Config : constant Tropos.Configuration :=
                          Commodities_Config.Child (Commodity.Identifier);
               Local_Price_Factor : constant Float :=
                                      Config.Get ("price", 1.0);
               Local_Price        : constant Price_Type :=
                                      Adjust_Price
                                        (Commodity.Base_Price,
                                         Real (Local_Price_Factor));
               Local              : constant Local_Commodity_Record :=
                                      Local_Commodity_Record'
                                        (Price      => Local_Price,
                                         Base_Price => Local_Price,
                                         Quantity   => Zero,
                                         Supply     => Zero,
                                         Demand     => Zero);
            begin
               Community.Local_Commodities.Replace_Element
                 (Commodity, new Local_Commodity_Record'(Local));
            end Add_Local_Commodity;

         begin
            Concorde.Commodities.Scan (Add_Local_Commodity'Access);
         end;

      end Create;

      -----------------
      -- Create_Pops --
      -----------------

      procedure Create_Pops (Community : in out Root_Community_Type'Class) is
         Pops_Config : constant Tropos.Configuration :=
                         Template.Child ("pops");
      begin
         for Pop_Config of Pops_Config loop
            declare
               Pop : constant Concorde.People.Pops.Pop_Type :=
                       Concorde.People.Pops.Create.New_Pop
                         (Market     => Community.Market,
                          Government => Community.Government,
                          Location   =>
                            Concorde.Locations.In_Community
                              (Db.Reference (Community)),
                          Group      =>
                            (if Concorde.People.Groups.Exists
                               (Pop_Config.Config_Name)
                             then Concorde.People.Groups.Get
                               (Pop_Config.Config_Name)
                             else raise Constraint_Error with
                               "no such pop group '" & Pop_Config.Config_Name
                             & "' in configuration for template '"
                               & Template.Config_Name & "'"),
                          Size       =>
                            Concorde.People.Pops.Pop_Size
                              (Concorde.Random.About
                                   (Real (Float'(Pop_Config.Value)),
                                    0.1)),
                          Apathy     =>
                            Unit_Clamp
                              (Concorde.Random.About
                                   (Real
                                        (Float'
                                             (Template.Get
                                                  ("apathy", 0.5))),
                                    0.1)));
            begin
               Community.Pops.Append (Pop);
               Total_Pop := Total_Pop + Pop.Size_Quantity;
            end;
         end loop;
      end Create_Pops;

      Community   : constant Community_Type :=
                      Db.Create (Create'Access);
   begin

      Community.Update.Government :=
        Concorde.Government.Create.Create_Government
          (Governed          => Community,
           Location          => Concorde.Locations.In_Community (Community),
           Cash              =>
             Concorde.Money.To_Money
               (Concorde.Random.About
                  (Real (Float'(Template.Get ("cash", 10_000.0))),
                   0.1)),
           Owner             => Faction);

      Community.Update.Market :=
        Concorde.Markets.Create_Market
          (Identifier     => World.Identifier & "-market",
           Owner          => Community,
           Manager        => Community.Government,
           Enable_Logging => False);

      Create_Pops (Community.Update);

      for Industry_Config of Template.Child ("industries") loop
         declare
            Config_Size   : constant Non_Negative_Real :=
                              Non_Negative_Real
                                (Float'(Industry_Config.Value));
            Industry_Size : constant Non_Negative_Real :=
                              (if Config_Size < 1.0
                               then Total_Land * Config_Size
                               else Config_Size);
            Start_Cash    : constant Concorde.Money.Money_Type :=
                              Concorde.Money.To_Money
                                (10_000.0);
            Production_Name : constant String :=
                                Industry_Config.Config_Name;
            Production      : constant Concorde.Production.Production_Type :=
                                (if Concorde.Production.Exists
                                   (Production_Name)
                                 then Concorde.Production.Get
                                   (Production_Name)
                                 else raise Constraint_Error
                                   with "no such production "
                                 & Production_Name
                                 & " in industry config");
         begin

            Community.Update.Industries.Append
              (Concorde.Industries.Create.New_Industry
                 (Market     => Community.Market,
                  Government => Community.Government,
                  Owner      =>
                    Community.Get_Pop_By_Group (Production.Owner_Pop_Group),
                  Community  => Community,
                  Production => Production,
                  Size       => Industry_Size,
                  Cash       => Start_Cash));
         end;
      end loop;

      for Corporation_Config of Template.Child ("corporations") loop
         declare
            use Concorde.Corporations;
            use Concorde.Money;

            Config_Size      : constant Non_Negative_Real :=
                                 Non_Negative_Real
                                   (Float'(Corporation_Config.Get ("size")));
            Corporation_Size : constant Quantity_Type :=
                                 To_Quantity (Config_Size);
            Start_Cash       : constant Money_Type :=
                                 To_Money (Config_Size);
            Requirements     : Concorde.Commodities.Virtual_Stock_Type;
            Business : constant Corporation_Business_Type :=
                         Corporation_Business_Type'Value
                           (Corporation_Config.Config_Name);
         begin

            Concorde.Commodities.Configure.Configure_Stock
              (Corporation_Config.Child ("commodities"),
               Requirements);

            Community.Update.Corporations.Append
              (Concorde.Corporations.Create.New_Corporation
                 (Market       => Community.Market,
                  Government   => Community.Government,
                  Owner        => Faction,
                  Community    => Community,
                  Business     => Business,
                  Requirements => Requirements,
                  Size         => Corporation_Size,
                  Cash         => Start_Cash));
         end;
      end loop;

      Concorde.Managers.Communities.Create_Manager (Community).Activate;

      declare
         use type Concorde.Calendar.Time;
      begin
         Concorde.Objects.Queues.Next_Event
           (Community,
            Concorde.Calendar.Clock
            + Duration (Concorde.Random.Unit_Random * 86_400.0));
      end;

      return Community;
   end New_Community;

end Concorde.People.Communities.Create;
