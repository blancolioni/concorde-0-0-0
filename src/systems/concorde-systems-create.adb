with Ada.Text_IO;

--  with WL.Random;

with Concorde.Random;
with Concorde.Scenarios;

with Concorde.Commodities;
with Concorde.Stars.Create;
with Concorde.Worlds.Create;
with Concorde.Worlds.Lists;

with Concorde.Systems.Db;

package body Concorde.Systems.Create is

   type System_Create_Job is
     new WL.Work.Root_Job_Type with
      record
         System : Star_System_Type;
      end record;

   overriding procedure Execute
     (Job : in out System_Create_Job);

   function Random_Star_Mass return Non_Negative_Real;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Job : in out System_Create_Job)
   is
      List   : Concorde.Worlds.Lists.List;

   begin

      Concorde.Worlds.Create.Create_Worlds
        (Job.System,
         Concorde.Stars.Star_Type (Job.System.Main_Object),
         List);

      for World of List loop
         declare
            procedure Add_Object
              (To_System : in out Root_Star_System_Type'Class);

            ----------------
            -- Add_Object --
            ----------------

            procedure Add_Object
              (To_System : in out Root_Star_System_Type'Class)
            is
            begin
               To_System.Add_Object
                 (World,
                  Concorde.Geometry.Degrees_To_Radians
                    (Concorde.Random.Unit_Random * 360.0));
            end Add_Object;

         begin
            Db.Update (Job.System.Reference, Add_Object'Access);
         end;

      end loop;

      Ada.Text_IO.Put_Line
        (Job.System.Name & ":"
         & Ada.Containers.Count_Type'Image
           (List.Length) & " planets");

   end Execute;

   ----------------
   -- New_System --
   ----------------

   function New_System
     (Index       : Positive;
      Name        : String;
      Work_Handle : WL.Work.Work_Handle;
      X, Y        : Real;
      Boundary    : System_Influence_Boundary;
      Production  : Non_Negative_Real;
      Capacity    : Non_Negative_Real)
      return Star_System_Type
   is
      use Concorde.Commodities;

--        Resources : constant Concorde.Commodities.Array_Of_Commodities :=
--                      Concorde.Commodities.Get
--                        (Concorde.Commodities.Organic);

      Imperial_Centre : constant Boolean :=
                          Concorde.Scenarios.Imperial_Centre
                              and then Index = 1;

      procedure Create (System : in out Root_Star_System_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (System : in out Root_Star_System_Type'Class) is
--           Resource : constant Concorde.Commodities.Commodity_Type :=
--                        Resources
--                          (WL.Random.Random_Number
--                             (Resources'First, Resources'Last));
      begin
         System.Set_Name (Name);
         System.Index := Index;
         System.X := X;
         System.Y := Y;
         System.Production := Production;
         System.Capacity := Capacity;
         System.Boundary :=
           new System_Influence_Boundary'(Boundary);

         declare
            Main_Star : constant Concorde.Stars.Star_Type :=
                          Concorde.Stars.Create.New_Main_Sequence_Star
                            (Name, Concorde.Systems.Db.Reference (System),
                             (if True or else Imperial_Centre
                              then 1.0
                              else Random_Star_Mass));
         begin
            System.Add_Object
              (Object   => Main_Star,
               Position => Concorde.Geometry.Degrees_To_Radians (0.0));
         end;

      end Create;

      System : constant Star_System_Type :=
                 Concorde.Systems.Db.Create (Create'Access);

      Job    : constant WL.Work.Job_Type :=
                 new System_Create_Job'
                   (WL.Work.Root_Job_Type with System => System);

   begin

      WL.Work.Add_Job (Work_Handle, Job);

      return System;

   end New_System;

   ----------------------
   -- Random_Star_Mass --
   ----------------------

   function Random_Star_Mass return Non_Negative_Real is
      Seed : constant Unit_Real := Concorde.Random.Unit_Random;
      Solar_Mass_Count : Real;
   begin
      if Seed <= 0.99 then
         Solar_Mass_Count :=
           0.1 + 6.0 * Seed - 15.0 * Seed ** 2
             + 11.0 * Seed ** 3;
      else
         declare
            X : constant Real := (Seed - 0.99) * 1.0E4;
            A : constant Real := 0.110833;
            B : constant Real := -14.0358;
            C : constant Real := 445.25;
         begin
            Solar_Mass_Count := A * X ** 2 + B * X + C;
         end;
      end if;
      return Solar_Mass_Count;
   end Random_Star_Mass;

end Concorde.Systems.Create;
