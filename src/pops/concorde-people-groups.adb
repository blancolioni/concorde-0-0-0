package body Concorde.People.Groups is

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Pop_Group is
   begin
      return Db.Get (Name);
   end Get;

   -------------------------
   -- Initial_Cash_Factor --
   -------------------------

   function Initial_Cash_Factor
     (Group : Root_Pop_Group'Class)
      return Natural
   is
   begin
      return Group.Initial_Cash_Factor;
   end Initial_Cash_Factor;

   ------------------
   -- Middle_Class --
   ------------------

   function Middle_Class return Pop_Group is
   begin
      return Get ("middle_class");
   end Middle_Class;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Pop_Group)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   ----------
   -- Poor --
   ----------

   function Poor return Pop_Group is
   begin
      return Get ("poor");
   end Poor;

   -----------------------
   -- Preferred_Quality --
   -----------------------

   function Preferred_Quality
     (Group : Root_Pop_Group'Class)
      return Concorde.Commodities.Commodity_Quality
   is
   begin
      return Group.Preferred_Quality;
   end Preferred_Quality;

   ----------
   -- Rich --
   ----------

   function Rich return Pop_Group is
   begin
      return Get ("rich");
   end Rich;

   ----------------
   -- Scan_Needs --
   ----------------

   procedure Scan_Needs
     (Group   : Root_Pop_Group'Class;
      Process : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Type;
                   Need      : Non_Negative_Real))
   is
   begin
      for Need of Group.Needs loop
         Process (Need.Commodity, Need.Need);
      end loop;
   end Scan_Needs;

   ---------------------------
   -- Set_Affiliation_Range --
   ---------------------------

   procedure Set_Affiliation_Range
     (Vector : in out Affiliation_Vector'Class;
      Group  : Pop_Group;
      Value  : Affiliation_Range)
   is
   begin
      Vector.Replace_Element (Group, Value);
   end Set_Affiliation_Range;

   ------------------
   -- Wealth_Group --
   ------------------

   function Wealth_Group
     (Affiliator : Affiliation_Interface'Class)
      return Pop_Group
   is
   begin
      if Affiliator.Poor then
         return Poor;
      elsif Affiliator.Middle_Class then
         return Middle_Class;
      elsif Affiliator.Rich then
         return Rich;
      else
         raise Constraint_Error with
           "affiliator has no wealth group";
      end if;
   end Wealth_Group;

end Concorde.People.Groups;
