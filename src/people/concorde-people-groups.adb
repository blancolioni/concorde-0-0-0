package body Concorde.People.Groups is

   function Get_Groups
     (Test : not null access
        function (Group : Pop_Group) return Boolean)
      return Array_Of_Pop_Groups;

   ----------------
   -- All_Groups --
   ----------------

   function All_Groups return Array_Of_Pop_Groups is
      function Always (Group : Pop_Group) return Boolean
      is (Group /= null);
   begin
      return Get_Groups (Always'Access);
   end All_Groups;

   ---------------
   -- Everybody --
   ---------------

   function Everybody return Pop_Group is
   begin
      return Get ("everybody");
   end Everybody;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Pop_Group is
   begin
      if Db.Exists (Name) then
         return Db.Get (Name);
      else
         raise Constraint_Error with
           "no such pop group: " & Name;
      end if;
   end Get;

   function Get_Groups
     (Test : not null access
        function (Group : Pop_Group) return Boolean)
      return Array_Of_Pop_Groups
   is
      Result : Array_Of_Pop_Groups (1 .. Vector.Last_Index);
      Count  : Natural := 0;
   begin
      for Group of Vector loop
         if Test (Group) then
            Count := Count + 1;
            Result (Count) := Group;
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Groups;

   ---------------
   -- Influence --
   ---------------

   function Influence
     (Group : Root_Pop_Group'Class;
      Other : Pop_Group)
      return Signed_Unit_Real
   is
   begin
      for Item of Group.Influences loop
         if Item.Group.Identifier = Other.Identifier then
            return Item.Influence;
         end if;
      end loop;
      return 0.0;
   end Influence;

   ----------------------
   -- Political_Groups --
   ----------------------

   function Political_Groups return Array_Of_Pop_Groups is
      function Is_Political (Group : Pop_Group) return Boolean
      is (Group.Political_Wing);
   begin
      return Get_Groups (Is_Political'Access);
   end Political_Groups;

   -------------------
   -- Wealth_Groups --
   -------------------

   function Wealth_Groups return Array_Of_Pop_Groups is
      function Is_Wealth (Group : Pop_Group) return Boolean
      is (Group.Wealth_Group);
   begin
      return Get_Groups (Is_Wealth'Access);
   end Wealth_Groups;

end Concorde.People.Groups;
