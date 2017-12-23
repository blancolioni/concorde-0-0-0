package body Concorde.People.Groups is

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Pop_Group is
   begin
      if Db.Exists (Name) then
         return Db.Get (Name);
      elsif Db.Exists (Name & "s") then
         return Db.Get (Name & "s");
      elsif Name (Name'Last - 2 .. Name'Last) = "man"
        and then Db.Exists (Name (Name'First .. Name'Last - 3) & "men")
      then
         return Db.Get (Name (Name'First .. Name'Last - 2) & "men");
      else
         raise Constraint_Error with
           "no such pop group: " & Name;
      end if;
   end Get;

   ----------------
   -- Scan_Needs --
   ----------------

   procedure Scan_Needs
     (Group   : Root_Pop_Group'Class;
      Level   : Need_Level;
      Size    : WL.Quantities.Quantity_Type;
      Process : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Type;
                   Quantity : WL.Quantities.Quantity_Type))
   is
      use WL.Quantities;
   begin
      for Needs of Group.Needs (Level) loop
         Process (Needs.Commodity,
                  Needs.Need_Quantity * Size / Needs.Pop_Quantity);
      end loop;
   end Scan_Needs;

end Concorde.People.Groups;
