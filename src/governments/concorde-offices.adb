package body Concorde.Offices is

   -------------------
   -- Effectiveness --
   -------------------

   function Effectiveness
     (Office    : Root_Office_Type'Class;
      Portfolio : Portfolio_Size_Range;
      Holder    : Concorde.People.Attributes.Has_Attributes'Class)
      return Unit_Real
   is
   begin
      return Office.Effect.Effectiveness (Natural (Portfolio), Holder);
   end Effectiveness;

   ---------
   -- Get --
   ---------

   function Get
     (Responsibility : Responsibility_Type)
      return Office_Type
   is
   begin
      for Office of Office_Vector loop
         if Office.Has_Responsibility (Responsibility) then
            return Office;
         end if;
      end loop;

      raise Constraint_Error with
        "no such office for responsibility: "
        & Responsibility_Type'Image (Responsibility);
   end Get;

   ------------------
   -- Scan_Offices --
   ------------------

   procedure Scan_Offices
     (Process : not null access
        procedure (Office : Office_Type))
   is
   begin
      for Office of Office_Vector loop
         Process (Office);
      end loop;
   end Scan_Offices;

end Concorde.Offices;
