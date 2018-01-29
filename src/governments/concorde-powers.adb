package body Concorde.Powers is

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Container : Power_Set;
      Power     : Power_Type)
      return Boolean
   is
   begin
      return Power_Lists.Has_Element (Container.Set.Find (Power));
   end Contains;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Container : in out Power_Set;
      Power     : Power_Type)
   is
   begin
      if not Container.Contains (Power) then
         Container.Set.Append (Power);
      end if;
   end Insert;

   ------------
   -- Remove --
   ------------

   overriding procedure Remove
     (Container : in out Power_Set;
      Power     : Power_Type)
   is
      use Power_Lists;
      Position : Cursor := Container.Set.Find (Power);
   begin
      if Has_Element (Position) then
         Container.Set.Delete (Position);
      end if;
   end Remove;

end Concorde.Powers;
