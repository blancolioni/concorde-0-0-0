package body Concorde.Protected_Lists is

   --------------------
   -- Add_If_Missing --
   --------------------

   procedure Add_If_Missing
     (Container : in out List;
      Item      : in     Element_Type;
      Added     :    out Boolean)
   is
   begin
      Container.Lock.Exclusive;
      Added := False;
      if not Container.Inner_List.Contains (Item) then
         Container.Inner_List.Append (Item);
         Added := True;
      end if;
      Container.Lock.Unlock;
   end Add_If_Missing;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : in out List;
      Item      : Element_Type)
      return Boolean
   is
      Result : Boolean;
   begin
      Container.Lock.Shared;
      begin
         Result := Container.Inner_List.Contains (Item);
      end;
      Container.Lock.Unlock;
      return Result;
   end Contains;

   -----------------------
   -- Delete_If_Present --
   -----------------------

   procedure Delete_If_Present
     (Container : in out List;
      Item      : in     Element_Type;
      Deleted   :    out Boolean)
   is
      Position : List_Of_Elements.Cursor;
   begin
      Container.Lock.Exclusive;
      Deleted := False;
      Position := Container.Inner_List.Find (Item);
      if List_Of_Elements.Has_Element (Position) then
         Container.Inner_List.Delete (Position);
         Deleted := True;
      end if;
      Container.Lock.Unlock;
   end Delete_If_Present;

   ---------------------
   -- Delete_Matching --
   ---------------------

   procedure Delete_Matching
     (Container : in out List;
      Match     : not null access
        function (Item : Element_Type) return Boolean)
   is
      use List_Of_Elements;
      Position : Cursor;
   begin
      Container.Lock.Exclusive;
      Position := Container.Inner_List.First;
      while Has_Element (Position) loop
         declare
            Item : constant Element_Type := Element (Position);
         begin
            if Match (Item) then
               Container.Inner_List.Delete (Position);
            else
               Next (Position);
            end if;
         end;
      end loop;
      Container.Lock.Unlock;
   end Delete_Matching;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : in out List;
      Process   : not null access
        procedure (Element : Element_Type))
   is
      use List_Of_Elements;
      Position : Cursor;
   begin
      Container.Lock.Exclusive;
      Position := Container.Inner_List.First;
      while Has_Element (Position) loop
         declare
            Item : constant Element_Type := Element (Position);
         begin
            Process (Item);
            Next (Position);
         end;
      end loop;
      Container.Lock.Unlock;
   end Iterate;

   ---------------
   -- List_Lock --
   ---------------

   protected body List_Lock is

      ------------
      -- Shared --
      ------------

      entry Shared when not Exclusive_Locked is
      begin
         Shared_Lock_Count := Shared_Lock_Count + 1;
      end Shared;

      ---------------
      -- Exclusive --
      ---------------

      entry Exclusive when
        Shared_Lock_Count = 0
        and then not Exclusive_Locked
      is
      begin
         Exclusive_Locked := True;
      end Exclusive;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         if Exclusive_Locked then
            Exclusive_Locked := False;
         elsif Shared_Lock_Count > 0 then
            Shared_Lock_Count := Shared_Lock_Count - 1;
         else
            raise Constraint_Error with
              "unlocked called on unlocked focus";
         end if;
      end Unlock;

   end List_Lock;

end Concorde.Protected_Lists;
