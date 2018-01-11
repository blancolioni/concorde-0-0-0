with Ada.Characters.Handling;

package body Concorde.People.Individuals is

   ---------
   -- Age --
   ---------

   function Age
     (Individual : Root_Individual_Type'Class)
      return Natural
   is
      use Concorde.Calendar;
      Seconds : constant Duration := Clock - Individual.Birth;
      Days    : constant Non_Negative_Real := Real (Seconds) / 86_600.0;
      Years   : constant Non_Negative_Real := Days / 360.0;
   begin
      return Natural (Real'Truncation (Years));
   end Age;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name (Individual : Root_Individual_Type'Class)
                       return String
   is
   begin
      return Ada.Strings.Unbounded.To_String
        (Individual.First_Name)
        & " "
        & Ada.Strings.Unbounded.To_String
        (Individual.Last_Name);
   end Full_Name;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Individual : in out Root_Individual_Type;
      New_Name   : String)
   is
      Name : String := New_Name;
      Cap  : Boolean := True;
   begin
      for Ch of Name loop
         if Cap then
            Ch := Ada.Characters.Handling.To_Upper (Ch);
            Cap := False;
         elsif Ch = '-' then
            Cap := True;
         else
            Ch := Ada.Characters.Handling.To_Lower (Ch);
         end if;
      end loop;
      Individual.First_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Individual_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Concorde.People.Individuals;
