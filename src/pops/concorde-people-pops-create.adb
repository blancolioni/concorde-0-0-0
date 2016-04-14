with Concorde.People.Pops.Db;

package body Concorde.People.Pops.Create is

   -------------
   -- New_Pop --
   -------------

   function New_Pop
     (Wealth_Group : Concorde.People.Groups.Pop_Group;
      Skill        : Concorde.People.Skills.Pop_Skill;
      Size         : Pop_Size;
      Cash         : Concorde.Money.Money_Type)
      return Pop_Type
   is
      procedure Create (Pop : in out Root_Pop_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Pop : in out Root_Pop_Type'Class) is
      begin
         Pop.Groups.Replace_Element (Wealth_Group.Reference, 1.0);
         Pop.Skills.Append (Skill);
         Pop.Size := Size;
         Pop.Set_Cash (Cash);
      end Create;

   begin
      return Db.Create (Create'Access);
   end New_Pop;

end Concorde.People.Pops.Create;
