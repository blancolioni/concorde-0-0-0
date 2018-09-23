with Concorde.Ministries;

package Concorde.Powers.Ministries is

   function Appoint_Minister return Power_Type;

   function Direct_Minister
     (Ministry : not null access constant
        Concorde.Ministries.Root_Ministry_Type'Class)
      return Power_Type;

   function Law_Enforcement return Power_Type;

   function Pay
     (Group : Concorde.People.Groups.Pop_Group)
      return Power_Type;

   function Is_Direct_Minister (Power : Power_Type) return Boolean;
   function Ministry (Power : Power_Type)
                      return not null access constant
     Concorde.Ministries.Root_Ministry_Type'Class
       with Pre => Is_Direct_Minister (Power);

end Concorde.Powers.Ministries;
