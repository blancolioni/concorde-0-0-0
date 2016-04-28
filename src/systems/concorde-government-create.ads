with Concorde.Money;

package Concorde.Government.Create is

   function Create_Government
     (Governed          : not null access constant Governed_Interface'Class;
      Cash              : Concorde.Money.Money_Type;
      Owner             : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Headquarters      : not null access constant
        Concorde.Installations.Root_Installation_Type'Class;
      Basic_Living_Wage : Boolean)
      return Government_Type;

end Concorde.Government.Create;
