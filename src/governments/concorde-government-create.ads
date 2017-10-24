with WL.Money;

package Concorde.Government.Create is

   function Create_Government
     (Governed          : not null access constant Governed_Interface'Class;
      Cash              : WL.Money.Money_Type;
      Owner             : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Headquarters      : not null access constant
        Concorde.Installations.Root_Installation_Type'Class;
      Basic_Living_Wage : WL.Money.Price_Type)
      return Government_Type;

end Concorde.Government.Create;
