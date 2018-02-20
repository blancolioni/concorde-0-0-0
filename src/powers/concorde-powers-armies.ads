with Concorde.Armies;

package Concorde.Powers.Armies is

   function Appoint_General return Power_Type;

   function Command_Army
     (Army : not null access constant
        Concorde.Armies.Root_Army_Type'Class)
      return Power_Type;

end Concorde.Powers.Armies;
