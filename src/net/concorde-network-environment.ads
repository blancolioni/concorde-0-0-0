package Concorde.Network.Environment is

   type Environment is interface;

   function Value
     (Env : Environment;
      Name : String)
      return Real
      is abstract;

end Concorde.Network.Environment;
