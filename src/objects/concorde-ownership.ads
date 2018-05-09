package Concorde.Ownership is

   type Owner_Interface is limited interface;

   type Owned_Interface is limited interface;

   function Owner (Owned : Owned_Interface)
                   return access constant Owner_Interface'Class
                   is abstract;

   function Owned_By (Owned : Owned_Interface'Class;
                      Owner : not null access constant
                        Owner_Interface'Class)
                      return Boolean;

   function Owns (Owner : not null access constant Owner_Interface'Class;
                  Owned : not null access constant
                    Owned_Interface'Class)
                  return Boolean;

private

   function Owned_By (Owned : Owned_Interface'Class;
                      Owner : not null access constant
                        Owner_Interface'Class)
                      return Boolean
   is (Owned.Owner = Owner);

   function Owns (Owner : not null access constant Owner_Interface'Class;
                  Owned : not null access constant
                    Owned_Interface'Class)
                  return Boolean
   is (Owned.Owner = Owner);

end Concorde.Ownership;
