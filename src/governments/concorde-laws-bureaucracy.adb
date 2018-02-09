with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Unbounded;

with Concorde.Bureaucracy;
with Concorde.Factions;

with Concorde.Ministries.Create;

package body Concorde.Laws.Bureaucracy is

   package Power_Holder is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Powers.Power_Type, Concorde.Powers."=");

   type Power_Delegation_Law is
     new Root_Law_Type with
      record
         Copy  : Boolean;
         Power : Power_Holder.Holder;
      end record;

   overriding function Can_Enact (Law : Power_Delegation_Law) return Boolean;
   overriding procedure Enact (Law : in out Power_Delegation_Law);
   overriding procedure Repeal (Law : in out Power_Delegation_Law) is null;
   overriding function Show (Law : Power_Delegation_Law) return String;

   type Create_Ministry_Law is
     new Root_Law_Type with
      record
         Name     : Ada.Strings.Unbounded.Unbounded_String;
         Location : Concorde.Installations.Installation_Type;
         Powers   : Concorde.Powers.Power_Set;
      end record;

   overriding function Can_Enact (Law : Create_Ministry_Law) return Boolean;
   overriding procedure Enact (Law : in out Create_Ministry_Law);
   overriding procedure Repeal (Law : in out Create_Ministry_Law) is null;
   overriding function Show (Law : Create_Ministry_Law) return String;

   ---------------
   -- Can_Enact --
   ---------------

   overriding function Can_Enact (Law : Power_Delegation_Law) return Boolean is
   begin
      return Concorde.Bureaucracy.Bureaucracy_Type (Law.Context.Legislator)
        .Has_Power (Law.Power.Element);
   end Can_Enact;

   ---------------
   -- Can_Enact --
   ---------------

   overriding function Can_Enact (Law : Create_Ministry_Law) return Boolean is

      function Enactable
        (Power : Concorde.Powers.Power_Type)
         return Boolean
      is (Concorde.Bureaucracy.Bureaucracy_Type (Law.Context.Legislator)
          .Has_Power (Power));

   begin
      return Law.Powers.Check_Powers (Enactable'Access);
   end Can_Enact;

   ---------------------
   -- Create_Ministry --
   ---------------------

   function Create_Ministry
     (Context  : Law_Context;
      Name     : String;
      Location : Concorde.Installations.Installation_Type;
      Powers   : Concorde.Powers.Power_Set)
      return Law_Type
   is
   begin
      return new Create_Ministry_Law'
        (Level    => Legislation,
         Context  => Context,
         Name     => Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Location => Location,
         Powers   => Powers);
   end Create_Ministry;

   ------------------
   -- Create_Power --
   ------------------

   function Create_Power
     (Context   : Law_Context;
      Power     : Concorde.Powers.Power_Type)
      return Law_Type
   is
   begin
      return new Power_Delegation_Law'
        (Level         => Legislation,
         Context       => Context,
         Copy          => True,
         Power         => Power_Holder.To_Holder (Power));
   end Create_Power;

   --------------------
   -- Delegate_Power --
   --------------------

   function Delegate_Power
     (Context   : Law_Context;
      Power     : Concorde.Powers.Power_Type)
      return Law_Type
   is
   begin
      return new Power_Delegation_Law'
        (Level         => Legislation,
         Context       => Context,
         Copy          => False,
         Power         => Power_Holder.To_Holder (Power));
   end Delegate_Power;

   -----------
   -- Enact --
   -----------

   overriding procedure Enact (Law : in out Power_Delegation_Law) is
   begin
      if Law.Copy then
         Concorde.Bureaucracy.Bureaucracy_Type (Law.Context.Target)
           .Variable_Reference.Add_Power (Law.Power.Element);
      else
         declare
            use Concorde.Bureaucracy;
            From : constant Bureaucracy_Type :=
                     Bureaucracy_Type (Law.Context.Legislator);
            To   : constant Bureaucracy_Type :=
                     Bureaucracy_Type (Law.Context.Target);
         begin
            From.Variable_Reference.Remove_Power (Law.Power.Element);
            From.Variable_Reference.Delegate_Power (Law.Power.Element, To);
            To.Variable_Reference.Add_Power (Law.Power.Element);
         end;
      end if;
   end Enact;

   -----------
   -- Enact --
   -----------

   overriding procedure Enact (Law : in out Create_Ministry_Law) is
      Faction : constant Concorde.Factions.Faction_Type :=
                  Concorde.Factions.Faction_Type (Law.Context.Legislator);
   begin
      Concorde.Ministries.Create.Create_Ministry
        (Faction  => Faction,
         Area     => Law.Context.Target,
         Location => Law.Location,
         Market   => Law.Location.Market,
         Name     => Ada.Strings.Unbounded.To_String (Law.Name),
         Powers   => Law.Powers);
   end Enact;

   ----------
   -- Show --
   ----------

   overriding function Show (Law : Power_Delegation_Law) return String is
   begin
      return "delegate " & Concorde.Powers.Show (Law.Power.Element)
        & " to " & Law.Context.Target.Identifier;
   end Show;

   ----------
   -- Show --
   ----------

   overriding function Show (Law : Create_Ministry_Law) return String is
      use Ada.Strings.Unbounded;

      Powers : Unbounded_String;

      procedure Add_Power (Power : Concorde.Powers.Power_Type);

      ---------------
      -- Add_Power --
      ---------------

      procedure Add_Power (Power : Concorde.Powers.Power_Type) is
      begin
         if Powers = "" then
            Powers := To_Unbounded_String (Concorde.Powers.Show (Power));
         else
            Powers := Powers & ", " & Concorde.Powers.Show (Power);
         end if;
      end Add_Power;

   begin

      Law.Powers.Scan_Powers (Add_Power'Access);
      return "create " & To_String (Law.Name) & " with the following powers: "
        & To_String (Powers);
   end Show;

end Concorde.Laws.Bureaucracy;
