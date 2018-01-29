with Ada.Containers.Indefinite_Holders;

with Concorde.Factions;

package body Concorde.Laws.Bureaucracy is

   package Power_Holder is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Powers.Power_Type, Concorde.Powers."=");

   type Power_Delegation_Law is
     new Root_Law_Type with
      record
         Power : Power_Holder.Holder;
      end record;

   overriding function Can_Enact (Law : Power_Delegation_Law) return Boolean;
   overriding procedure Enact (Law : in out Power_Delegation_Law);
   overriding procedure Repeal (Law : in out Power_Delegation_Law) is null;
   overriding function Show (Law : Power_Delegation_Law) return String;

   ---------------
   -- Can_Enact --
   ---------------

   overriding function Can_Enact (Law : Power_Delegation_Law) return Boolean is
   begin
      return Concorde.Factions.Faction_Type (Law.Context.Legislator)
        .Has_Power (Law.Power.Element);
   end Can_Enact;

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
         Power         => Power_Holder.To_Holder (Power));
   end Delegate_Power;

   -----------
   -- Enact --
   -----------

   overriding procedure Enact (Law : in out Power_Delegation_Law) is
   begin
      Concorde.Factions.Faction_Type (Law.Context.Target)
        .Update.Add_Power (Law.Power.Element);
   end Enact;

   ----------
   -- Show --
   ----------

   overriding function Show (Law : Power_Delegation_Law) return String is
   begin
      return "delegate " & Concorde.Powers.Show (Law.Power.Element)
        & " to " & Law.Context.Target.Identifier;
   end Show;

end Concorde.Laws.Bureaucracy;
