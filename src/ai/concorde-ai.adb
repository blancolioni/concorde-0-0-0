package body Concorde.AI is

   -----------
   -- Awake --
   -----------

   function Awake (AI : Root_AI_Type) return Boolean is
   begin
      return AI.Awake;
   end Awake;

   ----------------------------
   -- Minimum_Defense_Factor --
   ----------------------------

   function Minimum_Defense_Factor
     (AI : Root_AI_Type)
      return Non_Negative_Real
   is
   begin
      return AI.Current_Defense_Factor;
   end Minimum_Defense_Factor;

   --------------------------
   -- Update_Defensiveness --
   --------------------------

   procedure Update_Attack_Factor
     (AI           : in out Root_AI_Type'Class;
      Faction       : in out Concorde.Factions.Root_Faction_Type'Class;
      Enemy        : Concorde.Factions.Faction_Type;
      Can_Increase : Boolean := True;
      Can_Decrease : Boolean := True)
   is
      AF : Non_Negative_Real;
   begin
      if Faction.Current_Ships > Enemy.Current_Ships then
         AF := 1.2;
      elsif Faction.Current_Ships = 0
        or else Faction.Current_Ships * 4 < Enemy.Current_Ships
      then
         AF := 4.0;
      else
         AF :=
           Real (Enemy.Current_Ships) * 1.3
           / Real (Faction.Current_Ships);
      end if;

      if (AF < AI.Current_Attack_Factor and then Can_Decrease)
        or else (AF > AI.Current_Attack_Factor and then Can_Increase)
      then
         AI.Current_Attack_Factor := (AI.Current_Attack_Factor + AF) / 2.0;
      end if;

   end Update_Attack_Factor;

   ----------
   -- Wake --
   ----------

   procedure Wake (AI : in out Root_AI_Type) is
   begin
      AI.Awake := True;
   end Wake;

end Concorde.AI;
