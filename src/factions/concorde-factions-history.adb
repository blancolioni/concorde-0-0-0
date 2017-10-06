with Ada.Containers.Ordered_Maps;

with Memor.Element_Vectors;

package body Concorde.Factions.History is

   use Concorde.Calendar;

   type Metric_Record is
     array (Historical_Metric) of Real;

   package History_Record_Vectors is
     new Memor.Element_Vectors
       (Root_Faction_Type, Metric_Record, (others => 0.0));

   package History_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Concorde.Calendar.Time,
        Element_Type => History_Record_Vectors.Vector,
        "<"          => Concorde.Calendar."<",
        "="          => History_Record_Vectors."=");

   History : History_Maps.Map;

   ----------------
   -- Get_Metric --
   ----------------

   function Get_Metric
     (Date   : Concorde.Calendar.Time;
      Metric : Historical_Metric;
      Faction : Faction_Type)
      return Real
   is
      Info : Metric_Record renames
               History.Element (Date).Element (Faction);
   begin
      return Info (Metric);
   end Get_Metric;

   --------------------
   -- Update_History --
   --------------------

   procedure Update_History is

      R : History_Record_Vectors.Vector;

      procedure Update (Faction : Root_Faction_Type'Class);

      ------------
      -- Update --
      ------------

      procedure Update (Faction : Root_Faction_Type'Class) is
         M : constant Metric_Record :=
               (Controlled_Systems => Real (Faction.Current_Systems),
                Ship_Count         => Real (Faction.Current_Ships),
                Production         => 0.0);
      begin
         R.Replace_Element (Faction, M);
      end Update;

   begin
      Db.Scan (Update'Access);
      History.Insert (Clock - Seconds (Clock), R);

   end Update_History;

end Concorde.Factions.History;
