with WL.Localisation;

package body Concorde.Powers.Ministries is

   type Appoint_Minister_Power is
     new Root_Power_Type with null record;

   overriding function Class_Identifier
     (Power : Appoint_Minister_Power)
      return String
   is ("appoint_minister");

   type Direct_Minister_Power is
     new Root_Power_Type with
      record
         Ministry : Concorde.Ministries.Ministry_Type;
      end record;

   overriding function Class_Identifier
     (Power : Direct_Minister_Power)
      return String
   is ("direct_minister");

   overriding function Identifier
     (Power : Direct_Minister_Power)
      return String
   is ("direct_minister_" & Power.Ministry.Identifier);

   overriding function Show
     (Power : Direct_Minister_Power)
      return String
   is (WL.Localisation.Local_Text
       ("direct_minister", Power.Ministry.Name));

   type Law_Enforcement_Power is
     new Root_Power_Type with null record;

   overriding function Class_Identifier
     (Power : Law_Enforcement_Power)
      return String
   is ("law_enforcement");

   ----------------------
   -- Appoint_Minister --
   ----------------------

   function Appoint_Minister return Power_Type is
   begin
      return Power : Appoint_Minister_Power;
   end Appoint_Minister;

   ---------------------
   -- Direct_Minister --
   ---------------------

   function Direct_Minister
     (Ministry : not null access constant
        Concorde.Ministries.Root_Ministry_Type'Class)
      return Power_Type
   is
   begin
      return Direct_Minister_Power'
        (Ministry => Concorde.Ministries.Ministry_Type (Ministry));
   end Direct_Minister;

   ------------------------
   -- Is_Direct_Minister --
   ------------------------

   function Is_Direct_Minister (Power : Power_Type) return Boolean is
   begin
      return Power in Direct_Minister_Power'Class;
   end Is_Direct_Minister;

   ---------------------
   -- Law_Enforcement --
   ---------------------

   function Law_Enforcement return Power_Type is
   begin
      return Power : Law_Enforcement_Power;
   end Law_Enforcement;

   --------------
   -- Ministry --
   --------------

   function Ministry
     (Power : Power_Type)
      return not null access constant
     Concorde.Ministries.Root_Ministry_Type'Class
   is
   begin
      return Direct_Minister_Power'Class (Power).Ministry;
   end Ministry;

end Concorde.Powers.Ministries;
