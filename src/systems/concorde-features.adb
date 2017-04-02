with Concorde.Features.Db;

package body Concorde.Features is

   Local_Ice    : Feature_Type := null;
   Local_Desert : Feature_Type := null;

   procedure Scan_Standard_Features;

   ------------
   -- Colour --
   ------------

   function Colour
     (Feature : Root_Feature_Type'Class)
      return Lui.Colours.Colour_Type
   is
   begin
      return Feature.Colour;
   end Colour;

   ------------
   -- Desert --
   ------------

   function Desert return Feature_Type is
   begin
      if Local_Desert = null then
         Scan_Standard_Features;
      end if;
      return Local_Desert;
   end Desert;

   ---------
   -- Get --
   ---------

   function Get (Id : String) return Feature_Type is
   begin
      return Db.Get (Id);
   end Get;

   ---------
   -- Ice --
   ---------

   function Ice return Feature_Type is
   begin
      if Local_Ice = null then
         Scan_Standard_Features;
      end if;
      return Local_Ice;
   end Ice;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Feature : Root_Feature_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Feature);
   begin
      return Db.Get_Database;
   end Object_Database;

   ----------------------------
   -- Scan_Standard_Features --
   ----------------------------

   procedure Scan_Standard_Features is

      procedure Process (Feature : Feature_Type);

      -------------
      -- Process --
      -------------

      procedure Process (Feature : Feature_Type) is
      begin
         if Feature.Is_Ice then
            Local_Ice := Feature;
         elsif Feature.Is_Desert then
            Local_Desert := Feature;
         end if;
      end Process;

   begin
      Db.Scan (Process'Access);
   end Scan_Standard_Features;

end Concorde.Features;
