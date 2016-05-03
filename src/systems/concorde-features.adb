with Concorde.Features.Db;

package body Concorde.Features is

   Local_Ice : Feature_Type := null;

   procedure Scan_Standard_Features;

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
      return Memor.Root_Database_Type'Class
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
         end if;
      end Process;

   begin
      Db.Scan (Process'Access);
   end Scan_Standard_Features;

end Concorde.Features;
