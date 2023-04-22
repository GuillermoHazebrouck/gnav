--//////////////////////////////////////////////////////////////////////////////
-- G-NAV PROJECT
-- Written by Guillermo HAZEBROUCK - gahazebrouck@gmail.com
--\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- This file is part of "G-NAV".
--
-- G-NAV is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- G-NAV is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with G-NAV.  If not, see <https://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

-- Depencencies
--//////////////////////////////////////////////////////////////////////////////

-- Standard
with Ada.Directories;
with Ada.Text_IO;
-- Gnav
with Maps.Terrain;
with Maps.Layers;
with Maps.Layers.Airspaces;
with Maps.Reference;
with Utility;
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Loader is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The path to the file containing the default dataset name
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Default_File : constant String := Utility.Base_Directory & "maps/default.dat";



   --===========================================================================
   -- Returns the path to the given dataset name
   --===========================================================================
   function Get_Path (Name : String) return String is
   begin

      return Utility.Base_Directory & "maps/" & Name & "/";

   end Get_Path;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Loads a given dataset
   --===========================================================================
   procedure Load_Dataset (Name : String) is

      use Ada.Directories;
      use Ada.Text_IO;

      File_Id : File_Type;
      Folder  : String := Get_Path (Name);

   begin

      Utility.Log.Put_Message ("loading map data for " & Name & " in " & Folder);

      if Exists (Folder) then

         -- Set new as default
         -------------------------

         Dataset_Name := +Name;

         Dataset_Path := +Folder;

         Open (File_Id, Out_File, Default_File);

         Put_Line (File_Id, Name);

         Close (File_Id);

      else

         Utility.Log.Put_Message ("warning: dataset not found");

         -- Clear but keep default
         --------------------------

         Dataset_Name := Empty_String;

         Dataset_Path := Empty_String;

      end if;

      Maps.Terrain.Load_Grid_File;

      Maps.Layers.Load_Shape_Files;

      Maps.Layers.Airspaces.Generate_Airspaces;

      Maps.Reference.Load_Reference_File;

   exception

      when others =>

         Utility.Log.Put_Message ("problem while loading dataset " & Name);

         Close (File_Id);

   end Load_Dataset;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the list of datasets in memory
   --===========================================================================
   function Get_Datasets return Dataset_Array is
   begin

      return (others => Empty_String);

   end Get_Datasets;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Removes the given dataset
   --===========================================================================
   procedure Delete_Dataset (Name : Dynamic_String) is
   begin

      null;

   end Delete_Dataset;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Default_Dataset is

      use Ada.Directories;
      use Ada.Text_IO;

      File_Id : File_Type;

   begin

      if Exists (Default_File) then

         -- Try loading the default dataset
         --------------------------------------------------------------

         Open (File_Id, In_File, Default_File);

         declare
            Name : String := Trim (Get_Line (File_Id));
         begin

            Close (File_Id);

            Load_Dataset (Name);

         end;

      end if;

   exception

      when others =>

         Close (File_Id);

   end Load_Default_Dataset;
   -----------------------------------------------------------------------------

end Maps.Loader;
--------------------------------------------------------------------------------
