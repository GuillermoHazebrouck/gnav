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

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Loader is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The path to the file containing the default dataset name
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Default_File : String := "maps/default.dat";




   --===========================================================================
   -- Returns the path to the given dataset name
   --===========================================================================
   function Get_Path (Name : Dynamic_String) return String is
   begin

      return "maps/" & (-Name) & "/";

   end Get_Path;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Loads a given dataset
   --===========================================================================
   procedure Load_Dataset (Name : Dynamic_String) is

      use Ada.Directories;
      use Ada.Text_IO;

      File_Id   : File_Type;

   begin

      Ada.Text_IO.Put_Line ("loading map data from " & (-Name));

      Database_Path := + Get_Path (Name);

      if Exists (-Database_Path) then

         -- Set new as default
         -------------------------

         Open (File_Id, Out_File, Default_File);

         Put_Line (File_Id, -Name);

         Close (File_Id);

      else

         -- Clear but keep default
         --------------------------

         Database_Path := Empty_String;

      end if;

      Maps.Terrain.Load_Grid_File;

      Maps.Layers.Load_Shape_Files;

      Maps.Layers.Airspaces.Generate_Airspaces;

   exception

      when others =>

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
      Name    : Dynamic_String;

   begin

      if Exists (Default_File) then

         -- Try loading the default dataset
         --------------------------------------------------------------

         Open (File_Id, In_File, Default_File);

         Name := +Trim (Get_Line (File_Id));

         Close (File_Id);

         Load_Dataset (Name);

      end if;

   exception

      when others =>

         Close (File_Id);

   end Load_Default_Dataset;
   -----------------------------------------------------------------------------

end Maps.Loader;
--------------------------------------------------------------------------------
