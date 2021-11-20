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

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Maps.Terrain is

   --===========================================================================
   -- Loads the terrain data from the given file in Esri or binary
   --===========================================================================
   procedure Load_Grid_File;

   --===========================================================================
   -- Saves the chart in binary format
   --===========================================================================
   procedure Save_Binary;

   --===========================================================================
   -- Returns the middle for the entire terrain chart.
   --===========================================================================
   function Get_Middle return Position_Record;

   --===========================================================================
   -- Draws the map terrain using OpenGL for the given zoom level
   --===========================================================================
   procedure Draw (View : Map_View_Record);

   --===========================================================================
   -- Modifies the altitude in the viscinity of the given position.
   --===========================================================================
   procedure Modify_Altitude (Position    : Position_Record;
                              Elevation   : Short_Float := 0.1;
                              Extent      : Integer     := 0;
                              Lower_Level : Short_Float := Short_Float'First;
                              Upper_Level : Short_Float := Short_Float'Last);

   --===========================================================================
   -- Returns the approximate elevation at the given position
   --===========================================================================
   function Get_Elevation (Position : Position_Record) return Float;

   --===========================================================================
   -- Notifies that the range function has changed
   --===========================================================================
   procedure Notify_Range_Changed;

private

   --===========================================================================
   -- Opens an Esri ASCII grid file and loads the terrain data into the buffer
   --===========================================================================
   procedure Load_Esri_Grid_File;

   --===========================================================================
   -- Loads the chart from a binary file
   --===========================================================================
   procedure Load_Binary;

end Maps.Terrain;
--------------------------------------------------------------------------------
