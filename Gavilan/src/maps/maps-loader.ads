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

-- Gnav
with Utility.Strings;
use  Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Maps.Loader is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The available range of datasets
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Dataset_Range is new Positive range 1..10;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A collection of datasets
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Dataset_Array is array (Dataset_Range) of Dynamic_String;

   --===========================================================================
   -- Loads the default dataset given by maps/default.dat
   --===========================================================================
   procedure Load_Default_Dataset;

   --===========================================================================
   -- Loads a given dataset
   --===========================================================================
   procedure Load_Dataset (Name : Dynamic_String);

   --===========================================================================
   -- Returns the list of datasets in memory
   --===========================================================================
   function Get_Datasets return Dataset_Array;

   --===========================================================================
   -- Removes the given dataset
   --===========================================================================
   procedure Delete_Dataset (Name : Dynamic_String);

end Maps.Loader;
--------------------------------------------------------------------------------
