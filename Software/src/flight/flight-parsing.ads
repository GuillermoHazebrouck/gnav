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
package Flight.Parsing is

   --===========================================================================
   -- Reads the data from a G-NAV message
   --===========================================================================
   procedure Parse_Gnav_Message (Message : String);

   --===========================================================================
   -- Reads the data from a NMEA/FLARM message
   --===========================================================================
   procedure Parse_Nmea_Message (Message : String);

   --===========================================================================
   -- Indicates if there has been a fatal error during the parsing during the
   -- last 5 minutes.
   --===========================================================================
   function Error_Detected return Boolean;

   --===========================================================================
   -- Indicates if the error ocurred less than 5 seconds ago
   --===========================================================================
   function Recent_Error return Boolean;

end Flight.Parsing;
--------------------------------------------------------------------------------
