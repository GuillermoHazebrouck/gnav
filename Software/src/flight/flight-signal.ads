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
with Ada.Calendar;
use  Ada.Calendar;
with Ada.Calendar.Formatting;
-- Gnav

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Flight.Signal is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Satellite_Kinds is (Sat_Gps,
                            Sat_Galileo,
                            Sat_Glonass,
                            Sat_Beidou);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A tracked satellite
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Satellite_Record is record

      Refreshed : Boolean;

      Valid     : Boolean;

      Id        : String (1..2);

      Elevation : Natural;

      Azimuth   : Natural;

      Strength  : Natural;

   end record;
   -----------------------------------------------------------------------------

   No_Satellite : constant Satellite_Record := (Refreshed => False,
                                                Valid     => False,
                                                Id        => "00",
                                                Elevation => 0,
                                                Azimuth   => 0,
                                                Strength  => 0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Satellite_Array is array (1..12) of Satellite_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The tracked satellites
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Satellites : array (Satellite_Kinds) of Satellite_Array := (others => (others => No_Satellite));

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Number_Of_Satellites : array (Satellite_Kinds) of Natural := (others => 0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Signal_Strength : Natural := 0;

end Flight.Signal;
--------------------------------------------------------------------------------
