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
package Utility.Units is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Distance_Units is (Unit_Kilometer,
                           Unit_Nautical_Mile,
                           Unit_Meter,
                           Unit_Feet,
                           Unit_Hectofeet);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Range_Units is Distance_Units range Unit_Kilometer..Unit_Nautical_Mile;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Altitude_Units is Distance_Units range Unit_Meter..Unit_Hectofeet;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Velocity_Units is (Unit_Meter_Second, Unit_Kilometer_Hour, Unit_Knot);

   --===========================================================================
   --
   --===========================================================================
   function Convert (Value : Float; Source, Target : Distance_Units) return Float;

   --===========================================================================
   --
   --===========================================================================
   function Convert (Value : Float; Source, Target : Velocity_Units) return Float;

   --===========================================================================
   --
   --===========================================================================
   function Image (Value : Distance_Units) return String;

   --===========================================================================
   --
   --===========================================================================
   function Image (Value : Velocity_Units) return String;

end Utility.Units;
--------------------------------------------------------------------------------
