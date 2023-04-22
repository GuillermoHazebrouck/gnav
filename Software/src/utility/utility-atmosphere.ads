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
with Utility.Events;
use  Utility.Events;
with Utility.Units;
use  Utility.Units;

--//////////////////////////////////////////////////////////////////////////////
-- Provides functions to correct data due to atmospheric variations
--//////////////////////////////////////////////////////////////////////////////
package Utility.Atmosphere is

   --===========================================================================
   --
   --===========================================================================
   function Get_Qnh_Valid return Boolean;

   --===========================================================================
   -- Sets the QNH in hPa
   --===========================================================================
   procedure Set_Qnh (Value : Float; Valid : Boolean);

   --===========================================================================
   -- Gets the selected QNH
   --===========================================================================
   function Get_Qnh return Float;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   On_Qnh_Changed : Event_Stack;

   --===========================================================================
   -- Sets the QNH in hPa
   --===========================================================================
   procedure Set_Altitude_Unit (Value : Altitude_Units);

   --===========================================================================
   -- Gets the selected altititude unit
   --===========================================================================
   function Get_Altitude_Unit return Altitude_Units;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   On_Altitude_Unit_Changed : Event_Stack;

   --===========================================================================
   -- Converts flight level (in hectofeet) into altitude for a given QNH
   --===========================================================================
   function To_Altitude (Level : Float) return Float;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- String type used to represent altitudes (and flight levels)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Altitude_Strings is String (1..7);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Altitude_String : constant Altitude_Strings := (others => ' ');

   --===========================================================================
   -- Converts a string representation of an altitude to another one based on
   -- the current QNH and selected unit. For example: FL045 becomes 3873M for a
   -- QNH of 990hPa and meters.
   -- NOTE:
   --  > "GND" is always preserved as such.
   --  > If the QNH is not valid, flight levels are preserved unless forced.
   --===========================================================================
   function To_Altitude (Value : Altitude_Strings; Forced : Boolean := False) return Altitude_Strings;

   --===========================================================================
   -- Converts converts TAS to IAS (ISA) for a given altitude
   --===========================================================================
   function To_Airspeed (Value : Float; Altitude : Float) return Float;

end Utility.Atmosphere;
--------------------------------------------------------------------------------
