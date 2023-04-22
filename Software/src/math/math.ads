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
with Ada.Numerics;

--//////////////////////////////////////////////////////////////////////////////
-- This namespace contains strictly mathematical objects for general purpose.
-- This is a low level namespace containing no references to implementation
-- dependent namespaces.
-- No specific functions/objects are allowed here, they all must be for general
-- purpose. This has to be kept like this for the sake of code clarity and
-- portability.
--//////////////////////////////////////////////////////////////////////////////
package Math is

   pragma Pure;

   Pi    : constant Long_Float := Ada.Numerics.Pi;
   TwoPi : constant Long_Float := 2.0 * Pi;

end Math;
--------------------------------------------------------------------------------
