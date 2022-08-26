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
with Maps;

--//////////////////////////////////////////////////////////////////////////////

--//////////////////////////////////////////////////////////////////////////////
package Display is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Global flag to indicates if a refresh is pending
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Refresh : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if this is a refresh to blink
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Blink   : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Stops the main loop
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Stop    : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The display width
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Width   : Float := 0.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The display height
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Height  : Float := 0.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The five front panel keys
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Front_Panel_Keys is (Panel_Wheel_Left,
                             Panel_Wheel_Right,
                             Panel_Wheel_Button,
                             Panel_Button_Left,
                             Panel_Button_Right);

end Display;
--------------------------------------------------------------------------------
