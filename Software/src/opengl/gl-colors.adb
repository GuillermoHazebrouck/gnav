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
package body Gl.Colors is

   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   function With_Alpha (This : Color_Record; New_A : Color_Float) return Color_Record is
   begin

      return (This.R, This.G, This.B, New_A);

   end With_Alpha;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Dim (This : in out Color_Record; Factor : Color_Float) is
   begin

      This.R := Factor * This.R;
      This.G := Factor * This.G;
      This.B := Factor * This.B;

   end Dim;
   -----------------------------------------------------------------------------

end Gl.Colors;
--------------------------------------------------------------------------------
