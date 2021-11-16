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
-- This package provides an interface to update GPU resources.
--//////////////////////////////////////////////////////////////////////////////
package Gl.Resources is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Context_Range is new Positive range 1..2;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The current context
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Active_Context : Context_Range := Context_Range'First;

   --===========================================================================
   -- Requests the GPU to load the data.
   -- NOTE: if Length is 0 (default), the full buffer is loaded. Otherwise only
   -- Length is used.
   --===========================================================================
   procedure Update_Resource (Resource : in out Gl_Uint; Buffer : access Gl_Float_Vec; Length : Natural := 0);


end Gl.Resources;
--------------------------------------------------------------------------------
