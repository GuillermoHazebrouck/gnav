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
-- Standard
with Ada.Exceptions;
use  Ada.Exceptions;

-- Depencencies
--//////////////////////////////////////////////////////////////////////////////

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Utility.Log is

   --===========================================================================
   --
   --===========================================================================
   procedure Init;

   --===========================================================================
   --
   --===========================================================================
   procedure Close;

   --===========================================================================
   -- Puts the message in the log file
   --===========================================================================
   procedure Put_Message (Message : String);

   --===========================================================================
   -- Puts the message in the log file
   --===========================================================================
   procedure Put_Message (E : Exception_Occurrence; Message : String);

end Utility.Log;
--------------------------------------------------------------------------------
