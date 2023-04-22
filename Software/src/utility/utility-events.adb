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
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Events is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Connect (This : in out Event_Stack; Callback : Callback_Type) is
   begin

      for I in This.Callbacks'Range loop

         if This.Callbacks (I) = null then

            This.Callbacks (I) := Callback;

            return;

         elsif This.Callbacks (I) = Callback then

            -- (already connected to the stack)

            return;

         end if;

      end loop;

      Utility.Log.Put_Message ("WARNING: a callback could not be connected to an event");

   end Connect;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Trigger (This : Event_Stack) is
   begin

      for I in This.Callbacks'Range loop

         if This.Callbacks (I) /= null then

            This.Callbacks (I).all;

         end if;

      end loop;

   end Trigger;
   -----------------------------------------------------------------------------


end Utility.Events;
--------------------------------------------------------------------------------
