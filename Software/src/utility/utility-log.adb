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
use  Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Text_IO;
use  Ada.Text_IO;
-- Gnav
with Timing.Events;
with Utility.Strings;




--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Log is

   Log_File : Ada.Text_IO.File_Type;

   --===========================================================================
   --
   --===========================================================================
   procedure Flush_Log is
   begin

      Flush (Log_File);

   end Flush_Log;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   procedure Init is
   begin

      Create (Log_File, Out_File, "log/" & Utility.Strings.Replace (Image (Clock), ' ', '_') & ".dat");

      -- Flush the file every 5 seconds
      ------------------------------------
      Timing.Events.Register_Timer (5.0, Flush_Log'Access);

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Close is
   begin

      Close (Log_File);

   end Close;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Puts the message in the log file
   --===========================================================================
   procedure Put_Message (Message : String) is
   begin

      Put_Line (Log_File, Message);

   end Put_Message;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- Puts the message in the log file
   --===========================================================================
   procedure Put_Message (E : Exception_Occurrence; Message : String) is
   begin

      Put_Line (Log_File, Message & " >> raised exception >> " & Exception_Message (E));

   end Put_Message;
   -----------------------------------------------------------------------------

end Utility.Log;
--------------------------------------------------------------------------------
