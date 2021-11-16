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
package Timing.Events is
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Timed_Procedure is access procedure;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Timer_Record is tagged private;
     
   --===========================================================================
   -- Sets the timer to inactive
   --===========================================================================
   procedure Stop (This : in out Timer_Record);
   
   --===========================================================================
   -- Restarts the timer if it was inactive
   --===========================================================================
   procedure Restart (This : in out Timer_Record);
   
   --===========================================================================
   --
   --===========================================================================
   procedure Register_Timer (Timer : Duration; Callback : Timed_Procedure);
   
   --===========================================================================
   --
   --===========================================================================
   function Register_Timer (Timer : Duration; Callback : Timed_Procedure) return access Timer_Record;
   
   --===========================================================================
   --
   --===========================================================================
   procedure Tick;   

private
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Timer_Record is tagged record
   
      Active   : Boolean  := False;
   
      Timer    : Duration := 0.0;
      
      Counter  : Duration := 0.0;
   
      Callback : Timed_Procedure := null;
      
   end record;
     
   --===========================================================================
   -- Indicates if the timer has elapsed
   --===========================================================================
   function Elapsed (This : in out Timer_Record) return Boolean;
   
   No_Timed_Record : constant Timer_Record := (Active   => False,
                                               Timer    => 0.0,
                                               Counter  => 0.0,
                                               Callback => null);
   
   Timer_Stack : array (1..10) of aliased Timer_Record := (others => No_Timed_Record);
   
end Timing.Events;
--------------------------------------------------------------------------------
