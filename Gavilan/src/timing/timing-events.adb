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
with Ada.Text_IO;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Timing.Events is
   
   --===========================================================================
   --
   --===========================================================================
   function Elapsed (This : in out Timer_Record) return Boolean is
   begin
      
      if This.Active then
                   
         if This.Counter >= This.Timer then
         
            This.Counter := Time_Delta;
         
            return True;
         
         else
         
            This.Counter := This.Counter + Time_Delta;
         
         end if;
         
      end if;
      
      return False;
      
   end Elapsed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Stop (This : in out Timer_Record) is
   begin
      
      This.Active := False;
      
   end Stop;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Restart (This : in out Timer_Record) is
   begin
      
      This.Active  := True;
      
      This.Counter := 0.0;
      
   end Restart;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Register_Timer (Timer : Duration; Callback : Timed_Procedure) is
   begin
      
      for I in Timer_Stack'Range loop
         
         if Timer_Stack (I) = No_Timed_Record then
            
            Timer_Stack (I).Active   := True;
            
            Timer_Stack (I).Timer    := Timer;
            
            Timer_Stack (I).Callback := Callback;
            
            return;
            
         end if;
                  
      end loop;
            
      Ada.Text_IO.Put_Line ("warning: could not register timed event!");
      
   end Register_Timer;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   function Register_Timer (Timer : Duration; Callback : Timed_Procedure) return access Timer_Record is
   begin
      
      for I in Timer_Stack'Range loop
         
         if Timer_Stack (I) = No_Timed_Record then
            
            Timer_Stack (I).Active   := True;
            
            Timer_Stack (I).Timer    := Timer;
            
            Timer_Stack (I).Callback := Callback;
            
            return Timer_Stack (I)'Access;
            
         end if;
                  
      end loop;
            
      Ada.Text_IO.Put_Line ("warning: could not register timed event!");
      
      return null;
      
   end Register_Timer;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Tick is
   begin
      
      for I in Timer_Stack'Range loop
         
         if Timer_Stack (I).Callback /= null and Timer_Stack (I).Active then
            
            if Timer_Stack (I).Elapsed then
         
               Timer_Stack (I).Callback.all;
               
            end if;
                     
         end if;
         
      end loop;
               
   end Tick;
   -----------------------------------------------------------------------------
   
   
   

end Timing.Events;
--------------------------------------------------------------------------------
