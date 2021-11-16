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
-- Gnav
with Math.Vector2;
use  Math.Vector2;
with Maps;
use  Maps;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Flight is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Contains the instantaneus flight variables
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Flight_Data_Record is record
   
      Timestamp : Time;
      
      Position  : Position_Record;
   
      Speed     : Float;
   
      Step      : Float;
      
      Airspeed  : Float;
   
      Altitude  : Float;
   
      Elevation : Float;
      
      Heading   : Float;
      
      Course    : Float;
      
      Wind      : Vector2_Record;
   
   end record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Default flight data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
   No_Flight_Data : constant Flight_Data_Record := (Timestamp => Clock,
                                                    Position  => No_Position_Record,
                                                    Speed     => 0.0,
                                                    Step      => 0.0,
                                                    Airspeed  => 0.0,
                                                    Altitude  => 0.0,
                                                    Elevation => 0.0,
                                                    Heading   => 0.0,
                                                    Course    => 0.0,
                                                    Wind      => No_Vector2_Record);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The current flight data (from gps aquisition or simulation)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
   Data : Flight_Data_Record := No_Flight_Data;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The number of dots in a history cluster (up to 10s in 0.5s intervals)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++         
   type Dots_Range is range 1..20;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The number of clusters (up to 10 minutes in 0.5s intervals)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++         
   type Cluster_Range is range 1..60;
   
   --===========================================================================
   -- Loads the current position to the circular buffer
   --===========================================================================
   procedure Cache_Data;
   
   --===========================================================================
   -- Clears the whole history
   --===========================================================================
   procedure Clear_History;
   
   --===========================================================================
   -- Returns the previous state
   --===========================================================================
   function Get_Previous return Flight_Data_Record;
   
   --===========================================================================
   -- Draws the horizontal path in geographic coordinates
   --===========================================================================
   procedure Draw_Horizontal_Path (View : Map_View_Record);
   
end Flight;
--------------------------------------------------------------------------------
