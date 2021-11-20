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
use  Maps;
with Utility.Strings;
use  Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
-- This package provides all aircraft related data:
--  > Callsign, model, etc.
--  > Weight and balance
--  > Performance and range
--//////////////////////////////////////////////////////////////////////////////
package Flight.Aircraft is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The aircraft model (e.g.: "ASK-21")
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Model : String_12 := (others => ' ');
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The aircraft registration (e.g.: "D-1142") 
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Registration : String_12 := (others => ' ');
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The maximum airspeed (in m/s)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Maximum_Airspeed : Float := 70.0;
   
   --===========================================================================
   -- Recalculates the aircraft mass using the local mass points.
   --===========================================================================
   procedure Recalculate_Mass;
   
   --===========================================================================
   -- Calculates the airspeed and sink rate in the equilibrium state for all
   -- polar nodes.
   -- NOTE: this must be run when the altitude and mass changed considerably.
   --===========================================================================
   procedure Calculate_Gliding_States;
   
   --===========================================================================
   -- Calculates the best gliding slopes in all directions for the current wind.
   --===========================================================================
   procedure Calculate_Gliding_Spectrum;
   
   --===========================================================================
   -- Returns the maximum altitude when getting to a given point in a straight
   -- line, considering uniform wind (not considering the wind aloft, sink 
   -- or lift, nor the necessary turn to align the craft in that direction).
   -- NOTE: No_Altitude is returned when the point is unreachable
   --===========================================================================
   function Get_Final_Altitude (Position : Position_Record) return Float;

   --===========================================================================
   -- Initializes the package
   --===========================================================================
   procedure Init;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A point on the aircraft where mass can be attached
   -- It can be used for pilots, water balast, spin balast, lead balast, etc.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Mass_Point is record
      
      -- Indicates if the mass point is used
      Active : Boolean;
      
      -- The name of the mass point
      Label  : String_12;
      
      -- The mass (in kg)
      Mass   : Float;
      
      -- The arm (in meters)
      Arm    : Float;
      
   end record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Default mass point value
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Mass_Point : constant Mass_Point := (Active => False,
                                           Label  => (others => ' '),
                                           Mass => 0.0,
                                           Arm    => 0.0);
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The range of mass points (not all need to be active)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Mass_Point_Range is new Positive range 1..10;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- All mass points
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Mass_Points : array (Mass_Point_Range) of Mass_Point := (others => No_Mass_Point);
   
   --===========================================================================
   -- Reads the aircraft data from the data file 'data/aircraft.dat'
   --===========================================================================
   procedure Read_Aircraft_Data;
   
end Flight.Aircraft;
--------------------------------------------------------------------------------
