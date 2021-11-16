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
--
--//////////////////////////////////////////////////////////////////////////////
package Flight.Aircraft is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Model        : String_12 := (others => ' ');
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Registration : String_12 := (others => ' ');
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The maximum airspeed
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Maximum_Airspeed : Float := 70.0;
   
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
   -- Returns the maximum altitude when getting to a given point
   -- NOTE: No_Altitude is returned when the point is unreachable
   --===========================================================================
   function Get_Final_Altitude (Position : Position_Record) return Float;

   --===========================================================================
   -- Initializes the package
   --===========================================================================
   procedure Init;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Mass_Point is record
      
      Active : Boolean;
      
      Label  : String_12;
      
      Mass : Float;
      
      Arm    : Float;
      
   end record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Mass_Point : constant Mass_Point := (Active => False,
                                           Label  => (others => ' '),
                                           Mass => 0.0,
                                           Arm    => 0.0);
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Mass_Point_Range is new Positive range 1..10;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Mass_Points : array (Mass_Point_Range) of Mass_Point := (others => No_Mass_Point);
   
   --===========================================================================
   --
   --===========================================================================
   procedure Read_Aircraft_Data;
   
end Flight.Aircraft;
--------------------------------------------------------------------------------
