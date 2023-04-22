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
-- Gnav
with Math.Vector2;
use  Math.Vector2;
with Maps;
use  Maps;
with Utility.Events;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Flight.Wind is

   --===========================================================================
   -- Stream: take the wind from the data stream when available
   -- Computation: make an internal computation (several methods available)
   -- Manual: enter a manual value.
   --===========================================================================
   type Wind_Source_Mode is (Wind_Source_Stream,
                             Wind_Source_Computation,
                             Wind_Source_Manual);

   --===========================================================================
   -- The method used to compute the wind internally.
   -- None: there is no data available to make a computation
   -- Differential: uses the complement vector between the airspeed and the ground speed.
   -- (this requires sensor to obtain the airspeed and the heading).
   -- Path: uses the trajectory drift during turns (it only requires GPS data).
   --===========================================================================
   type Wind_Computation_Mode is (Wind_Computation_None,
                                  Wind_Computation_Differential,
                                  Wind_Computation_Path_Drift);

   --===========================================================================
   -- Computes the wind using the current data. The value will be used when the
   -- source is set to computation.
   -- The result depends on the source and computation modes.
   --===========================================================================
   procedure Compute_Wind;

   --===========================================================================
   -- Sets a manual entry for the wind. The value will be used when the source
   -- is set to manual.
   --===========================================================================
   procedure Set_Manual_Wind (Wind : Vector2_Record);

   --===========================================================================
   -- Sets the source
   --===========================================================================
   procedure Set_Source (Value : Wind_Source_Mode);

   --===========================================================================
   -- Gets the source
   --===========================================================================
   function Get_Source return Wind_Source_Mode;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The actual method used for the computation of the wind
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Computation_Mode : Wind_Computation_Mode := Wind_Computation_Path_Drift;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The actual source of wind data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Source : Wind_Source_Mode := Wind_Source_Computation;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Last wind computed by the algorithm
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Computed_Wind : Vector2_Record := No_Vector2_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The last time the was wind computed by the algorithm
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Computed_Wind_Time : Time := No_Time;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Last wind entered manually
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Manual_Wind : Vector2_Record := No_Vector2_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The last time the wind was manually provided
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Manual_Wind_Time : Time := No_Time;

end Flight.Wind;
--------------------------------------------------------------------------------
