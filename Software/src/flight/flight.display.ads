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
   -- The age of the data items
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Data_Field_Kind is (Field_Position,     
                            Field_Altitude, 
                            Field_Elevation,
                            Field_Speed,         
                            Field_Airspeed,      
                            Field_Course,
                            Field_Heading,
                            Field_Turn,
                            Field_Wind);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Data_Age_Array is array (Data_Field_Kind) of Time;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Data_Origin_Kind is (Origin_External,
                             Origin_Internal);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Data_Origin_Array is array (Data_Field_Kind) of Data_Origin_Kind;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Contains the instantaneus flight variables
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Flight_Data_Record is tagged record
   
      Timestamp : Time;              -- The dataframe timestamp
      
      Position  : Position_Record;   -- The position from the GPS
   
      Speed     : Float;             -- The ground speed
   
      Step      : Float;             -- The distance to the previous step
      
      Airspeed  : Float;             -- The indicated airspeed (km/h)
   
      Altitude  : Float;             -- The gps altitude above MSL (m)
    
      Elevation : Float;             -- The altitude above the ground
      
      Heading   : Float;             -- The true heading (direction of the nose)
      
      Course    : Float;             -- The path course (relative to the north)
      
      Turn      : Float;             -- The turn rate (deg/s)
      
      Wind      : Vector2_Record;    -- The wind vector (m/s)
   
      Ages      : Data_Age_Array;    -- The data ages
      
      Origin    : Data_Origin_Array; -- The data origin
      
   end record;
   
   --===========================================================================
   -- Indicates how old the data is in relation to now
   --===========================================================================
   function Age (This : Flight_Data_Record; Field : Data_Field_Kind) return Duration;
   
   --===========================================================================
   -- Indicates how old the data is in relation to the original update
   --===========================================================================
   function Relative_Age (This : Flight_Data_Record; Field : Data_Field_Kind) return Duration;
   
   --===========================================================================
   -- Indicates if the data was an new update
   --===========================================================================
   function Is_Update (This : Flight_Data_Record; Field : Data_Field_Kind) return Boolean;
      
   --===========================================================================
   -- Indicates if the data was collected less than 2 seconds ago
   --===========================================================================
   function Is_Recent (This : Flight_Data_Record; Field : Data_Field_Kind) return Boolean;
      
   --===========================================================================
   -- Sets the timestamp of the data field to the currently cached time
   --===========================================================================
   procedure Cache_Timestamp (This : in out Flight_Data_Record; Field : Data_Field_Kind);
   
   --===========================================================================
   -- Returns the previous state
   --===========================================================================
   function Get_Previous return Flight_Data_Record;
   
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
                                                    Turn      => 0.0,
                                                    Wind      => No_Vector2_Record,
                                                    Ages      => (others => Clock - 10.0),
                                                    Origin    => (others => Origin_Internal));
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The most recent flight data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
   Data : Flight_Data_Record := No_Flight_Data;
   
   --===========================================================================
   -- Loads the current position to the circular buffer
   --===========================================================================
   procedure Cache_Data;
   
   --===========================================================================
   -- Clears the whole history
   --===========================================================================
   procedure Clear_History;
   
   --===========================================================================
   -- Draws the horizontal path in geographic coordinates
   --===========================================================================
   procedure Draw_Horizontal_Path (View : Map_View_Record);
   
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
   -- Computes the wind using the current data:
   -- The result depends on the source and computation modes.
   --===========================================================================
   procedure Compute_Wind;
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compute_Turn_Rate;
   
   --===========================================================================
   -- Event triggered when the data is cached
   --===========================================================================
   On_Data_Cached : access procedure := null;
   
   --===========================================================================
   -- Event triggered when the data is cleared
   --===========================================================================
   On_Data_Cleared : access procedure := null;
   
private
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A counter to count the number of acquired messagess.
   -- NOTE: the counter is reset at regular intervals by Flight.Stream.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Message_Counter : Natural := 0;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The actual source of wind data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Wind_Source : Wind_Source_Mode := Wind_Source_Computation;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The actual method used for the computation of the wind
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Wind_Computation : Wind_Computation_Mode := Wind_Computation_None;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The last data time
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Cached_Time : Time := Clock;
   
end Flight;
--------------------------------------------------------------------------------
