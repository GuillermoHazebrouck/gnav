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

-- Gnav


--//////////////////////////////////////////////////////////////////////////////
-- This the package contains the surrounding traffic.
--//////////////////////////////////////////////////////////////////////////////
package Flight.Traffic is
 
   --===========================================================================
   -- Connects the timed signal that maintains the tracks
   --===========================================================================
   procedure Init;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Traffic_Id is String (1..10);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Traffic_Id : constant Traffic_Id := (others => ' ');
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Alarm_Level is new Natural range 0..3;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Alarm : constant Alarm_Level := 0;
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Different kind of traffic (based on FLARM ICD)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Traffic_Type is (Uknonwn,
                         Glider,
                         Tow_Plane,
                         Rotorcraft,
                         Skydiver,
                         Drop_Plane,
                         Hang_Glider,
                         Paraglider,
                         Prop,
                         Turbo_Jet,
                         Ballon,
                         Airship,
                         Unmanned,
                         Obstacle);                         
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a traffic object.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Traffic_Record is record
   
      Active     : Boolean;
      
      Time_Stamp : Time;
      
      Id         : Traffic_Id;
      
      Position   : Position_Record;
      
      Vertical   : Float;
      
      Speed      : Float;
      
      Climb_Rate : Float;
      
      Course     : Float;
      
      No_Track   : Boolean;
      
      Coasted    : Boolean;
      
   end record;
         
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The default value
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
   No_Traffic_Record : constant Traffic_Record := (Active     => False,      
                                                   Time_Stamp => Clock,      
                                                   Id         => No_Traffic_Id,      
                                                   Position   => No_Position_Record,      
                                                   Vertical   => 0.0,      
                                                   Speed      => 0.0,      
                                                   Climb_Rate => 0.0,      
                                                   Course     => 0.0,      
                                                   No_Track   => False,
                                                   Coasted    => False);
                                                  
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The range of traffic objects
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Traffic_Range is new Positive range 1..100;
         
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Contains the traffic objects
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Traffic_Data : array (Traffic_Range) of Traffic_Record := (others => No_Traffic_Record);
      
   --===========================================================================
   -- Puts the track in the storage, taking into account its identity
   --===========================================================================
   procedure Keep_Track (Element : Traffic_Record);
   
end Flight.Traffic;
--------------------------------------------------------------------------------
