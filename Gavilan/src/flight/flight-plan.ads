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
-- Gnav
with Maps;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Flight.Plan is

   --===========================================================================
   --
   --===========================================================================
   procedure Init;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the waypoints must be automatically set when reached
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Jump_In_Proximity : Boolean := True;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if there have been manual actions on the flight plans
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Modified : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The distance to jump automatically to the next waypoint
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Proximity_Threshold : Float := 1.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Waypoint_Names is String (1..4);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Waypoint_Record is tagged record

      Name         : Waypoint_Names;

      Position     : Position_Record;

      Distance     : Float;

      Bearing      : Float;

      Arrival      : Ada.Calendar.Time;

      Elevation    : Float;

      Margin       : Float;

      Visited      : Boolean;

      In_Range     : Boolean;

      In_Proximity : Boolean;

      Is_Active    : Boolean;

      Is_Loaded    : Boolean;

   end record;

   --===========================================================================
   -- A string representation the bearing to the waypoint
   --===========================================================================
   function Get_Bearing_Image (This : Waypoint_Record) return String;

   --===========================================================================
   -- A string representation the distance to the waypoint
   --===========================================================================
   function Get_Distance_Image (This : Waypoint_Record) return String;

   --===========================================================================
   -- A string representation of the vector
   --===========================================================================
   function Get_Vector_Image (This : Waypoint_Record) return String;

   --===========================================================================
   --
   --===========================================================================
   function Next_Waypoint return access Waypoint_Record;

   --===========================================================================
   --
   --===========================================================================
   function Home_Waypoint return access Waypoint_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Waypoint_Record : constant Waypoint_Record := (Name         => (others => ' '),
                                                     Position     => No_Position_Record,
                                                     Distance     => 0.0,
                                                     Bearing      => 0.0,
                                                     Arrival      => Ada.Calendar.Clock,
                                                     Elevation    => 0.0,
                                                     Margin       => 0.0,
                                                     Visited      => False,
                                                     In_Range     => False,
                                                     In_Proximity => False,
                                                     Is_Active    => False,
                                                     Is_Loaded    => False);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Waypoint_Range is new Positive range 1..15;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Waypoint_Array is array (Waypoint_Range) of aliased Waypoint_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Task_Record is tagged record

      Length    : Float;

      Course    : Float;

      Vector    : Vector2_Record;

      Progress  : Float;

      Is_Active : Boolean;

      Is_Loaded : Boolean;

      Point_A   : access Waypoint_Record := null;

      Point_B   : access Waypoint_Record := null;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Task_Record : constant Task_Record := (Length    => 0.0,
                                             Course    => 0.0,
                                             Vector    => No_Vector2_Record,
                                             Progress  => 0.0,
                                             Is_Active => False,
                                             Is_Loaded => False,
                                             Point_A   => null,
                                             Point_B   => null);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Task_Array is array (Waypoint_Range) of aliased Task_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Flight_Names is String (1..8);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Flight_Plan_Record is tagged record

      Name      : Flight_Names;

      Waypoints : Waypoint_Array;

      Target    : Waypoint_Range;

      Tasks     : Task_Array;

      Go_Back   : Boolean;

      Is_Loaded : Boolean;

   end record;

   --===========================================================================
   -- Initializes a new flight plan with meanfull data
   --===========================================================================
   procedure Initialize (This : in out Flight_Plan_Record);

   --===========================================================================
   -- Recomputes the leg size and bearing
   --===========================================================================
   procedure Recompute_Tasks (This : in out Flight_Plan_Record);

   --===========================================================================
   -- Toggles the back route
   --===========================================================================
   procedure Set_Go_Back (This : in out Flight_Plan_Record; Value : Boolean);

   --===========================================================================
   -- Returns the active flight plan
   --===========================================================================
   function Flight_Plan return access Flight_Plan_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Flight_Plan_Record : constant Flight_Plan_Record := (Name      => (others => ' '),
                                                           Waypoints => (others => No_Waypoint_Record),
                                                           Target    => Waypoint_Range'First,
                                                           Tasks     => (others => No_Task_Record),
                                                           Go_Back   => False,
                                                           Is_Loaded => False);

   --===========================================================================
   -- Activates the next flight plan
   --===========================================================================
   function Next_Flight_Plan return Boolean;

   --===========================================================================
   -- Activates the previous flight plan
   --===========================================================================
   function Previous_Flight_Plan return Boolean;

   --===========================================================================
   -- Creates a new flight plan
   --===========================================================================
   function Append_Flight_Plan return Boolean;

   --===========================================================================
   -- Removes the selected flight plan
   --===========================================================================
   function Remove_Flight_Plan return Boolean;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Flight_Plan_Range is new Positive range 1..10;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Flight_Plan_Array is array (Flight_Plan_Range) of aliased Flight_Plan_Record;

   --===========================================================================
   -- Loads the flight plans from memory
   --===========================================================================
   procedure Read_Flight_Plans;

   --===========================================================================
   -- Writes all flight plans to the disk
   --===========================================================================
   procedure Write_Flight_Plans;

   --===========================================================================
   -- Saves the flight plan if there where changes (when Modified is true)
   --===========================================================================
   procedure Save_If_Modified;

   --===========================================================================
   -- Updates the waypoint status using the latest flight data
   --===========================================================================
   procedure Update_Flight_Plan;

   --===========================================================================
   -- Activates the previous waypoint.
   --===========================================================================
   procedure Goto_Previous_Waypoint;

   --===========================================================================
   -- Activates the next waypoint
   --===========================================================================
   procedure Goto_Next_Waypoint;

   --===========================================================================
   -- Removes the current waypoint (the first items is never removed)
   --===========================================================================
   function Remove_Active_Waypoint return Boolean;

   --===========================================================================
   -- Adds a waypoint after the active one as a copy
   --===========================================================================
   function Append_Waypoint return Boolean;

   --===========================================================================
   -- Adds a waypoint before the active one as a copy
   --===========================================================================
   function Prepend_Waypoint return Boolean;

   --===========================================================================
   -- Toggles the back route
   --===========================================================================
   procedure Toggle_Go_Back;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Active_Flight_Plan : Flight_Plan_Range := Flight_Plan_Range'First;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- All of the flight plans
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Flight_Plans : Flight_Plan_Array := (others => No_Flight_Plan_Record);

end Flight.Plan;
--------------------------------------------------------------------------------
