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
with Math.Vector2;
use  Math.Vector2;
with Math.Vector2_List;

--//////////////////////////////////////////////////////////////////////////////
-- Provides tools to perform general purpose actions on a stack of nodes, like
-- measuring distances and inserting nodes.
--//////////////////////////////////////////////////////////////////////////////
package Math.Tools is

   --///////////////////////////////////////////////////////////////////////////
   -- Floating point functions
   --///////////////////////////////////////////////////////////////////////////

   --===========================================================================
   -- Indicates if the given point is located inside the polygon. This algorithm
   -- is based in the number of horizontal cuts.
   --===========================================================================
   function Contains_Point (Points : Math.Vector2_List.Stack_Access; X, Y : Long_Float) return Boolean;

   --===========================================================================
   -- Indicates if the given point is within the offset of the given polyline
   --===========================================================================
   function Inside_Offset (Points  : Math.Vector2_List.Stack_Access;
                           Point   : Vector2_Record;
                           Offset  : Long_Float;
                           Closed  : Boolean;
                           Rounded : Boolean) return Boolean;

   --===========================================================================
   -- Cleans the given line by depopulating dense areas, while preserving the
   -- original shape. At least three points are needed.
   -- The first and the last points are never removed from the line.
   --===========================================================================
   procedure Clean_Line (Points : Math.Vector2_List.Stack_Access; Resolution : Long_Float);

   --===========================================================================
   -- Gets the minimum distance to the points
   --===========================================================================
   function Get_Minimum_Distance (Points : Math.Vector2_List.Stack_Access;
                                  Point  : Vector2_Record;
                                  Closed : Boolean) return Long_Float;

   --===========================================================================
   -- Gets the closest node in the given path to the given point.
   --===========================================================================
   function Get_Closest_Node  (Points : Math.Vector2_List.Stack_Access;
                               Point  : Vector2_Record) return Vector2_Access;

   --===========================================================================
   -- Returns the node that is closest to the given boundary.
   --===========================================================================
   function Get_Closest_Point (Points : Math.Vector2_List.Stack_Access;
                               Point  : Vector2_Record;
                               Closed : Boolean) return Vector2_Record;

   --===========================================================================
   -- Returns the node that is closest to the given boundary.
   --===========================================================================
   procedure Get_Closest_Point (Points  : Math.Vector2_List.Stack_Access;
                                Point   : Vector2_Record;
                                Closed  : Boolean;
                                Closest : in out Vector2_Record;
                                Offset  : out Long_Float;
                                Segment : out Natural);

   --===========================================================================
   -- Gets the curvilinear coordinate from the first point up to the given point
   -- If the point does not belog to this stack, it returns zero.
   --===========================================================================
   function Get_Curvilinear_Coordinate (Points : Math.Vector2_List.Stack_Access;
                                        Point  : Vector2_Access) return Long_Float;

   --===========================================================================
   -- Inserts a point in the stack at the closest edge within the threshold.
   -- Closed: indicates if the path is closed, in which case the edge between
   -- the last and the first node is also considered.
   -- Returns the added node (or null if no node has been added).
   --===========================================================================
   function Insert_Point (Points    : Math.Vector2_List.Stack_Access;
                          Point     : Vector2_Record;
                          Threshold : Long_Float;
                          Closed    : Boolean) return Vector2_Access;

   --===========================================================================
   -- Calculates the lenght of the polyline
   --===========================================================================
   function Get_Length (Points : Math.Vector2_List.Stack_Access) return Long_Float;

   --===========================================================================
   -- Returns a point at the given curvilinear coordinate.
   -- NOTE: for S < 0      => (0,0)
   --       for S > Lenght => (0,0)
   --===========================================================================
   function Get_Point_At (Points : Math.Vector2_List.Stack_Access;
                          S      : Long_Float) return Vector2_Record;

   --===========================================================================
   -- Returns the geometric center of the given points
   --===========================================================================
   function Get_Geometric_Center (Points : Math.Vector2_List.Stack_Access) return Vector2_Record;

end Math.Tools;
