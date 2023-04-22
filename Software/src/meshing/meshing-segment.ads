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
with Stacks.Linked;

--//////////////////////////////////////////////////////////////////////////////
-- Contains the definition of an object that represents a two-dimesional
-- segment, and exposes geometrical operations on it.
--//////////////////////////////////////////////////////////////////////////////
package Meshing.Segment is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a 2D straight segment in double presition.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Segment_Record is new Stacks.Linked.Linked_Record with private;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a reference to a 2D segment.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Segment_Access is access all Segment_Record'Class;

   --===========================================================================
   -- Returns a reference to vertex A.
   --===========================================================================
   function Get_Vertex_A (This : Segment_Record) return Vector2_Access;

   --===========================================================================
   -- Returns a reference to vertex B.
   --===========================================================================
   function Get_Vertex_B (This : Segment_Record) return Vector2_Access;

   --===========================================================================
   -- Returns the direction of the vector.
   --===========================================================================
   function Get_Direction (This : Segment_Record) return Vector2_Access;

   --===========================================================================
   -- Returns the lenght of the vector.
   --===========================================================================
   function Get_Length (This : Segment_Record) return Long_Float;

   --===========================================================================
   -- Indicates if the given point is contained in this segment between A and B.
   --===========================================================================
   function Contains (This : Segment_Record; Point : Vector2_Access; Offset : Long_Float := Meshing.Proximity_Threshold) return Boolean;

   --===========================================================================
   -- Returns the (minimum) distance from the point to this segment.
   --===========================================================================
   function Distance_To (This : Segment_Record; Point : Vector2_Access) return Long_Float;

   --===========================================================================
   -- Indicates if the segment represented by the given points crosses this segment.
   --===========================================================================
   function Crosses (This : Segment_Record; Point_A, Point_B : Vector2_Access) return Boolean;

   --===========================================================================
   -- Indicates if the given segment crosses this segment, and returns the crossing point.
   -- If there is no intersection, the intersection vector remains (0.0; 0.0).
   --===========================================================================
   procedure Intersection (This : Segment_Record; Segment : Segment_Access; Intersect : out Boolean; Intersection : in out Vector2_Record);

   --===========================================================================
   -- Outputs the vertices in the console.
   --===========================================================================
   procedure Publish (This : Segment_Record);

   --===========================================================================
   -- Defines the vertices of the segment.
   --===========================================================================
   procedure Set_Vertices (This : in out Segment_Record; A, B : Vector2_Access);

   --===========================================================================
   -- Initializes the object.
   --===========================================================================
   overriding procedure Initialize (This : in out Segment_Record);

   --===========================================================================
   -- Adjusts the object.
   --===========================================================================
   overriding procedure Adjust (This : in out Segment_Record);

   --===========================================================================
   -- Finalizes the object.
   --===========================================================================
   overriding procedure Finalize (This : in out Segment_Record);

private

   --===========================================================================
   -- Updates the properties of this segment.
   --===========================================================================
   procedure Update_Properties (This : in out Segment_Record);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Segment_Record is new Stacks.Linked.Linked_Record with record

      Initialized  : Boolean := False;

      Length       : Long_Float;

      Vertex_A     : Vector2_Access;

      Vertex_B     : Vector2_Access;

      Direction    : Vector2_Access;

   end record;

end Meshing.Segment;
