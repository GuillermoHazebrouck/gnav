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
with Meshing.Segment;
use  Meshing.Segment;
with Math.Vector2;
use  Math.Vector2;
with Math.Vector2_List;
with Stacks.Linked;

--//////////////////////////////////////////////////////////////////////////////
-- Contains the definition of an object that represents a two-dimesional
-- triangle, and exposes geometrical operations on it.
--//////////////////////////////////////////////////////////////////////////////
package Meshing.Triangle is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a triangle
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Triangle_Record is new Stacks.Linked.Linked_Record with private;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a reference to a triangle
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Triangle_Access is access all Triangle_Record'Class;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- When the position of a point relative to a triangle is requested, these
   -- are the posible results.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Point_Location_Kinds is (Inside, Outside, Close_To_AB, Close_To_BC, Close_To_CA);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Triangle_Vertex_Kinds is (A_Vertex, B_Vertex, C_Vertex);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Triangle_Side_Kinds is (AB_Side, BC_Side, CA_Side);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Triangle_Sence_Kinds is (Positive, Negative);

   --===========================================================================
   -- Returns a reference to vertex A.
   --===========================================================================
   function Get_Vertex_A (This : Triangle_Record) return Vector2_Access;

   --===========================================================================
   -- Returns a reference to vertex B.
   --===========================================================================
   function Get_Vertex_B (This : Triangle_Record) return Vector2_Access;

   --===========================================================================
   -- Returns a reference to vertex C.
   --===========================================================================
   function Get_Vertex_C (This : Triangle_Record) return Vector2_Access;

   --===========================================================================
   -- Returns the circumcenter.
   --===========================================================================
   function Get_Center (This : Triangle_Record) return Vector2_Record;

   --===========================================================================
   -- Indicates if the triangle has been initialized properly.
   --===========================================================================
   function Get_Initialized (This : Triangle_Record) return Boolean;

   --===========================================================================
   -- Indicates if the given point is inside this triangle.
   --===========================================================================
   function Contains (This : Triangle_Record; Point : Vector2_Access) return Boolean;

   --===========================================================================
   -- Finds where the given point is located in reference to this triangle.
   --===========================================================================
   function Locate_Point (This : Triangle_Record; Point : Vector2_Access) return Point_Location_Kinds;

   --===========================================================================
   -- Indicates if the given point is inside the circumcircle of this triangle.
   --===========================================================================
   function In_Circumcircle (This : Triangle_Record; Point : Vector2_Access) return Boolean;

   --===========================================================================
   -- Indicates if the given segment crosses this triangle by one side and a
   -- common vertex.
   --===========================================================================
   procedure Crosses_Segment (This : Triangle_Record; Segment : Segment_Access; Result : out Boolean; Side : out Triangle_Side_Kinds; Common_Vertex : out Vector2_Access);

   --===========================================================================
   -- Defines the vertices of the triangle.
   --===========================================================================
   procedure Set_Vertices (This : in out Triangle_Record; A, B, C : Vector2_Access);

   --===========================================================================
   -- Prints out information about the given triangle.
   --===========================================================================
   procedure Publish (This : Triangle_Record);

   --===========================================================================
   -- Returns the height of a triangle represented by ABC.
   --===========================================================================
   function Height (A, B, C : Vector2_Access) return Long_Float;

   --===========================================================================
   -- Initializes the object.
   --===========================================================================
   overriding procedure Initialize (This : in out Triangle_Record);

   --===========================================================================
   -- Adjusts the object.
   --===========================================================================
   overriding procedure Adjust (This : in out Triangle_Record);

   --===========================================================================
   -- Finalizes the object.
   --===========================================================================
   overriding procedure Finalize (This : in out Triangle_Record);

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Updates the properties of this triangle.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   procedure Update_Properties (This : in out Triangle_Record);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Triangle_Record is new Stacks.Linked.Linked_Record with record

      Initialized  : Boolean := False;

      Vertex_A     : Vector2_Access;

      Vertex_B     : Vector2_Access;

      Vertex_C     : Vector2_Access;

      Circumcenter : Vector2_Access;

      Circumradius : Long_Float;

      Area         : Long_Float;

      Sence        : Triangle_Sence_Kinds := Positive;

      AB_Lenght    : Long_Float;

      BC_Lenght    : Long_Float;

      CA_Lenght    : Long_Float;

   end record;

end Meshing.Triangle;
--------------------------------------------------------------------------------
