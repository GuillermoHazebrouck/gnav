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
with Math.Vector2_Vector;
with Meshing.Triangle;
use  Meshing.Triangle;
with Meshing.Triangle_List;

--//////////////////////////////////////////////////////////////////////////////
-- Provides a polygon mesher
--//////////////////////////////////////////////////////////////////////////////
package Meshing.Delaunay is

   -----------------------------------------------------------------------------
   -- Populates the provided triangles stack given the nodes of a polygon.
   -- If a back buffer is provided (Cloud), then the vertices will be populated
   -- there: intersections will be added and the geometric details will be
   -- simplified.
   -- If no back buffer is provided, then the triangles will be built on the
   -- nodes, and the mesher will not consider the boundary intersections.
   -----------------------------------------------------------------------------
   procedure Generate_Mesh (Nodes  : Math.Vector2_List.Stack_Access;
                            Mesh   : Meshing.Triangle_List.Stack_Access;
                            Cloud  : Math.Vector2_List.Stack_Access := null);

private

   -----------------------------------------------------------------------------
   -- Loads the nodes into the local boundary and vertex stack, normalizing
   -- their coordinates to guarantee the hightes presition.
   -----------------------------------------------------------------------------
   procedure Load_Nodes;

   -----------------------------------------------------------------------------
   -- Assignes an Id to each vertex (equal to their position in the stack).
   -----------------------------------------------------------------------------
   procedure Indexate_Vertices;

   -----------------------------------------------------------------------------
   -- Builds the local list of boundary segments directly on the nodes, not
   -- moving or removing any of them.
   -- This method does a minimal consistency check.
   -----------------------------------------------------------------------------
   procedure Generate_Boundary_1;

   -----------------------------------------------------------------------------
   -- Builds the local list of boundary segments on a parallel vertex buffer,
   -- leaving the original nodes untouched. The parallel buffer my contain
   -- a different number of points, and in slightly different positions.
   -- This method simplifies the geometry details as much as possible to avoid
   -- inconsistencies. It also adds the boundary intersections.
   -----------------------------------------------------------------------------
   procedure Generate_Boundary_2;

   -----------------------------------------------------------------------------
   -- Generates a triangle covering the whole polygon.
   -----------------------------------------------------------------------------
   procedure Generate_Starting_Triangle;

   -----------------------------------------------------------------------------
   -- Adds the given vertex to the triangulation, generating new triangles
   -- according to the insertion rules.
   -----------------------------------------------------------------------------
   procedure Insert_Vertex (Vertex_To_Add : Vector2_Access);

   -----------------------------------------------------------------------------
   -- Splits the given triangle in two by the specified side, and adds the
   -- new half to the stack. It returns a reference to the new triangle.
   -----------------------------------------------------------------------------
   procedure Split_In_Two (Piercing_Vertex   : Vector2_Access;
                           Triangle_To_Split : Triangle_Access;
                           Side              : Triangle_Side_Kinds;
                           New_Triangle      : out Triangle_Access);

   -----------------------------------------------------------------------------
   -- Returns the triangle adjacent to the provided one by the provided side.
   -----------------------------------------------------------------------------
   procedure Get_Adjacent_Triangle (Host          : Triangle_Access;
                                    Host_Side     : Triangle_Side_Kinds;
                                    Adjacent      : out Triangle_Access;
                                    Adjacent_Side : out Triangle_Side_Kinds);

   -----------------------------------------------------------------------------
   -- Indicates if the two given vertices belong to a boundary segment.
   -----------------------------------------------------------------------------
   function Is_At_Boundary (Vertex_1, Vertex_2 : Vector2_Access) return Boolean;

   -----------------------------------------------------------------------------
   -- Introduces the missing boundary segments by swapping the diagonals between
   -- adjacent triangles.
   -----------------------------------------------------------------------------
   procedure Insert_Missing_Segments;

   -----------------------------------------------------------------------------
   -- Recursively inserts the triangles inside the given pseudo-polygon, until
   -- the stack is empty. This method is protected against deadlocks by a
   -- recursion limit.
   -----------------------------------------------------------------------------
   procedure Complete_Pseudo_Polygon (Polygon : in out Math.Vector2_Vector.Vector;
                                      Vertex_A, Vertex_B : Vector2_Access);

   -----------------------------------------------------------------------------
   -- Indicates if the provided triangle is ouside of the polygon by scanning
   -- the sides and the segments (deprecated because of its inefficiency).
   -----------------------------------------------------------------------------
   function Is_Outside_Polygon (Triangle : Triangle_Access) return Boolean;

   -----------------------------------------------------------------------------
   -- Removes the unnesesary triangles that are not contained in the original
   -- polygon.
   -----------------------------------------------------------------------------
   procedure Clean_Mesh;

end Meshing.Delaunay;
--------------------------------------------------------------------------------
