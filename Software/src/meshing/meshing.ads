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

-- Local
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

--//////////////////////////////////////////////////////////////////////////////
-- This namespace contains a meshing algorithm used to generate triangles over
-- a polygon, given as a sequence of consecutive points.
-- The resulting mesh is the constrained Delaunay triangulation of the
-- provided set of points, which means that every triangle that is not adjacent
-- to the boundary excludes all vertices from inside its circumcircle.
--//////////////////////////////////////////////////////////////////////////////
package Meshing is

   type Callback is access procedure;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Occurs when the mesher starts meshing
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   On_Started  : Callback;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Occurs when the finished meshing
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   On_Finished : Callback;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Occurs when a progress is made in the meshing algorithm
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   On_Progress_Changed : Callback;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The level of verbosity. 0 means no verbosity, 1 will at least output all
   -- severe errors and 2 will at least output all severe warnings. 3 will
   -- aditionally output debug useful comments.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Verbosity_Level : Short_Integer := 0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The two possible states of the mesher.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Meshing_Status_Kinds is (Running, Iddle);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The available methods to clean the mesh.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Meshing_Cleaning_Kinds is (Polygon_Based, Adjacency_Base);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The normalized distance at which two entities will be considered to be in
   -- proximty.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Proximity_Threshold : Long_Float := 1.0E-8;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The normalized distance at which two entities will be merged. This must be
   -- higher than the Proximity_Threshold.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Fusion_Threshold : Long_Float := 1.0E-6;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The method to be used for removing the triangles that are outside the
   -- given polygon.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Cleaning_Method : Meshing_Cleaning_Kinds := Polygon_Based;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the algorithm was forcibly stoped.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Abort_Meshing : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the progress can be published while generationg the mesh.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Publish_Progress : Boolean := True;

   --===========================================================================
   -- Returns the current status message.
   --===========================================================================
   function Get_Status_Message return String;

   --===========================================================================
   -- Returns the current status message.
   --===========================================================================
   function Get_Progress_Fraction return Float;

   --===========================================================================
   -- Returns the current status.
   --===========================================================================
   function Get_Status return Meshing_Status_Kinds;

private

   --===========================================================================
   --
   --===========================================================================
   procedure Publish_Start;

   --===========================================================================
   --
   --===========================================================================
   procedure Publish_Done;

   --===========================================================================
   --
   --===========================================================================
   procedure Reset_Progress (Total : Natural);

   --===========================================================================
   --
   --===========================================================================
   procedure Push_Progress;

   --===========================================================================
   --
   --===========================================================================
   procedure Set_Status_Message (Message : String);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The cumulated progress.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Progress_Fraction : Float;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The cumulated progress.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Status : Meshing_Status_Kinds := Iddle;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The curent status message.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Status_Message : Unbounded_String;

end Meshing;
--------------------------------------------------------------------------------
