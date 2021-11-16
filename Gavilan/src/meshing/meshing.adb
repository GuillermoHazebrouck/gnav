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
with Ada.Text_IO;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

--//////////////////////////////////////////////////////////////////////////////
-- This namespace contains a meshing algorithm used to generate triangles over
-- a polygon, given as a sequence of consecutive points.
-- The resulting mesh is the constrained Delaunay triangulation of the
-- provided set of points, which means that every triangle that is not adjacent
-- to the boundary excludes all vertices from inside its circumcircle.
--//////////////////////////////////////////////////////////////////////////////
package body Meshing is

   Count : Natural := 0;

   Total_Count : Natural := 0;

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Status_Message return String is
   begin

      return To_String (Status_Message);

   end;

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Progress_Fraction return Float is
   begin

      return Progress_Fraction;

   end;

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Status return Meshing_Status_Kinds is
   begin

      return Status;

   end;

   procedure Publish_Start is
   begin

      if Publish_Progress then

         Progress_Fraction := 0.0;

         Status := Running;

         On_Started.all;

      end if;

   end;

   procedure Publish_Done is
   begin

      if Publish_Progress then

         Total_Count := 0;

         Count := 0;

         Progress_Fraction := 1.0;

         Status := Iddle;

         On_Finished.all;

      end if;

   end;

   --===========================================================================
   --
   --===========================================================================
   procedure Reset_Progress (Total : Natural) is
   begin

      if Publish_Progress then

         Total_Count := Total;

         Count := 0;

         Progress_Fraction := 0.0;

         On_Progress_Changed.all;

      end if;

   end;

   --===========================================================================
   --
   --===========================================================================
   procedure Push_Progress is
   begin

      if Publish_Progress then

         Count := Count + 1;

         if Total_Count > 0 then

            Progress_Fraction := Float'Min (1.0, Float (Count) / Float (Total_Count));

         else

            Progress_Fraction := 0.0;

         end if;

         On_Progress_Changed.all;

      end if;

   end;

   --===========================================================================
   --
   --===========================================================================
   procedure Set_Status_Message (Message : String) is
   begin

      if Publish_Progress then

         if Verbosity_Level > 1 then

            Ada.Text_IO.Put_Line (Message);

         end if;

         Status_Message := To_Unbounded_String (Message);

      end if;

   end;

end Meshing;
