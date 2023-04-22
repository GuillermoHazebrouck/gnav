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
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
-- Gnav
with Flight;
with Flight.Plan;
with Gl;
use  Gl;
with Gl.Colors;
use  Gl.Colors;
with Gl.Fonts;
with Gl.Shaders;
with Gl.Resources;
with Utility.Strings;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Compass is

   -- Needle vertex ids
   ---------------------------

   Waypoint_Id : Gl_Uint;

   North_Id    : Gl_Uint;

   -- Quadrant vertex data
   ---------------------------

   Quadrant_Id : Gl_Uint;

   Rosetta_Id  : Gl_Uint;

   Quadrant_Size : constant Natural := 36;

   Allocation : Allocation_Record;

   --===========================================================================
   --
   --===========================================================================
   procedure Init is

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;

      S : constant Float := 2.0 * Pi / Float (Quadrant_Size);

      Quadrant_Buffer : Gl.Gl_Float_Vec (1 .. 2 * Quadrant_Size);

      Rosetta_Buffer  : Gl.Gl_Float_Vec (1 .. 20);

      Angle : Float := 0.0;

      North_Buffer    : Gl.Gl_Float_Vec (1 .. 6);

      Waypoint_Buffer : Gl.Gl_Float_Vec (1 .. 6);

   begin

      -- Load quadrant buffer in GPU
      --------------------------------------------------------------------------

      for I in 1..Quadrant_Size loop

         Quadrant_Buffer (2 * I - 1) := 0.5 * Cos (Angle);
         Quadrant_Buffer (2 * I    ) := 0.5 * Sin (Angle);

         Angle := Angle + S;

      end loop;

      Gl.Resources.Update_Resource (Quadrant_Id, Quadrant_Buffer'Unrestricted_Access);

      -- Rosetta (NESW deshes)
      --------------------------------------------------------------------------

      Rosetta_Buffer (1)  := 0.55;
      Rosetta_Buffer (2)  := 0.00;
      Rosetta_Buffer (3)  := 0.40;
      Rosetta_Buffer (4)  := 0.00;

      Rosetta_Buffer (5)  :=-0.55;
      Rosetta_Buffer (6)  := 0.00;
      Rosetta_Buffer (7)  :=-0.40;
      Rosetta_Buffer (8)  := 0.00;

      Rosetta_Buffer (9)  := 0.00;
      Rosetta_Buffer (10) := 0.55;
      Rosetta_Buffer (11) := 0.00;
      Rosetta_Buffer (12) := 0.40;

      Rosetta_Buffer (13) := 0.00;
      Rosetta_Buffer (14) :=-0.55;
      Rosetta_Buffer (15) := 0.00;
      Rosetta_Buffer (16) :=-0.40;

      Rosetta_Buffer (17) := 0.00;
      Rosetta_Buffer (18) := 0.20;
      Rosetta_Buffer (19) := 0.00;
      Rosetta_Buffer (20) := 0.45;

      Gl.Resources.Update_Resource (Rosetta_Id, Rosetta_Buffer'Unrestricted_Access);

      -- Load north arrow buffer in GPU
      --------------------------------------------------------------------------

      North_Buffer (1) := 0.00;
      North_Buffer (2) := 0.60;

      North_Buffer (3) := 0.10;
      North_Buffer (4) := 0.45;

      North_Buffer (5) :=-0.10;
      North_Buffer (6) := 0.45;

      Gl.Resources.Update_Resource (North_Id, North_Buffer'Unrestricted_Access);

      -- Load waypoint arrow buffer in GPU
      --------------------------------------------------------------------------

      Waypoint_Buffer (1) := 0.00;
      Waypoint_Buffer (2) := 0.50;

      Waypoint_Buffer (3) := 0.07;
      Waypoint_Buffer (4) := 0.34;

      Waypoint_Buffer (5) :=-0.07;
      Waypoint_Buffer (6) := 0.34;

      Gl.Resources.Update_Resource (Waypoint_Id, Waypoint_Buffer'Unrestricted_Access);

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw (X, Y, Size, Aspect : Float; Font : Fonts.Font_Style_Record) is

      use Flight;
      use Ada.Numerics;

      M1 : Gl.Gl_Mat_4 := Gl.Shaders.Get_Active_Matrix;

      M2 : Gl.Gl_Mat_4 := Gl.Gl_Mat_4_Identity;

      M3 : Gl.Gl_Mat_4 := Gl.Gl_Mat_4_Identity;

      Course : Float := 0.0;

      Angle  : Float;

   begin

      if Flight.Data.Is_Recent (Field_Course) then

         Course := Flight.Data.Course * Pi / 180.0;

      end if;

      Translate (M2, X, Y, 0.0);

      Scale     (M2, Size, Size * Aspect, 1.0);

      Rotate    (M2, Course);

      M2 := Multiply  (M1, M2);

      Gl.Shaders.Load_Matrix (M2);

      Gl.Bind_Buffer (GL_ARRAY_BUFFER, Quadrant_Id);

      Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Monochrome_2D);

      Gl.Shaders.Load_Color (Color_Black.R,
                             Color_Black.G,
                             Color_Black.B,
                             0.5);

      Gl.Draw_Arrays (GL_TRIANGLE_FAN, 0, Gl_Sizei (Quadrant_Size));

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (Color_Black.R,
                             Color_Black.G,
                             Color_Black.B,
                             Color_Black.A);

      Gl.Shaders.Load_Width (3.0);

      Gl.Draw_Arrays (GL_LINE_LOOP, 0, Gl_Sizei (Quadrant_Size));

      Gl.Shaders.Bind_Shader (Gl.Shaders.Points_2D);

      Gl.Shaders.Load_Diameter (2.5);

      Gl.Draw_Arrays (GL_POINTS, 0, Gl_Sizei (Quadrant_Size));

      Gl.Shaders.Load_Color (Color_Gray_6.R,
                             Color_Gray_6.G,
                             Color_Gray_6.B,
                             Color_Gray_6.A);

      Gl.Shaders.Load_Diameter (1.5);

      Gl.Draw_Arrays (GL_POINTS, 0, Gl_Sizei (Quadrant_Size));

      -- N-E-S-W marks
      --------------------------------------------------------------------------

      Gl.Bind_Buffer (GL_ARRAY_BUFFER, Rosetta_Id);

      Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

      -- Reference mark

      Gl.Shaders.Bind_Shader (Gl.Shaders.Monochrome_2D);

      Gl.Shaders.Load_Color (Color_Black.R,
                             Color_Black.G,
                             Color_Black.B,
                             Color_Black.A);

      M3 := Gl_Mat_4_Identity;

      Translate (M3, X, Y, 0.0);

      Scale     (M3, Size, Size * Aspect, 1.0);

      M3 := Multiply  (M1, M3);

      Gl.Shaders.Load_Matrix (M3);

      Gl.Line_Width (3.0);

      Gl.Shaders.Load_Color (Line_Cyan.Glow);

      Gl.Draw_Arrays (GL_LINES, 8, 2);

      Gl.Line_Width (1.0);

      Gl.Shaders.Load_Color (Line_Cyan.Fore);

      Gl.Draw_Arrays (GL_LINES, 8, 2);

      -- End reference mark

      Gl.Shaders.Load_Matrix (M2);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (Color_Black.R,
                             Color_Black.G,
                             Color_Black.B,
                             Color_Black.A);

      Gl.Shaders.Load_Width (3.0);

      Gl.Draw_Arrays (GL_LINES, 0, 8);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Points_2D);

      Gl.Shaders.Load_Diameter (3.0);

      Gl.Draw_Arrays (GL_POINTS, 0, 8);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (Color_White.R,
                             Color_White.G,
                             Color_White.B,
                             Color_White.A);

      Gl.Shaders.Load_Width (1.0);

      Gl.Draw_Arrays (GL_LINES, 0, 8);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Points_2D);

      Gl.Shaders.Load_Diameter (1.0);

      Gl.Draw_Arrays (GL_POINTS, 0, 8);

      -- North arrow
      --------------------------------------------------------------------------

      Gl.Bind_Buffer (GL_ARRAY_BUFFER, North_Id);

      Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Monochrome_2D);

      Gl.Shaders.Load_Color (1.0, 1.0, 1.0, 1.0);

      Gl.Draw_Arrays (GL_TRIANGLES, 0, 3);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (0.0, 0.0, 0.0, 1.0);

      Gl.Shaders.Load_Width (1.5);

      Gl.Draw_Arrays (GL_LINE_LOOP, 0, 3);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Points_2D);

      Gl.Shaders.Load_Diameter (1.5);

      Gl.Draw_Arrays (GL_POINTS, 0, 3);

      if Flight.Data.Is_Recent (Field_Course) then

         -- Waypoint arrow
         --------------------------------------------------------------------------

         Angle := Flight.Data.Course - Flight.Plan.Next_Waypoint.Bearing;

         if Angle < 0.0 then

            Angle := 360.0 + Angle;

         end if;

         Angle := Angle * Pi / 180.0;

         M2 := Gl_Mat_4_Identity;

         Translate (M2, X, Y, 0.0);

         Scale     (M2, Size, Size * Aspect, 1.0);

         Rotate    (M2, Angle);

         M2 := Multiply  (M1, M2);

         Gl.Shaders.Load_Matrix (M2);

         Gl.Bind_Buffer (GL_ARRAY_BUFFER, Waypoint_Id);

         Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

         Gl.Shaders.Bind_Shader (Gl.Shaders.Monochrome_2D);

         Gl.Shaders.Load_Color (1.0, 0.3, 1.0, 1.0);

         Gl.Draw_Arrays (GL_TRIANGLES, 0, 3);

         Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

         Gl.Shaders.Load_Color (0.4, 0.1, 0.4, 1.0);

         Gl.Shaders.Load_Width (1.0);

         Gl.Draw_Arrays (GL_LINE_LOOP, 0, 3);

         -- Home arrow
         --------------------------------------------------------------------------

         Angle := Flight.Data.Course - Flight.Plan.Home_Waypoint.Bearing;

         if Angle < 0.0 then

            Angle := 360.0 + Angle;

         end if;

         Angle := Angle * Pi / 180.0;

         M2 := Gl_Mat_4_Identity;

         Translate (M2, X, Y, 0.0);

         Scale     (M2, Size, Size * Aspect, 1.0);

         Rotate    (M2, Angle);

         M2 := Multiply  (M1, M2);

         Gl.Shaders.Load_Matrix (M2);

         Gl.Bind_Buffer (GL_ARRAY_BUFFER, Waypoint_Id);

         Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

         Gl.Shaders.Bind_Shader (Gl.Shaders.Monochrome_2D);

         Gl.Shaders.Load_Color (0.3, 1.0, 0.30, 1.0);

         Gl.Draw_Arrays (GL_TRIANGLES, 0, 3);

         Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

         Gl.Shaders.Load_Color (0.1, 0.4, 0.1, 1.0);

         Gl.Shaders.Load_Width (1.0);

         Gl.Draw_Arrays (GL_LINE_LOOP, 0, 3);

      end if;

      -- Course indicator
      --------------------------------------------------------------------------

      Gl.Shaders.Load_Matrix (M1);

      if Flight.Data.Is_Recent (Field_Course) then

         Gl.Fonts.Draw (Utility.Strings.Float_Image (Flight.Data.Course, 0),
                        X,
                        Y,
                        Font,
                        Line_Cyan,
                        Alignment_CC);

      else

         Gl.Fonts.Draw ("-",
                        X,
                        Y,
                        Font,
                        Line_Red,
                        Alignment_CC);

      end if;

      -- Allocation
      --------------------------------------------------------------------------

      Allocation.X := X - 0.5 * Size;
      Allocation.W := Size;
      Allocation.Y := Y - 0.5 * Size * Aspect;
      Allocation.H := Size * Aspect;

   end Draw;
   -----------------------------------------------------------------------------

end Display.Compass;
--------------------------------------------------------------------------------
