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
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Text_IO;
with Interfaces.C;
use  Interfaces.C;
-- Gnav
with Flight.Plan;
use  Flight.Plan;
with Gl;
use  Gl;
with Gl.Fonts;
with Gl.Shaders;
with Gl.Resources;
with Maps;
use  Maps;
with Utility.Strings;
with Utility.Colors;
use  Utility.Colors;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Route is

   Waypoints_Id : Gl_Uint;

   Airplane_Id  : Gl_Uint;

   Vectors_Id   : Gl_Uint;

   -- Fonts
   ---------------------------------
   Font_1 : Gl.Fonts.Font_Style_Record := (Width     => 0.008,
                                           Height    => 0.020,
                                           Space     => 0.005,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular,
                                           Line_R    => 1.0,
                                           Line_G    => 0.3,
                                           Line_B    => 1.0,
                                           Glow_R    => 0.2,
                                           Glow_G    => 0.1,
                                           Glow_B    => 0.2);

   --===========================================================================
   --
   --===========================================================================
   procedure Init is

      use Gl;

      Airplane_Buffer : Gl_Float_Vec (1..12);

   begin

      Airplane_Buffer (1)  :=  2.0;
      Airplane_Buffer (2)  :=  0.0;

      Airplane_Buffer (3)  := -4.0;
      Airplane_Buffer (4)  :=  0.0;

      Airplane_Buffer (5)  :=  0.0;
      Airplane_Buffer (6)  :=  4.0;

      Airplane_Buffer (7)  :=  0.0;
      Airplane_Buffer (8)  := -4.0;

      Airplane_Buffer (9)  := -4.0;
      Airplane_Buffer (10) :=  1.0;

      Airplane_Buffer (11) := -4.0;
      Airplane_Buffer (12) := -1.0;

      Gl.Resources.Update_Resource (Airplane_Id, Airplane_Buffer'Unrestricted_Access);

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Waypoints (View : Map_View_Record) is

      use Gl;
      use Gl.Fonts;
      use Utility.Strings;

      Point  : Point_Record;

      Buffer : Gl_Float_Vec (1..2 * Flight_Plan.Waypoints'Length);

      Size   : Gl_Sizei;

   begin

      Size := 0;

      for I in Flight_Plan.Waypoints'Range loop

         exit when not Flight_Plan.Waypoints (I).Is_Loaded;

         Point := View.To_Screen_Coordinates (Flight_Plan.Waypoints (I).Position);

         Buffer (Integer (2 * I - 1)) := Float (Point.Get_X);

         Buffer (Integer (2 * I))     := Float (Point.Get_Y);

         Size := Size + 1;

         if I = Waypoint_Range'First then

            Font_1.Line_R := 0.0;
            Font_1.Line_G := 1.0;
            Font_1.Line_B := 0.0;

            Font_1.Glow_R := 0.0;
            Font_1.Glow_G := 0.2;
            Font_1.Glow_B := 0.0;

         elsif Flight_Plan.Waypoints (I).Is_Active then

            Font_1.Line_R := 1.0;
            Font_1.Line_G := 0.0;
            Font_1.Line_B := 1.0;

            Font_1.Glow_R := 0.2;
            Font_1.Glow_G := 0.1;
            Font_1.Glow_B := 0.2;

         else

            Font_1.Line_R := 0.0;
            Font_1.Line_G := 1.0;
            Font_1.Line_B := 1.0;

            Font_1.Glow_R := 0.0;
            Font_1.Glow_G := 0.0;
            Font_1.Glow_B := 0.2;

         end if;

         Gl.Fonts.Draw (Trim (Flight_Plan.Waypoints (I).Name),
                        Float (Point.Get_X),
                        Float (Point.Get_Y + 0.02),
                        Font_1,
                        Alignment_LC);

      end loop;

      Gl.Resources.Update_Resource (Waypoints_Id, Buffer'Unrestricted_Access, Natural (2 * Size));

      Gl.Bind_Buffer (GL_ARRAY_BUFFER, Waypoints_Id);

      Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

      -- Points

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (Color_Black.R,
                             Color_Black.G,
                             Color_Black.B,
                             Color_Black.A);

      Gl.Shaders.Load_Width (3.0);

      Gl.Draw_Arrays (GL_LINE_STRIP, 0, Size);

      Gl.Shaders.Load_Color (Color_White.R,
                             Color_White.G,
                             Color_White.B,
                             Color_White.A);

      Gl.Shaders.Load_Width (1.0);

      Gl.Draw_Arrays (GL_LINE_STRIP, 0, Size);

      -- Points

      Gl.Shaders.Bind_Shader (Gl.Shaders.Points_2D);

      Gl.Shaders.Load_Color (Color_Black.R,
                             Color_Black.G,
                             Color_Black.B,
                             Color_Black.A);

      Gl.Shaders.Load_Diameter (4.0);

      Gl.Draw_Arrays (GL_POINTS, 0, Size);

      Gl.Shaders.Load_Color (Color_White.R,
                             Color_White.G,
                             Color_White.B,
                             Color_White.A);

      Gl.Shaders.Load_Diameter (2.0);

      Gl.Draw_Arrays (GL_POINTS, 0, Size);

   end Draw_Waypoints;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Airplane (View : Map_View_Record) is

      use Ada.Numerics;
      use Ada.Numerics.Long_Elementary_Functions;

      Point  : Point_Record;

      M1, M2 : Gl.Gl_Mat_4 := Gl.Shaders.Get_Active_Matrix;

      Angle  : Float := Flight.Data.Course;

   begin

      Angle := 90.0 - Angle;

      if Angle < 0.0 then

         Angle := Angle + 360.0;

      end if;

      Angle := Angle * Pi / 180.0;

      Point := View.To_Screen_Coordinates (Flight.Data.Position);

      Translate (M2, Float (Point.Get_X), Float (Point.Get_Y), 0.0);

      Scale     (M2, 0.008, 0.008 * View.Width / View.Height, 1.0);

      Rotate    (M2, Angle);

      Gl.Shaders.Load_Matrix (M2);

      Gl.Bind_Buffer (GL_ARRAY_BUFFER, Airplane_Id);

      Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (Color_Black.R,
                             Color_Black.G,
                             Color_Black.B,
                             Color_Black.A);

      Gl.Shaders.Load_Width (3.0);

      Gl.Draw_Arrays (GL_LINES, 0, 6);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Points_2D);

      Gl.Shaders.Load_Diameter (2.5, 0.8);

      Gl.Draw_Arrays (GL_POINTS, 0, 6);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (Color_White.R,
                             Color_White.G,
                             Color_White.B,
                             Color_White.A);

      Gl.Shaders.Load_Width (1.0);

      Gl.Draw_Arrays (GL_LINES, 0, 6);

      Gl.Shaders.Load_Matrix (M1);

   end Draw_Airplane;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Vectors (View : Map_View_Record) is

      use Gl;
      use Gl.Fonts;
      use Utility.Strings;

      Point  : Point_Record;

      Buffer : Gl_Float_Vec (1..6);

   begin

      if
        Flight.Plan.Next_Waypoint.Position /= No_Position_Record and then
        Flight.Data.Position /= No_Position_Record
      then

         Point := View.To_Screen_Coordinates (Flight.Plan.Next_Waypoint.Position);

         Buffer (1) := Float (Point.Get_X);

         Buffer (2) := Float (Point.Get_Y);

         Point := View.To_Screen_Coordinates (Flight.Data.Position);

         Buffer (3) := Float (Point.Get_X);

         Buffer (4) := Float (Point.Get_Y);

         Point := View.To_Screen_Coordinates (Flight.Plan.Home_Waypoint.Position);

         Buffer (5) := Float (Point.Get_X);

         Buffer (6) := Float (Point.Get_Y);

         Gl.Resources.Update_Resource (Vectors_Id, Buffer'Unrestricted_Access);

         Gl.Bind_Buffer (GL_ARRAY_BUFFER, Vectors_Id);

         Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

         Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

         Gl.Shaders.Load_Width (3.0, 0.6);

         Gl.Shaders.Load_Color (Color_Gray_3.R,
                                Color_Gray_3.G,
                                Color_Gray_3.B,
                                1.0);

         if not Flight_Plan.Waypoints (1).Is_Active then

            Gl.Draw_Arrays (GL_LINES, 0, 2);

         end if;

         Gl.Draw_Arrays (GL_LINES, 1, 2);

         Gl.Shaders.Load_Width (1.0, 0.8);

         if not Flight_Plan.Waypoints (1).Is_Active then

            Gl.Shaders.Load_Color (Color_Magenta.R,
                                   Color_Magenta.G,
                                   Color_Magenta.B,
                                   1.0);

            Gl.Draw_Arrays (GL_LINES, 0, 2);

         end if;

         Gl.Shaders.Load_Color (Color_Green.R,
                                Color_Green.G,
                                Color_Green.B,
                                1.0);

         Gl.Draw_Arrays (GL_LINES, 1, 2);

      end if;

   end Draw_Vectors;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw (View : Map_View_Record) is
   begin

      Draw_Vectors   (View);

      Draw_Waypoints (View);

      Draw_Airplane  (View);

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin

      null;

   end Screen_Pressed;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin

      null;

   end Key_Changed;
   -----------------------------------------------------------------------------

end Display.Route;
--------------------------------------------------------------------------------
