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
with Flight;
use  Flight;
with Flight.Plan;
use  Flight.Plan;
with Flight.Traffic;
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
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Representation is

   -- Vertex buffer Id's
   ---------------------------------

   Waypoints_Id : Gl_Uint;

   Airplane_Id  : Gl_Uint;

   Vectors_Id   : Gl_Uint;

   Eyes_Id      : Gl_Uint;

   -- Fonts
   ---------------------------------
   Font : Gl.Fonts.Font_Style_Record := (Width     => 0.008,
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

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The number of clusters
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Cluster_Range is Positive range 1..80;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The current cluster
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   C : Cluster_Range := Cluster_Range'First;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The identification of the cluster resources on the GPU
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Cluster_Record is record

      Id   : Gl_Uint;

      Size : Gl_Sizei;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Cluster_Record : constant Cluster_Record := (Id   => 0,
                                                   Size => 0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Cluster_Array is array (Cluster_Range) of Cluster_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The GPU data resources
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Clusters : Cluster_Array;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The maximum number of points to load in a cluster
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Buffer_Size : constant Gl_Sizei := 30;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Buffer size
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Buffer_Range is Natural range 1..2 * Natural (Buffer_Size);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- When the buffer reaches this size (or a bit more), the cluster is no
   -- longer updated and a new one is allocated.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Size_Threshold : constant Gl_Sizei := 20;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Buffer index
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   B : Buffer_Range := Buffer_Range'Last;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The local storage of points to be loaded in the GPU
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Buffer : Gl_Float_Vec (Buffer_Range);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the cluster should be updated due to one or more new points
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Point_Pending : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Reset_Pending : Boolean := False;

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Trajectory_Point is
   begin

      if Data.Is_Update (Field_Position) then

         if B = Buffer_Range'Last then

            Buffer (1) := Gl_Float (Data.Position.Lon);

            Buffer (2) := Gl_Float (Data.Position.Lat);

            B := 2;

         else

            B := B + 1;

            Buffer (B) := Gl_Float (Data.Position.Lon);

            B := B + 1;

            Buffer (B) := Gl_Float (Data.Position.Lat);

         end if;

         Point_Pending := True;

         Utility.Log.Put_Message ("loaded trajectory point" & Cluster_Range'Image (C) & Buffer_Range'Image (B));

      end if;

   end Load_Trajectory_Point;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Reset_Flight_Path is

      Count      : Gl_Sizei := 0;
      Remove_Ids : Gl_Uint_Vec (Cluster_Range) := (others => 0);

   begin

      if Reset_Pending then

         for I in Cluster_Range loop

            if Clusters (I).Id > 0 then

               Count := Count + 1;

               Remove_Ids (Cluster_Range (Count)) := Clusters (I).Id;

            end if;

         end loop;

         Clusters := (others => No_Cluster_Record);

         Gl.Delete_Buffers (Count,
                            Remove_Ids'Unrestricted_Access);

         B := Buffer_Range'Last;

         Reset_Pending := False;

         Point_Pending := False;

         Utility.Log.Put_Message ("trajectory reset");

      end if;

   end Reset_Flight_Path;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Reset_Pending is
   begin

      Reset_Pending := True;

   end Set_Reset_Pending;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Init is

      use Gl;

      Airplane_Buffer : Gl_Float_Vec (1..12);

      Eyes_Buffer     : Gl_Float_Vec (1..6);

   begin

      Flight.On_Data_Cached.Connect (Load_Trajectory_Point'Access);

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

      --

      Eyes_Buffer (1)  :=  1.0;
      Eyes_Buffer (2)  :=  0.5;

      Eyes_Buffer (3)  :=  0.0;
      Eyes_Buffer (4)  :=  0.0;

      Eyes_Buffer (5)  :=  1.0;
      Eyes_Buffer (6)  := -0.5;

      Gl.Resources.Update_Resource (Eyes_Id, Eyes_Buffer'Unrestricted_Access);

      -- Listen to replay reset
      ----------------------------------------------------
      On_Replay_Reset.Connect (Set_Reset_Pending'Access);

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Flush_Path is
   begin

      Reset_Flight_Path;

      if Point_Pending then

         -- Load the buffer
         -----------------------------------------------

         Gl.Resources.Update_Resource (Clusters (C).Id, Buffer'Unrestricted_Access, B);

         Clusters (C).Size := Gl_Sizei (B) / 2;

         Point_Pending := False;

         -- Jump to the next cluster if necessary
         -----------------------------------------------
         if Clusters (C).Size >= Size_Threshold then

            Utility.Log.Put_Message ("new cluster" & Cluster_Range'Image (C));

            if C = Cluster_Range'Last then

               C := 1;

            else

               C := C + 1;

            end if;

            -- Initialize next buffer with the last point
            ---------------------------------------------
            if B > Buffer_Range'First then

               Buffer (1) := Gl_Float (Buffer (B - 1));

               Buffer (2) := Gl_Float (Buffer (B));

               B := 2;

            end if;

         end if;

      end if;

   end Flush_Path;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw_Trajectory (View : Map_View_Record) is

      M1 : Gl_Mat_4 := Gl.Shaders.Get_Active_Matrix;

      M2 : Gl_Mat_4 := View.Geographic_Matrix;

   begin

      Reset_Flight_Path;

      Gl.Shaders.Load_Matrix (M2);

      for I in Cluster_Range loop

         if Clusters (I).Id > 0 then

            Gl.Bind_Buffer (GL_ARRAY_BUFFER, Clusters (I).Id);

            Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

            Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

            Gl.Shaders.Load_Width (2.0);

            Gl.Shaders.Load_Color (0.2, 0.2, 0.2, 1.0);

            Gl.Draw_Arrays (GL_LINE_STRIP, 0, Clusters (I).Size);

         end if;

      end loop;

      Gl.Shaders.Load_Matrix (M1);

   end Draw_Trajectory;
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

            Font.Line_R := 0.0;
            Font.Line_G := 1.0;
            Font.Line_B := 0.0;

            Font.Glow_R := 0.0;
            Font.Glow_G := 0.2;
            Font.Glow_B := 0.0;

         elsif Flight_Plan.Waypoints (I).Is_Active then

            Font.Line_R := 1.0;
            Font.Line_G := 0.0;
            Font.Line_B := 1.0;

            Font.Glow_R := 0.2;
            Font.Glow_G := 0.1;
            Font.Glow_B := 0.2;

         else

            Font.Line_R := 0.0;
            Font.Line_G := 1.0;
            Font.Line_B := 1.0;

            Font.Glow_R := 0.0;
            Font.Glow_G := 0.0;
            Font.Glow_B := 0.2;

         end if;

         Gl.Fonts.Draw (Trim (Flight_Plan.Waypoints (I).Name),
                        Float (Point.Get_X),
                        Float (Point.Get_Y + 0.02),
                        Font,
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
   procedure Draw_Eye (View     : Map_View_Record;
                       Position : Position_Record;
                       Course   : Float) is

      use Ada.Numerics;
      use Ada.Numerics.Long_Elementary_Functions;

      Point  : Point_Record;

      M1, M2 : Gl.Gl_Mat_4 := Gl.Shaders.Get_Active_Matrix;

      Angle  : Float       := Course;

   begin

      Angle := 90.0 - Angle;

      if Angle < 0.0 then

         Angle := Angle + 360.0;

      end if;

      Angle := Angle * Pi / 180.0;

      Point := View.To_Screen_Coordinates (Position);

      Translate (M2, Float (Point.Get_X), Float (Point.Get_Y), 0.0);

      Scale     (M2, 0.15, 0.15 * View.Width / View.Height, 1.0);

      Rotate    (M2, Angle);

      Gl.Shaders.Load_Matrix (M2);

      Gl.Bind_Buffer (GL_ARRAY_BUFFER, Eyes_Id);

      Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Monochrome_2D);

      Gl.Shaders.Load_Color (0.0,
                             0.5,
                             1.0,
                             0.5);

      Gl.Draw_Arrays (GL_TRIANGLES, 0, 3);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (0.7,
                             0.7,
                             0.7,
                             1.0);

      Gl.Shaders.Load_Width (1.0);

      Gl.Draw_Arrays (GL_LINE_LOOP, 0, 3);

      Gl.Shaders.Load_Matrix (M1);

   end Draw_Eye; pragma Inline (Draw_Eye);
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Airplane (View     : Map_View_Record;
                            Position : Position_Record;
                            Course   : Float;
                            Color    : Color_Record := Color_White;
                            Shadow   : Color_Record := Color_Black) is

      use Ada.Numerics;
      use Ada.Numerics.Long_Elementary_Functions;

      Point  : Point_Record;

      M1, M2 : Gl.Gl_Mat_4 := Gl.Shaders.Get_Active_Matrix;

      Angle  : Float       := Course;

   begin

      Angle := 90.0 - Angle;

      if Angle < 0.0 then

         Angle := Angle + 360.0;

      end if;

      Angle := Angle * Pi / 180.0;

      Point := View.To_Screen_Coordinates (Position);

      Translate (M2, Float (Point.Get_X), Float (Point.Get_Y), 0.0);

      Scale     (M2, 0.008, 0.008 * View.Width / View.Height, 1.0);

      Rotate    (M2, Angle);

      Gl.Shaders.Load_Matrix (M2);

      Gl.Bind_Buffer (GL_ARRAY_BUFFER, Airplane_Id);

      Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (Shadow.R,
                             Shadow.G,
                             Shadow.B,
                             Shadow.A);

      Gl.Shaders.Load_Width (3.0);

      Gl.Draw_Arrays (GL_LINES, 0, 6);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Points_2D);

      Gl.Shaders.Load_Diameter (2.5, 0.8);

      Gl.Draw_Arrays (GL_POINTS, 0, 6);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (Color.R,
                             Color.G,
                             Color.B,
                             Color.A);

      Gl.Shaders.Load_Width (1.0);

      Gl.Draw_Arrays (GL_LINES, 0, 6);

      Gl.Shaders.Load_Matrix (M1);

   end Draw_Airplane; pragma Inline (Draw_Airplane);
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Airplane (View : Map_View_Record) is
   begin

      Draw_Eye      (View, Flight.Data.Position, Flight.Data.Course);

      Draw_Airplane (View, Flight.Data.Position, Flight.Data.Course);

   end Draw_Airplane;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Traffic (View : Map_View_Record) is

      use Flight.Traffic;

      Color : Color_Record;

   begin

      for T in Traffic_Range loop

         if Traffic_Data (T).Active then

            if Traffic_Data (T).Coasted then

               Color := Color_Cyan;

            else

               Color := Color_Reddish;

            end if;

            Draw_Airplane (View,
                           Traffic_Data (T).Position,
                           Traffic_Data (T).Course,
                           Color,
                           Color_Gray_4);

         end if;

      end loop;

   end Draw_Traffic;
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

      Draw_Trajectory (View);

      Draw_Vectors    (View);

      Draw_Waypoints  (View);

      Draw_Traffic    (View);

      Draw_Airplane   (View);

   end Draw;
   -----------------------------------------------------------------------------

end Flight.Representation;
--------------------------------------------------------------------------------
