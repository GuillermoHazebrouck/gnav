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
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Long_Elementary_Functions;
-- Gnav
with Gl.Colors;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps is

    package Math renames Ada.Numerics.Elementary_Functions;

    Deg_Chr : constant Character := '*';
    Min_Chr : constant Character := ''';
    Seg_Chr : constant Character := '"';
    Dec_Chr : constant Character := '.';

    Deg_Str : constant String := (1 => Deg_Chr);
    Min_Str : constant String := (1 => Min_Chr);
    Seg_Str : constant String := (1 => Seg_Chr);
    Dec_Str : constant String := (1 => Dec_Chr);




   --===========================================================================
   --
   --===========================================================================
   function Sexagecimal_String (Value : Long_Float) return String is

      Degrees : Long_Float := Long_Float'Floor (abs Value);
      Minutes : Long_Float := 60.0 * (abs Value - Degrees);
      Seconds : Long_Float := 60.0 * (Minutes - Long_Float'Floor (Minutes));
      Decimal : Long_Float := 10.0 * (Seconds - Long_Float'Floor (Seconds));

   begin

      Minutes := Long_Float'Truncation (Minutes);

      return Utility.Strings.Trim (Integer'Image (Integer (Long_Float'Floor (Degrees)))) & Deg_Str &
             Utility.Strings.Trim (Integer'Image (Integer (Long_Float'Floor (Minutes)))) & Min_Str &
             Utility.Strings.Trim (Integer'Image (Integer (Long_Float'Floor (Seconds)))) & Dec_Str &
             Utility.Strings.Trim (Integer'Image (Integer (Long_Float'Floor (Decimal)))) & Seg_Str;

   end Sexagecimal_String;
   --------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Lat_String (Value : Long_Float) return String is
   begin
      if Value < 0.0 then
         return "S";
      else
         return "N";
      end if;
   end Lat_String;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Lon_String (Value : Long_Float) return String is
   begin
      if Value < 0.0 then
         return "W";
      else
         return "E";
      end if;
   end Lon_String;
   -----------------------------------------------------------------------------


   --===========================================================================
   --
   --===========================================================================
   function Lat_Image (Value : Position_Record) return String is
   begin
      return Lat_String (Value.Lat) & Sexagecimal_String (Value.Lat);
   end Lat_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Lon_Image (Value : Position_Record) return String is
   begin

      return Lon_String (Value.Lon) & Sexagecimal_String (Value.Lon);

   end Lon_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Image (Value : Position_Record) return String is
   begin

      return Lat_Image (Value) & " " & Lon_Image (Value);

   end Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Sexagecimal_Value (Image : String) return Long_Float is

      J      : Natural    := 0;
      Sign   : Long_Float := 0.0;
      Result : Long_Float := 0.0;

   begin

      for I in Image'First..Image'Last loop

         if Image (I) = 'N' or Image (I) = 'E' then

            Sign :=  1.0;

            J := I + 1;

         elsif Image (I) = 'W' or Image (I) = 'S' then

            Sign := -1.0;

            J := I + 1;

         elsif Image (I) = Deg_Chr and I > J then

            Result := Long_Float'Value (Image (J..I - 1));

            J := I + 1;

         elsif Image (I) = Min_Chr and I > J then

            Result := Result + Long_Float'Value (Image (J..I - 1)) / 60.0;

            J := I + 1;

         elsif Image (I) = Seg_Chr and I > J then

            Result := Result + Long_Float'Value (Image (J..I - 1)) / 3600.0;

            J := I + 1;

         end if;

      end loop;

      return Sign * Result;

   end Sexagecimal_Value;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Value (Image : String) return Position_Record is

      Result : Position_Record := (0.0, 0.0);
      J      : Positive := Image'First;

   begin

      for I in Image'First..Image'Last loop

         if Image (I) = ' ' then

            J := I;

            exit;

         end if;

      end loop;

      if J > Image'First and J < Image'Last then

         Result.Lat := Sexagecimal_Value (Image (Image'First..J-1));

         Result.Lon := Sexagecimal_Value (Image (J+1..Image'Last));

      end if;

      return Result;

   end Value;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Zoom_In (This : in out Map_View_Record) is
   begin

      This.Zoom := Float'Max (Lower_Zoom, This.Zoom - Zoom_Step);

      This.Shadow := This.Zoom < 0.0008;

   end Zoom_In;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Zoom_Out (This : in out Map_View_Record) is
   begin

      This.Zoom := Float'Min (Upper_Zoom, This.Zoom + Zoom_Step);

      This.Shadow := This.Zoom < 0.0008;

   end Zoom_Out;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function To_Screen_Coordinates (This : Map_View_Record; Position : Position_Record) return Point_Record is

      Point  : Point_Record;
      Zoom_X : Float := This.Zoom * This.Shrink;
      Zoom_Y : Float := This.Zoom;

   begin

      Point.Set_X ((Position.Lon - This.Center.Lon) / Long_Float (Zoom_X * This.Width)  + 0.5);

      Point.Set_Y ((Position.Lat - This.Center.Lat) / Long_Float (Zoom_Y * This.Height) + 0.5);

      return Point;

   end To_Screen_Coordinates;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function To_Map_Coordinates (This : Map_View_Record; Point : Point_Record) return Position_Record is

      Position : Position_Record;
      Zoom_X : Float := This.Zoom * This.Shrink;
      Zoom_Y : Float := This.Zoom;

   begin

      Position.Lon := Long_Float ((Point.Get_X - 0.5) * Long_Float (Zoom_X * This.Width))  + This.Center.Lon;

      Position.Lat := Long_Float ((Point.Get_Y - 0.5) * Long_Float (Zoom_Y * This.Height)) + This.Center.Lat;

      return Position;

   end To_Map_Coordinates;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Geographic_Matrix (This : Map_View_Record) return Gl.Gl_Mat_4 is

      Matrix : Gl.Gl_Mat_4 := Gl.Gl_Mat_4_Identity;

      Zoom_X : Float := This.Zoom * This.Width * This.Shrink;

      Zoom_Y : Float := This.Zoom * This.Height;

   begin

      Gl.Scale (Matrix, 2.0 / Zoom_X, 2.0 / Zoom_Y, 1.0);

      Gl.Translate (Matrix, Float (-This.Center.Lon), Float (-This.Center.Lat), 0.0);

      return Matrix;

   end Geographic_Matrix;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Get_Limits (This         : Map_View_Record;
                         Top_Left     : out Position_Record;
                         Bottom_Right : out Position_Record) is

      -- The zoom levels, in number of degrees/pixel
      -----------------------------------------------
      Zoom_X : Float := This.Zoom * This.Shrink;
      Zoom_Y : Float := This.Zoom;

   begin

      -- Compute screen limits
      -----------------------------------------------------------------------

      Top_Left.Lat     := This.Center.Lat + Long_Float (0.5 * Zoom_Y * This.Height);

      Top_Left.Lon     := This.Center.Lon - Long_Float (0.5 * Zoom_X * This.Width);

      Bottom_Right.Lat := This.Center.Lat - Long_Float (0.5 * Zoom_Y * This.Height);

      Bottom_Right.Lon := This.Center.Lon + Long_Float (0.5 * Zoom_X * This.Width);

   end Get_Limits;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Distance (Position_A, Position_B : Position_Record) return Float is

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;
      use Ada.Numerics.Long_Elementary_Functions;

      Delta_Lat : Long_Float := Position_B.Lat - Position_A.Lat;
      Delta_Lon : Long_Float := Position_B.Lon - Position_A.Lon;
      Midle_Lat : Long_Float := (Position_B.Lat + Position_A.Lat) * Pi / 360.0;

      Delta_X, Delta_Y : Float;

   begin

      Delta_Y := Float (Meridian_Length * Delta_Lat);

      Delta_X := Float (Equator_Length * Cos (Midle_Lat) * Delta_Lon);

      return Sqrt (Delta_X * Delta_X + Delta_Y * Delta_Y);

   end Distance;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Shrinkage (Latitude : Long_Float) return Long_Float is

      use Ada.Numerics;
      use Ada.Numerics.Long_Elementary_Functions;

   begin

      return Meridian_Length / (Equator_Length * Cos (Latitude * Pi / 180.0));

   end Shrinkage;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Coordinates (Position_A,
                          Position_B  : Position_Record;
                          Distance    : out Float;
                          Bearing     : out Float) is

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;
      use Ada.Numerics.Long_Elementary_Functions;

      Delta_Lat : Long_Float := Position_B.Lat - Position_A.Lat;
      Delta_Lon : Long_Float := Position_B.Lon - Position_A.Lon;
      Midle_Lat : Long_Float := (Position_B.Lat + Position_A.Lat) * Pi / 360.0;

      Delta_X, Delta_Y : Float;

   begin

      Delta_Y := Float (Meridian_Length * Delta_Lat);

      Delta_X := Float (Equator_Length  * Cos (Midle_Lat) * Delta_Lon);

      Distance := Sqrt (Delta_X * Delta_X + Delta_Y * Delta_Y);

      if Delta_X = 0.0 and then Delta_Y = 0.0 then

         Bearing := 0.0;

      else

         Bearing  := 90.0 - Arctan (Delta_Y, Delta_X) * 180.0 / Pi;

         if Bearing < 0.0 then

            Bearing := Bearing + 360.0;

         end if;

      end if;

   exception
      when others =>

         Ada.Text_IO.Put_Line ("error while calculating coordinates");

   end Coordinates;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Vector (Position_A, Position_B : Position_Record) return Vector2_Record is

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;
      use Ada.Numerics.Long_Elementary_Functions;

      Delta_Lat : Long_Float :=  Position_B.Lat - Position_A.Lat;

      Delta_Lon : Long_Float :=  Position_B.Lon - Position_A.Lon;

      Parallel_Length : Long_Float := Equator_Length * Cos (Position_A.Lat * Pi / 180.0);

      Vector : Vector2_Record;

   begin

      Vector.Set_X (Delta_Lon * Parallel_Length);

      Vector.Set_Y (Delta_Lat * Meridian_Length);

      return Vector;

   end Vector;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Vector (Position_A,
                    Position_B : Position_Record;
                    Scale      : Float) return Vector2_Record is

      V : Vector2_Record := Vector (Position_A, Position_B);

   begin

      V.Scale (Long_Float (Scale));

      return V;

   end Vector;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Position (Reference : Position_Record;
                      Vector    : Vector2_Record) return Position_Record is

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;
      use Ada.Numerics.Long_Elementary_Functions;

      Parallel_Length : Long_Float := Equator_Length * Cos (Reference.Lat * Pi / 180.0);

      Position : Position_Record;

   begin

      Position.Lat := Reference.Lat + Vector.Get_Y / Meridian_Length;

      Position.Lon := Reference.Lon + Vector.Get_X / Parallel_Length;

      return Position;

   end Position;
   -----------------------------------------------------------------------------



   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Colormaps
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Colormap_Stage_Record is record

      Color : Gl.Colors.Color_Record;

      Level : Float;

   end record;

   type Color_Stage_Array is array (Positive range <>) of Colormap_Stage_Record;

   -- Simple colorset
   Color_Stages_1 : constant Color_Stage_Array (1..6) :=
     (1 => (Level => 0.0, Color => (0.90,0.80,0.55,1.0)),
      2 => (Level => 0.2, Color => (0.35,0.80,0.55,1.0)),
      3 => (Level => 0.4, Color => (0.50,0.30,0.10,1.0)),
      4 => (Level => 0.6, Color => (0.20,0.50,0.50,1.0)),
      5 => (Level => 0.8, Color => (0.10,0.30,0.30,1.0)),
      6 => (Level => 1.0, Color => (0.10,0.20,0.20,1.0)));

   -- G-NAV colorset
   Color_Stages_2 : constant Color_Stage_Array (1..7) :=
     (1 => (Level => 0.0/6.0, Color => (178.0/255.0, 229.0/255.0, 153.0/255.0, 1.0)),
      2 => (Level => 1.0/6.0, Color => (229.0/255.0, 255.0/255.0, 204.0/255.0, 1.0)),
      3 => (Level => 2.0/6.0, Color => (201.0/255.0, 224.0/255.0, 143.0/255.0, 1.0)),
      4 => (Level => 3.0/6.0, Color => (187.0/255.0, 193.0/255.0,  93.0/255.0, 1.0)),
      5 => (Level => 4.0/6.0, Color => (0.90,0.80,0.55,1.0)),
      6 => (Level => 5.0/6.0, Color => (132.0/255.0,  91.0/255.0,  21.0/255.0, 1.0)),
      7 => (Level => 6.0/6.0, Color => (109.0/255.0,  58.0/255.0,  38.0/255.0, 1.0)));

   -- Matlab colorset
   Color_Stages_3 : constant Color_Stage_Array (1..12) :=
     (1 =>  (Level => 0.0/11.0, Color => ( 90.0/255.0, 185.0/255.0,  90.0/255.0, 1.0)), --(  0.0/255.0,   102.0/255.0,  51.0/255.0, 1.0)),
      2 =>  (Level => 1.0/11.0, Color => ( 16.0/255.0, 127.0/255.0,  53.0/255.0, 1.0)),
      3 =>  (Level => 2.0/11.0, Color => ( 87.0/255.0, 153.0/255.0,  59.0/255.0, 1.0)),
      4 =>  (Level => 3.0/11.0, Color => ( 80.0/255.0, 178.0/255.0,  71.0/255.0, 1.0)),
      5 =>  (Level => 4.0/11.0, Color => (124.0/255.0, 204.0/255.0, 108.0/255.0, 1.0)),
      6 =>  (Level => 5.0/11.0, Color => (178.0/255.0, 229.0/255.0, 153.0/255.0, 1.0)),
      7 =>  (Level => 6.0/11.0, Color => (229.0/255.0, 255.0/255.0, 204.0/255.0, 1.0)),
      8 =>  (Level => 7.0/11.0, Color => (201.0/255.0, 224.0/255.0, 143.0/255.0, 1.0)),
      9 =>  (Level => 8.0/11.0, Color => (187.0/255.0, 193.0/255.0,  93.0/255.0, 1.0)),
      10 => (Level => 9.0/11.0, Color => (163.0/255.0, 146.0/255.0,  50.0/255.0, 1.0)),
      11 => (Level =>10.0/11.0, Color => (132.0/255.0,  91.0/255.0,  21.0/255.0, 1.0)),
      12 => (Level =>11.0/11.0, Color => (102.0/255.0,  42.0/255.0,   0.0/255.0, 1.0)));

   Color_Stages : constant Color_Stage_Array := Color_Stages_2;

   --===========================================================================
   --
   --===========================================================================
   procedure Find_Color (This  : Map_View_Record;
                         Point : Position_Record;
                         Z, S,
                         Z_Min,
                         Z_Max : Float;
                         R, G, B : out Float) is

      use Gl.Colors;

      K : Float;

      Meridian : constant Float := Float (1000.0 * Meridian_Length);

      --========================================================================
      --
      --========================================================================
      function Round_10 (Value : Float) return Float is
      begin

         return Float'Rounding (10.0 * Value) / 10.0;

      end Round_10; pragma Inline (Round_10);
      --------------------------------------------------------------------------

      --========================================================================
      --
      --========================================================================
      function Round_20 (Value : Float) return Float is
      begin

         return Float'Rounding (20.0 * Value) / 20.0;

      end Round_20; pragma Inline (Round_20);
      --------------------------------------------------------------------------

      --========================================================================
      --
      --========================================================================
      function Round_100 (Value : Float) return Float is
      begin

         return Float'Rounding (100.0 * Value) / 100.0;

      end Round_100; pragma Inline (Round_100);
      --------------------------------------------------------------------------

   begin

      case This.Mode is

         when Monochrome =>

            K := Round_100 (1.0 - (Z - Z_Min) / (Z_Max - Z_Min));

            R := K;

            G := K;

            B := K;

         when Colormap =>

            K := (Z - This.Zero) / (Z_Max - This.Zero);

            if K = 0.0 then

               R := Color_Water.R;

               G := Color_Water.G;

               B := Color_Water.B;

            else

               for C in Color_Stages'First + 1 .. Color_Stages'Last loop

                  if K < Color_Stages (C).Level then

                     declare
                        C1 : Color_Record renames Color_Stages (C-1).Color;
                        C2 : Color_Record renames Color_Stages (C  ).Color;
                        L1 : Float renames Color_Stages (C-1).Level;
                        L2 : Float renames Color_Stages (C  ).Level;
                        F  : Float := (K - L1)/ (L2 - L1);
                     begin

                        R := Round_100 (C1.R + (C2.R - C1.R) * F);

                        G := Round_100 (C1.G + (C2.G - C1.G) * F);

                        B := Round_100 (C1.B + (C2.B - C1.B) * F);

                     end;

                     exit;

                  end if;

               end loop;

            end if;

      end case;

      if S > 0.0 then

         R := R * S;

         G := G * S;

         B := B * S;

      else

         R := R - (1.0 - R) * S;

         G := G - (1.0 - G) * S;

         B := B - (1.0 - B) * S;

      end if;

      if This.Cone_Active then

         declare

            A : Float := Range_Cone_Function (Point);

         begin

            if A < Z then

               -- Unreachable place due to:
               -- a) reached surface
               -- b) wind drift too hight
               ----------------------------

               null;

               G := 0.5 * G;
               B := 0.5 * B;

            elsif A < Z + This.Cone_Margin then

               -- Below safety altitude
               ------------------------

               R := 0.5 * R;
               G := 0.5 * G;

            end if;

         end;

      end if;

   exception
      when others =>

         Ada.Text_IO.Put_Line ("error while finding color");

   end Find_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Dataset_Name return String is
   begin

      return -Dataset_Name;

   end Get_Dataset_Name;
   -----------------------------------------------------------------------------

end Maps;
--------------------------------------------------------------------------------
