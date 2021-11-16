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
with Ada.Numerics.Long_Elementary_Functions;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Meshing.Triangle is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the determinant method is used to check if points are located
   -- inside the circumcircle.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Use_Determinant_Method : constant Boolean := True;

   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Get_Vertex_A (This : Triangle_Record) return Vector2_Access is
   begin

      return This.Vertex_A;

   end Get_Vertex_A;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Get_Vertex_B (This : Triangle_Record) return Vector2_Access is
   begin

      return This.Vertex_B;

   end Get_Vertex_B;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Get_Vertex_C (This : Triangle_Record) return Vector2_Access is
   begin

      return This.Vertex_C;

   end Get_Vertex_C;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Get_Circumcenter (This : Triangle_Record) return Vector2_Access is
   begin

      return This.Circumcenter;

   end Get_Circumcenter;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Get_Center (This : Triangle_Record) return Vector2_Record is
   begin

      if This.Initialized then

         return New_Vector2_Record (
                                    (This.Vertex_A.Get_X + This.Vertex_B.Get_X + This.Vertex_C.Get_X) / 3.0,
                                    (This.Vertex_A.Get_Y + This.Vertex_B.Get_Y + This.Vertex_C.Get_Y) / 3.0
                                   );

      else

         return New_Vector2_Record (0.0, 0.0);

      end if;

   end Get_Center;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Get_Initialized (This : Triangle_Record) return Boolean is
   begin

      return This.Initialized;

   end Get_Initialized;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Contains (This : Triangle_Record; Point : Vector2_Access) return Boolean is
   begin

      return This.Locate_Point (Point) /= Outside;

   end Contains;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Locate_Point (This : Triangle_Record; Point : Vector2_Access) return Point_Location_Kinds is

      Distance_1, Distance_2, Distance_3 : Long_Float;

      Location : Point_Location_Kinds := Outside;

      Epsilon : Long_Float := 0.1 * Meshing.Proximity_Threshold;

   begin

      if not This.Initialized then

         if Verbosity_Level > 0 then

            Ada.Text_IO.Put_Line ("Error: attempting tu use non initialized triangle");

         end if;

         Abort_Meshing := True;

      elsif This.Area > 0.0 then

         Distance_1 := ((This.Vertex_B.Get_X - This.Vertex_A.Get_X) * (Point.Get_Y - This.Vertex_A.Get_Y) -
                        (Point.Get_X - This.Vertex_A.Get_X) * (This.Vertex_B.Get_Y - This.Vertex_A.Get_Y)) / This.AB_Lenght;

         Distance_2 := ((This.Vertex_C.Get_X - This.Vertex_B.Get_X) * (Point.Get_Y - This.Vertex_B.Get_Y) -
                        (Point.Get_X - This.Vertex_B.Get_X) * (This.Vertex_C.Get_Y - This.Vertex_B.Get_Y)) / This.BC_Lenght;

         Distance_3 := ((This.Vertex_A.Get_X - This.Vertex_C.Get_X) * (Point.Get_Y - This.Vertex_C.Get_Y) -
                        (Point.Get_X - This.Vertex_C.Get_X) * (This.Vertex_A.Get_Y - This.Vertex_C.Get_Y)) / This.CA_Lenght;

         if Verbosity_Level > 5 then

            Ada.Text_IO.Put_Line ("Distance to AB -> " & Long_Float'Image (Distance_1));

            Ada.Text_IO.Put_Line ("Distance to BC -> " & Long_Float'Image (Distance_2));

            Ada.Text_IO.Put_Line ("Distance to CA -> " & Long_Float'Image (Distance_3));

         end if;

         -----------------------------------------------------------------------
         -- Note: Epsilon is a fraction of the proximity threshold that is used
         -- to create a gray zone at the edges of the triangles, to handle
         -- errors derivated from floating point aritmetics.
         -----------------------------------------------------------------------

         if
           (This.Sence = Positive and then (Distance_1 >=-Epsilon and Distance_2 >=-Epsilon and Distance_3 >=-Epsilon)) or else
           (This.Sence = Negative and then (Distance_1 <= Epsilon and Distance_2 <= Epsilon and Distance_3 <= Epsilon))
         then

            Location := Inside;

            Distance_1 := abs Distance_1;

            Distance_2 := abs Distance_2;

            Distance_3 := abs Distance_3;

            if
              (Distance_1 < Meshing.Proximity_Threshold) and then
              (Distance_1 <= Distance_2 and Distance_1 <= Distance_3)
            then

               Location := Close_To_AB;

            elsif
              (Distance_2 < Meshing.Proximity_Threshold) and then
              (Distance_2 <= Distance_1 and Distance_2 <= Distance_3)
            then

               Location := Close_To_BC;

            elsif
              (Distance_3 < Meshing.Proximity_Threshold) and then
              (Distance_3 <= Distance_1 and Distance_3 <= Distance_2)
            then

               Location := Close_To_CA;

            end if;

         end if;

      else

         if Verbosity_Level > 1 then

            Ada.Text_IO.Put_Line ("Warning: attempting tu use a zero area triangle");

         end if;

         Abort_Meshing := True;

      end if;

      return Location;

   end Locate_Point;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function In_Circumcircle (This : Triangle_Record; Point : Vector2_Access) return Boolean is
   begin

      if This.Initialized then

         if Use_Determinant_Method then

            -----------------------------------------------------------------------
            -- Determinant-based method
            -----------------------------------------------------------------------

            declare

               a11 : Long_Float := This.Vertex_A.Get_X - Point.Get_X;

               a12 : Long_Float := This.Vertex_A.Get_Y - Point.Get_Y;

               a13 : Long_Float := a11 * a11 + a12 * a12;

               a21 : Long_Float := This.Vertex_B.Get_X - Point.Get_X;

               a22 : Long_Float := This.Vertex_B.Get_Y - Point.Get_Y;

               a23 : Long_Float := a21 * a21 + a22 * a22;

               a31 : Long_Float := This.Vertex_C.Get_X - Point.Get_X;

               a32 : Long_Float := This.Vertex_C.Get_Y - Point.Get_Y;

               a33 : Long_Float := a31 * a31 + a32 * a32;

               Det : Long_Float := a11 * (a22 * a33 - a23 * a32) - a12 * (a21 * a33 - a31 * a23) + a13 * (a21 * a32 - a31 * a22);

            begin

               if This.Sence = Positive then

                  return Det > 0.0;

               else

                  return Det < 0.0;

               end if;

            end;

         else

            -----------------------------------------------------------------------
            -- Euclidean-based method
            -----------------------------------------------------------------------

            declare

               rx : Long_Float := Point.Get_X - This.Circumcenter.Get_X;

               ry : Long_Float := Point.Get_Y - This.Circumcenter.Get_Y;

            begin

               --------------------------------------------------------------------------
               -- Important note: the operator '<' should not be replaced by '<='
               --------------------------------------------------------------------------

               return rx * rx + ry * ry < This.Circumradius * This.Circumradius;

            end;

         end if;

      else

         return False;

      end if;

   end In_Circumcircle;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   procedure Crosses_Segment (This : Triangle_Record; Segment : Segment_Access; Result : out Boolean; Side : out Triangle_Side_Kinds; Common_Vertex : out Vector2_Access) is
   begin

      Result := False;

      Side := AB_Side;

      Common_Vertex := null;

      if This.Initialized then

         if Segment.Get_Vertex_A = This.Vertex_C or Segment.Get_Vertex_B = This.Vertex_C then

            Result := Segment.Crosses (This.Vertex_A, This.Vertex_B);

            if Result then

               Side := AB_Side;

               Common_Vertex := This.Vertex_C;

               return;

            end if;

         end if;

         if Segment.Get_Vertex_A = This.Vertex_A or Segment.Get_Vertex_B = This.Vertex_A then

            Result := Segment.Crosses (This.Vertex_B, This.Vertex_C) ;

            if Result then

               Side := BC_Side;

               Common_Vertex := This.Vertex_A;

               return;

            end if;

         end if;

         if Segment.Get_Vertex_A = This.Vertex_B or Segment.Get_Vertex_B = This.Vertex_B then

            Result := Segment.Crosses (This.Vertex_C, This.Vertex_A);

            if Result then

               Side := CA_Side;

               Common_Vertex := This.Vertex_B;

               return;

            end if;

         end if;

      else

         if Verbosity_Level > 0 then

            Ada.Text_IO.Put_Line ("Error: attempting tu use non initialized triangle");

         end if;

         Abort_Meshing := True;

      end if;

   end Crosses_Segment;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   procedure Set_Vertices (This : in out Triangle_Record; A, B, C : Vector2_Access) is
   begin

      if A /= null and B /= null and C /= null then

         if A = B or A = C or B = C then

            This.Initialized := False;

            if Verbosity_Level > 0 then

               Ada.Text_IO.Put_Line ("Error: cannot initialize a triangle with two identic vertices");

            end if;

            Abort_Meshing := True;

         else

            This.Vertex_A := A;

            This.Vertex_B := B;

            This.Vertex_C := C;

            This.Initialized := True;

            This.Update_Properties;

         end if;

      else

         This.Initialized := False;

         if Verbosity_Level > 0 then

            Ada.Text_IO.Put_Line ("Error: cannot initialize a triangle with no reference to a vertex");

         end if;

         Abort_Meshing := True;

      end if;

   end Set_Vertices;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   procedure Update_Properties (This : in out Triangle_Record) is
   begin

      if This.Initialized then

         -----------------------------------------------------------------------
         -- Area
         -----------------------------------------------------------------------

         This.Area := 0.5 * ((This.Vertex_B.Get_X - This.Vertex_A.Get_X) * (This.Vertex_C.Get_Y - This.Vertex_A.Get_Y) -
                             (This.Vertex_C.Get_X - This.Vertex_A.Get_X) * (This.Vertex_B.Get_Y - This.Vertex_A.Get_Y));

         if This.Area > 0.0 then

            This.Sence := Positive;

         else

            This.Sence := Negative;

         end if;

         This.Area := abs This.Area;

         if This.Area = 0.0 then

            if Verbosity_Level > 1 then

               Ada.Text_IO.Put_Line ("Warning: triangle set with zero area");

               This.Publish;

            end if;

         end if;

         -----------------------------------------------------------------------
         -- Lenght of sides
         -----------------------------------------------------------------------

         This.AB_Lenght := This.Vertex_A.Distance (This.Vertex_B.all);

         This.BC_Lenght := This.Vertex_B.Distance (This.Vertex_C.all);

         This.CA_Lenght := This.Vertex_C.Distance (This.Vertex_A.all);

         if Verbosity_Level > 1 then

            if
              This.AB_Lenght < Meshing.Proximity_Threshold or
              This.BC_Lenght < Meshing.Proximity_Threshold or
              This.CA_Lenght < Meshing.Proximity_Threshold
            then

               Ada.Text_IO.Put_Line ("Warning: triangle set with a side lenght under the proximity treshold");

               This.Publish;

            elsif
              This.Area < Meshing.Proximity_Threshold * This.AB_Lenght or
              This.Area < Meshing.Proximity_Threshold * This.BC_Lenght or
              This.Area < Meshing.Proximity_Threshold * This.CA_Lenght
            then

               Ada.Text_IO.Put_Line ("Warning: triangle set with height under the proximity threshold");

               This.Publish;

            end if;

         end if;

         -----------------------------------------------------------------------
         -- Circumcenter
         -----------------------------------------------------------------------

         if not Use_Determinant_Method then

            declare

               p1x : Long_Float := This.Vertex_A.Get_X + This.Vertex_B.Get_X;

               p1y : Long_Float := This.Vertex_A.Get_Y + This.Vertex_B.Get_Y;

               u1x : Long_Float := This.Vertex_A.Get_Y - This.Vertex_B.Get_Y;

               u1y : Long_Float := This.Vertex_B.Get_X - This.Vertex_A.Get_X;

               u2x : Long_Float := This.Vertex_A.Get_Y - This.Vertex_C.Get_Y;

               u2y : Long_Float := This.Vertex_C.Get_X - This.Vertex_A.Get_X;

               Determinant : Long_Float := u2x * u1y - u1x * u2y;

               t, rx, ry : Long_Float;

            begin

               if (abs Determinant) > 0.0 then

                  t := (u2y * (This.Vertex_B.Get_X - This.Vertex_C.Get_X) - u2x * (This.Vertex_B.Get_Y - This.Vertex_C.Get_Y)) / Determinant;

                  This.Circumcenter.Set_X (0.5 * (p1x + t * u1x));

                  This.Circumcenter.Set_Y (0.5 * (p1y + t * u1y));

                  rx := This.Vertex_A.Get_X - This.Circumcenter.Get_X;

                  ry := This.Vertex_A.Get_Y - This.Circumcenter.Get_Y;

                  This.Circumradius := Ada.Numerics.Long_Elementary_Functions.Sqrt (rx * rx + ry * ry);

               else

                  if Verbosity_Level > 1 then

                     Ada.Text_IO.Put_Line ("Warning: circumcenter not set");

                  end if;

                  This.Circumcenter.Set (0.0, 0.0);

                  This.Circumradius := 0.0;

               end if;

            end;

         end if;

      else

         This.Area := 0.0;

      end if;

   end Update_Properties;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   procedure Publish (This : Triangle_Record) is
   begin

      if This.Initialized and Verbosity_Level > 0 then

         Ada.Text_IO.Put ("A -> ");

         This.Get_Vertex_A.Publish;

         Ada.Text_IO.Put ("B -> ");

         This.Get_Vertex_B.Publish;

         Ada.Text_IO.Put ("C -> ");

         This.Get_Vertex_C.Publish;

      end if;

   end Publish;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Height (A, B, C : Vector2_Access) return Long_Float is

      Area, Base : Long_Float;

   begin

      Area := abs (0.5 * ((B.Get_X - A.Get_X) * (C.Get_Y - A.Get_Y) -
                          (C.Get_X - A.Get_X) * (B.Get_Y - A.Get_Y)));

      Base := Long_Float'Max (A.Distance (B.all), Long_Float'Max (B.Distance (C.all), C.Distance (A.all)));

      if Base > Proximity_Threshold then

         return Area / Base;

      else

         return 0.0;

      end if;

   end Height;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   overriding procedure Initialize (This : in out Triangle_Record) is
   begin

      This.Circumcenter := new Vector2_Record;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   overriding procedure Adjust (This : in out Triangle_Record) is
   begin

      null;

   end Adjust;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   overriding procedure Finalize (This : in out Triangle_Record) is
   begin

      Free_Vector2 (This.Circumcenter);

   end Finalize;
   -----------------------------------------------------------------------------




end Meshing.Triangle;
--------------------------------------------------------------------------------
