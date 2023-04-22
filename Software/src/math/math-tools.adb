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
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Text_IO;
-- Gnav
with Math.Vector2;
use  Math.Vector2;
with Math.Vector2_List;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Math.Tools is

   --///////////////////////////////////////////////////////////////////////////
   -- Floating point functions
   --///////////////////////////////////////////////////////////////////////////

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Contains_Point (Points : Math.Vector2_List.Stack_Access; X, Y : Long_Float) return Boolean is

      Y_A, Y_B, X_A, D_X, D_Y, X_C : Long_Float := 0.0;

      Point_A, Point_B : Vector2_Access;

      Right_Count : Natural := 0;

      Add : Boolean := True;

      --========================================================================
      procedure Analize_A_To_B_Segment is
      begin

         Y_A := Long_Float (Point_A.Get_Y) - Y;

         Y_B := Long_Float (Point_B.Get_Y) - Y;

         D_Y := abs (Y_B - Y_A);

         if D_Y > 0.0 then

            if (Y_A >= 0.0 and Y_B < 0.0) or else (Y_A < 0.0 and Y_B >= 0.0) then

               X_A := Long_Float (Point_A.Get_X) - X;

               D_X := Long_Float (Point_B.Get_X) - Long_Float (Point_A.Get_X);

               X_C := X_A + D_X * abs (Y_A) / D_Y;

               if X_C > 0.0 then

                  if Add then

                     Right_Count := Right_Count + 1;

                     Add := False;

                  else

                     Right_Count := Right_Count - 1;

                     Add := True;

                  end if;

               end if;

            end if;

         end if;

      end Analize_A_To_B_Segment;
      pragma Inline (Analize_A_To_B_Segment);
      --------------------------------------------------------------------------

   begin

      --------------------------------------------------------------------------
      -- Algorithm explanation:
      --
      -- > This algorithm is independent on the sence in which the polygon is
      --   defined.
      -- > This algorithm has no singular points/lines.
      -- > The result is not strictly determined at the boundary and vertices
      --   (some will be inside, and some outside).
      --
      -- The algorithm scans all boundary segments, and for each one of them
      -- crossing the X axis it computes the absolute number of right side
      -- intersections, but taking into account the reflected and refracted
      -- lines to avoid singular lines.
      --------------------------------------------------------------------------

      if Points.Get_Count > 2 then

         Point_A := Points.Get_First_Item;

         Point_B := Points.Get_First_Item;

         Points.Get_Next_Item (Point_B);

         if Point_A /= null and Point_B /= null then

            while Point_B /= null loop

               Analize_A_To_B_Segment;

               Point_A := Point_B;

               Points.Get_Next_Item (Point_B);

            end loop;

            -- This is the last segment:

            Point_B := Points.Get_First_Item;

            Analize_A_To_B_Segment;

         end if;

         -- If the absolute count is zero, we are outside.

         return not (Right_Count = 0);

      else

         return False;

      end if;

   end Contains_Point;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Indicates if the given point is within the offset of the given polyline
   --===========================================================================
   function Inside_Offset (Points  : Math.Vector2_List.Stack_Access;
                           Point   : Vector2_Record;
                           Offset  : Long_Float;
                           Closed  : Boolean;
                           Rounded : Boolean) return Boolean is

      use Math.Vector2_List;

      N, V1, V2 : Vector2_Access;

      R, R1, R2 : Vector2_Record;

   begin

      if Points /= null and then Points.Get_Count > 1 then

         -- Check the outline segments
         --------------------------------------------

         V1 := Points.Get_First_Item;

         V2 := V1;

         Points.Get_Next_Item (V2);

         while V2 /= null loop

            if Point.Contained (V1, V2, Offset) then

               return True;

            end if;

            V1 := V2;

            Points.Get_Next_Item (V2);

         end loop;

         -- Check the rounded ends (if open)
         --------------------------------------------

         if not Closed and Rounded then

            N := Points.Get_First_Item;

            if Point.Distance (N) < Offset then

               V1 := N;

               Points.Get_Next_Item (V1);

               R.Set_From_Difference (Point, N.all);

               R1.Set_From_Difference (N.all, V1.all);

               if R.Dot_Product (R1) >= 0.0 then

                  return True;

               end if;

            end if;

            N := Points.Get_Last_Item;

            if Point.Distance (N) < Offset then

               V2 := N;

               Points.Get_Previous_Item (V2);

               R.Set_From_Difference (Point, N.all);

               R2.Set_From_Difference (N.all, V2.all);

               if R.Dot_Product (R2) >= 0.0 then

                  return True;

               end if;

            end if;

         end if;

         if Points.Get_Count > 2 then

            -- Check closure segment
            --------------------------------------------

            if Closed then

               V1 := Points.Get_Last_Item;

               V2 := Points.Get_First_Item;

               if Point.Contained (V1, V2, Offset) then

                  return True;

               end if;

            end if;

            -- Check convex joints
            --------------------------------------------

            N := Points.Get_First_Item;

            while N /= null loop

               if Point.Distance (N) < Offset then

                  -- Point inside the joint circle
                  --------------------------------

                  V1 := N;

                  Points.Get_Previous_Item (V1);

                  if V1 = null and Closed then

                     V1 := Points.Get_Last_Item;

                  end if;

                  V2 := N;

                  Points.Get_Next_Item (V2);

                  if V2 = null and Closed then

                     V2 := Points.Get_First_Item;

                  end if;

                  if V1 /= null and V2 /= null then

                     R.Set_From_Difference (Point, N.all);

                     R1.Set_From_Difference (V1.all, N.all);

                     if R1.Dot_Product (R) <= 0.0 then

                        R2.Set_From_Difference (V2.all, N.all);

                        if R2.Dot_Product (R) <= 0.0 then

                           -- Point within the convex gap
                           ------------------------------

                           return True;

                        end if;

                     end if;

                  end if;

               end if;

               Points.Get_Next_Item (N);

            end loop;

         end if;

         return False;

      else

         return False;

      end if;

   end Inside_Offset;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Clean_Line (Points : Math.Vector2_List.Stack_Access; Resolution : Long_Float) is

      Point_0, Point_1, Point_2 : Vector2_Access;

      Direction_01, Direction_12 : Vector2_Record;

      Cosinus, Distance_01, Distance_12, Factor, Admision_Distance : Long_Float;

      First : Boolean := True;

      Threshold : Long_Float := 0.1;

   begin

      if Points.Get_Count > 2 then

         Point_0 := Points.Get_First_Item;

         Point_1 := Point_0;

         Points.Get_Next_Item (Point_1);

         Point_2 := Point_1;

         Points.Get_Next_Item (Point_2);

         while Point_2 /= null loop

            Direction_01.Set (Point_1.Get_X - Point_0.Get_X, Point_1.Get_Y - Point_0.Get_Y);

            Direction_12.Set (Point_2.Get_X - Point_1.Get_X, Point_2.Get_Y - Point_1.Get_Y);

            Distance_01 := Direction_01.Norm2;

            Distance_12 := Direction_12.Norm2;

            if Distance_01 > Threshold and Distance_12 > Threshold then

               Cosinus := Long_Float'Min (1.0, Direction_01.Dot_Product (Direction_12) / (Distance_01 * Distance_12));

               if Cosinus > 0.0 then

                  if Cosinus > 0.999 then

                     Admision_Distance := 100.0 * Resolution;

                  else

                     Factor := 1.0 - Ada.Numerics.Long_Elementary_Functions.Arccos (Cosinus) / (0.5 * Math.Pi);

                     Admision_Distance := Factor * Factor * Resolution;

                  end if;

               else

                  Admision_Distance := Threshold;

               end if;

            else

               Admision_Distance := Resolution;

            end if;

            if Distance_01 < Admision_Distance and Distance_12 < Admision_Distance then

               Points.Remove_Item (Point_1);

               Point_1 := Point_2;

               Points.Get_Next_Item (Point_2);

            else

               Point_0 := Point_1;

               Point_1 := Point_2;

               Points.Get_Next_Item (Point_2);

            end if;

         end loop;

      end if;

   end Clean_Line;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Minimum_Distance (Points : Math.Vector2_List.Stack_Access;
                                  Point  : Vector2_Record;
                                  Closed : Boolean) return Long_Float is

      Point_A, Point_B : Vector2_Access;

      Min_Distance : Long_Float := Long_Float'Last;

      Distance : Long_Float;

   begin

      Point_B := Points.Get_First_Item;

      while Point_B /= null loop

         if Point_A /= null then

            Distance := Point.Distance (Point_A, Point_B);

            if Distance < Min_Distance then

               Min_Distance := Distance;

            end if;

         end if;

         Point_A := Point_B;

         Points.Get_Next_Item (Point_B);

      end loop;

      if Closed then

         Point_A := Points.Get_Last_Item;

         Point_B := Points.Get_First_Item;

         if
           Point_A /= null and
           Point_B /= null and
           Point_A /= Point_B
         then

            Distance := Point.Distance (Point_A, Point_B);

            if Distance < Min_Distance then

               Min_Distance := Distance;

            end if;

         end if;

      end if;

      return Min_Distance;

   end Get_Minimum_Distance;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Closest_Node (Points : Math.Vector2_List.Stack_Access;
                              Point  : Vector2_Record) return Vector2_Access is

      Other_Point, Closest_Point : Vector2_Access;

      Min_Distance : Long_Float := 0.0;

      Distance : Long_Float := 0.0;

      First : Boolean := True;

   begin

      Other_Point := Points.Get_First_Item;

      while Other_Point /= null loop

         Distance := Point.Distance (Other_Point.all);

         if
           First or
           Distance < Min_Distance
         then

            Closest_Point := Other_Point;

            Min_Distance := Distance;

            First := False;

         end if;

         Points.Get_Next_Item (Other_Point);

      end loop;

      return Closest_Point;

   end Get_Closest_Node;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Closest_Point (Points : Math.Vector2_List.Stack_Access;
                               Point  : Vector2_Record;
                               Closed : Boolean) return Vector2_Record is

      Node_A, Node_B : Vector2_Access;

      A_B, A_P, Test_Node, New_Node : Vector2_Record;

      Length, Projection, Distance, Offset : Long_Float;

   begin

      Offset := Long_Float'Last;

      if Closed then

         Node_A := Points.Get_Last_Item;

         Node_B := Points.Get_First_Item;

      else

         Node_A := Points.Get_First_Item;

         Node_B := Node_A;

         Points.Get_Next_Item (Node_B);

      end if;

      while Node_B /= null loop

         A_B.Set_X (Node_B.Get_X - Node_A.Get_X);

         A_B.Set_Y (Node_B.Get_Y - Node_A.Get_Y);

         A_B.Normalize (Length);

         A_P.Set_X (Point.Get_X - Node_A.Get_X);

         A_P.Set_Y (Point.Get_Y - Node_A.Get_Y);

         Projection := A_B.Dot_Product (A_P);

         if Projection < 0.0 then

            Test_Node.Set_X (Node_A.Get_X);

            Test_Node.Set_Y (Node_A.Get_Y);

         elsif Projection > Length then

            Test_Node.Set_X (Node_B.Get_X);

            Test_Node.Set_Y (Node_B.Get_Y);

         else

            Test_Node.Set_X (Node_A.Get_X + Projection * A_B.Get_X);

            Test_Node.Set_Y (Node_A.Get_Y + Projection * A_B.Get_Y);

         end if;

         Distance := Test_Node.Distance (Point);

         if Distance < Offset then

            New_Node := Test_Node;

            Offset := Distance;

         end if;

         --

         Node_A := Node_B;

         Points.Get_Next_Item (Node_B);

      end loop;

      return New_Node;

   end Get_Closest_Point;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the node that is closest to the given boundary.
   --===========================================================================
   procedure Get_Closest_Point (Points  : Math.Vector2_List.Stack_Access;
                                Point   : Vector2_Record;
                                Closed  : Boolean;
                                Closest : in out Vector2_Record;
                                Offset  : out Long_Float;
                                Segment : out Natural) is

      Node_A, Node_B : Vector2_Access;

      A_B, A_P, Test_Node : Vector2_Record;

      Length, Projection, Distance : Long_Float;

      S : Natural := 0;

   begin

      Closest.Set (0.0, 0.0);

      Offset := Long_Float'Last;

      if Closed then

         S := 0;

         Node_A := Points.Get_Last_Item;

         Node_B := Points.Get_First_Item;

      else

         S := 1;

         Node_A := Points.Get_First_Item;

         Node_B := Node_A;

         Points.Get_Next_Item (Node_B);

      end if;

      while Node_B /= null loop

         A_B.Set_X (Node_B.Get_X - Node_A.Get_X);

         A_B.Set_Y (Node_B.Get_Y - Node_A.Get_Y);

         A_B.Normalize (Length);

         A_P.Set_X (Point.Get_X - Node_A.Get_X);

         A_P.Set_Y (Point.Get_Y - Node_A.Get_Y);

         Projection := A_B.Dot_Product (A_P);

         if Projection < 0.0 then

            Test_Node.Set_X (Node_A.Get_X);

            Test_Node.Set_Y (Node_A.Get_Y);

         elsif Projection > Length then

            Test_Node.Set_X (Node_B.Get_X);

            Test_Node.Set_Y (Node_B.Get_Y);

         else

            Test_Node.Set_X (Node_A.Get_X + Projection * A_B.Get_X);

            Test_Node.Set_Y (Node_A.Get_Y + Projection * A_B.Get_Y);

         end if;

         Distance := Test_Node.Distance (Point);

         if Distance < Offset then

            Closest := Test_Node;

            Offset  := Distance;

            Segment := S;

         end if;

         --

         Node_A := Node_B;

         Points.Get_Next_Item (Node_B);

         S := S + 1;

      end loop;

   end Get_Closest_Point;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Curvilinear_Coordinate (Points : Math.Vector2_List.Stack_Access;
                                        Point  : Vector2_Access) return Long_Float is

      Pa, Pb : Vector2_Access;

      Distance : Long_Float := 0.0;

   begin

      Pa := Points.Get_First_Item;

      if Point /= Pa then

         Pb := Points.Get_First_Item;

         Points.Get_Next_Item (Pb);

         while Pb /= null loop

            Distance := Distance + Pa.Distance (Pb.all);

            if Pb = Point then

               return Distance;

            end if;

            Pa := Pb;

            Points.Get_Next_Item (Pb);

         end loop;

      end if;

      return 0.0;

      end Get_Curvilinear_Coordinate;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Insert_Point (Points    : Math.Vector2_List.Stack_Access;
                          Point     : Vector2_Record;
                          Threshold : Long_Float;
                          Closed    : Boolean) return Vector2_Access is

      Node_A, Node_B, New_Node : Vector2_Access;

      A_B, A_P : Vector2_Record;

      Projection : Long_Float;

      i : Positive := 1;

   begin

      if Closed then

         Node_A := Points.Get_Last_Item;

         Node_B := Points.Get_First_Item;

      else

         Node_A := Points.Get_First_Item;

         Node_B := Node_A;

         Points.Get_Next_Item (Node_B);

         i := i + 1;

      end if;

      while Node_B /= null loop

         if Point.Distance (Node_A, Node_B) < Threshold then

            Points.Insert_Item (i, New_Node);

            A_B.Set_X (Node_B.Get_X - Node_A.Get_X);

            A_B.Set_Y (Node_B.Get_Y - Node_A.Get_Y);

            A_B.Normalize;

            A_P.Set_X (Point.Get_X - Node_A.Get_X);

            A_P.Set_Y (Point.Get_Y - Node_A.Get_Y);

            Projection := A_B.Dot_Product (A_P);

            New_Node.Set_X (Node_A.Get_X + Projection * A_B.Get_X);

            New_Node.Set_Y (Node_A.Get_Y + Projection * A_B.Get_Y);

            return New_Node;

         end if;

         --

         Node_A := Node_B;

         Points.Get_Next_Item (Node_B);

         i := i + 1;

      end loop;

      return null;

   end Insert_Point;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Length (Points : Math.Vector2_List.Stack_Access) return Long_Float is

      Node_A, Node_B : Vector2_Access;

      Length : Long_Float := 0.0;

   begin

      Node_A := Points.Get_First_Item;

      Node_B := Node_A;

      Points.Get_Next_Item (Node_B);

      while Node_B /= null loop

         Length := Length + Node_A.Distance (Node_B.all);

         Node_A := Node_B;

         Points.Get_Next_Item (Node_B);

      end loop;

      return Length;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Point_At (Points : Math.Vector2_List.Stack_Access;
                          S      : Long_Float) return Vector2_Record is

      Node_A, Node_B : Vector2_Access;

      Dis_A, Dis_B : Long_Float := 0.0;

      Vector : Vector2_Record := No_Vector2_Record;

   begin

      Node_A := Points.Get_First_Item;

      Node_B := Node_A;

      Points.Get_Next_Item (Node_B);

      while Node_B /= null loop

         Dis_A := Dis_B;

         Dis_B := Dis_B + Node_A.Distance (Node_B.all);

         if S >= Dis_A and S <= Dis_B then

            Vector.Set (Node_B.Get_X - Node_A.Get_X, Node_B.Get_Y - Node_A.Get_Y);

            Vector.Normalize;

            Vector.Scale (S - Dis_A);

            Vector.Add (Node_A.all);

            return Vector;

         end if;

         Node_A := Node_B;

         Points.Get_Next_Item (Node_B);

      end loop;

      return Vector;

   end Get_Point_At;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Geometric_Center (Points : Math.Vector2_List.Stack_Access) return Vector2_Record is

      Center : Vector2_Record;
      Node   : Vector2_Access;

   begin

      Center.Set (0.0, 0.0);

      Node := Points.Get_First_Item;

      while Node /= null loop

         Center.Set_X (Center.Get_X + Node.Get_X);
         Center.Set_Y (Center.Get_Y + Node.Get_Y);

         Points.Get_Next_Item (Node);

      end loop;

      if Points.Get_Count > 0 then

         Center.Scale (1.0 / Long_Float (Points.Get_Count));

      end if;

      return Center;

   end Get_Geometric_Center;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (PA, PB)     -> First segment nodes
   -- (QA, QB)     -> Second segment nodes
   -- EP, EQ       -> Indicates if it is allowed to extend de segments
   -- Intersect    -> Whether it intersected or not
   -- LP           -> Distance from PA to intersection along first segment
   -- LQ           -> Distance from QA to intersection along second segment
   -- Intersection -> The intersection point
   --===========================================================================
   procedure Intersection (PA, PB, QA, QB : Vector2_Record;
                           EP, EQ         : Boolean;
                           Intersects     : out Boolean;
                           LP, LQ         : out Long_Float;
                           Intersection   : in out Vector2_Record) is

      V, U, D : Vector2_Record;

      Determinant, LV, LU : Long_Float;

   begin

      Intersects := False;

      Intersection.Set (0.0, 0.0);

      LP := 0.0;

      LQ := 0.0;

      V.Set (PB.Get_X - PA.Get_X, PB.Get_Y - PA.Get_Y);

      LV := V.Norm2;

      U.Set (QB.Get_X - QA.Get_X, QB.Get_Y - QA.Get_Y);

      LU := U.Norm2;

      if LV > 0.0 and LU > 0.0 then

         V.Scale (1.0 / LV);

         U.Scale (1.0 / LU);

         Determinant := U.Get_X * V.Get_Y - U.Get_Y * V.Get_X;

         if (abs Determinant) > 0.0 then

            D.Set (PA.Get_X - QA.Get_X, PA.Get_Y - QA.Get_Y);

            LP := (U.Get_Y * D.Get_X - U.Get_X * D.Get_Y) / Determinant;

            LQ := (V.Get_Y * D.Get_X - V.Get_X * D.Get_Y) / Determinant;

            if
              (EP or (LP >= 0.0 and LP <= LV)) and
              (EQ or (LQ >= 0.0 and LQ <= LU))
            then

               Intersects := True;

               Intersection.Set_X (PA.Get_X + LP * V.Get_X);

               Intersection.Set_Y (PA.Get_Y + LP * V.Get_Y);

            end if;

         end if;

      else

         null;

      end if;

   end Intersection;
   -----------------------------------------------------------------------------

end Math.Tools;
