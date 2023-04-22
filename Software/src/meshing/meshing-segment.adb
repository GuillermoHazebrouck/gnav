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
-- Gnav
with Math.Vector2;
use  Math.Vector2;
with Math.Vector2_List;
with Stacks.Linked;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Meshing.Segment is

   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Get_Vertex_A (This : Segment_Record) return Vector2_Access is
   begin

      return This.Vertex_A;

   end Get_Vertex_A;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Get_Vertex_B (This : Segment_Record) return Vector2_Access is
   begin

      return This.Vertex_B;

   end Get_Vertex_B;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Get_Direction (This : Segment_Record) return Vector2_Access is
   begin

      return This.Direction;

   end Get_Direction;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Get_Length (This : Segment_Record) return Long_Float is
   begin

      return This.Length;

   end Get_Length;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Contains (This : Segment_Record; Point : Vector2_Access; Offset : Long_Float := Meshing.Proximity_Threshold) return Boolean is

      Vector_To_Point : Vector2_Record;

      Projection : Long_Float;

      Distance : Long_Float;

   begin

      if This.Initialized then

         Vector_To_Point.Set_X (Point.Get_X - This.Vertex_A.Get_X);

         Vector_To_Point.Set_Y (Point.Get_Y - This.Vertex_A.Get_Y);

         Projection := This.Direction.Dot_Product (Vector_To_Point);

         if Projection > 0.0 and Projection < This.Length then

            Distance := abs This.Direction.Cross_Product (Vector_To_Point);

            if Distance < Offset then

               return True;

            end if;

         end if;

         return False;

      else

         Ada.Text_IO.Put_Line ("Error. Attempting to use an unitizalide segment.");

         Abort_Meshing := True;

         return False;

      end if;

   end Contains;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Distance_To (This : Segment_Record; Point : Vector2_Access) return Long_Float is

      Vector_To_Point : Vector2_Record;

      Projection : Long_Float;

   begin

      if This.Initialized then

         Vector_To_Point.Set_X (Point.Get_X - This.Vertex_A.Get_X);

         Vector_To_Point.Set_Y (Point.Get_Y - This.Vertex_A.Get_Y);

         Projection := This.Direction.Dot_Product (Vector_To_Point);

         if Projection > 0.0 and Projection < This.Length then

            return abs This.Direction.Cross_Product (Vector_To_Point);

         else

            return Long_Float'Min (This.Vertex_A.Distance (Point.all), This.Vertex_B.Distance (Point.all));

         end if;

      else

         Ada.Text_IO.Put_Line ("Error. Attempting to use an unitizalide segment.");

         Abort_Meshing := True;

         return 0.0;

      end if;

   end Distance_To;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   function Crosses (This : Segment_Record; Point_A, Point_B : Vector2_Access) return Boolean is

      U : Vector2_Record;

      Determinant, LU, L1, L2, Dx, Dy : Long_Float;

   begin

      if This.Initialized then

         U.Set (Point_B.Get_X - Point_A.Get_X,
                Point_B.Get_Y - Point_A.Get_Y);

         LU := U.Norm2;

         if LU > 0.0 then

            U.Scale (1.0 / LU);

            Determinant := U.Get_X * This.Direction.Get_Y - U.Get_Y * This.Direction.Get_X;

            if (abs Determinant) > 0.0 then

               Dx := This.Vertex_A.Get_X - Point_A.Get_X;

               Dy := This.Vertex_A.Get_Y - Point_A.Get_Y;

               L1 := (U.Get_Y * Dx - U.Get_X * Dy) / Determinant / This.Length;

               L2 := (This.Direction.Get_Y * Dx - This.Direction.Get_X * Dy) / Determinant / LU;

               if L1 > 0.0 and L1 < 1.0 and L2 > 0.0 and L2 < 1.0 then

                  return True;

               else

                  return False;

               end if;

            else

               return False;

            end if;

         else

            Ada.Text_IO.Put_Line ("Warning: null lenght segment provided");

            return False;

         end if;

      else

         Ada.Text_IO.Put_Line ("Error: attempting to use unitizalized segment");

         Abort_Meshing := True;

         return False;

      end if;

   end Crosses;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   procedure Intersection (This : Segment_Record; Segment : Segment_Access; Intersect : out Boolean; Intersection : in out Vector2_Record) is

      U : Vector2_Record;

      Determinant, LU, L1, L2, Dx, Dy : Long_Float;

   begin

      Intersect := False;

      Intersection.Set (0.0, 0.0);

      if This.Initialized then

         U.Set (Segment.Get_Vertex_B.Get_X - Segment.Get_Vertex_A.Get_X,
                Segment.Get_Vertex_B.Get_Y - Segment.Get_Vertex_A.Get_Y);

         LU := U.Norm2;

         if LU > 0.0 then

            U.Scale (1.0 / LU);

            Determinant := U.Get_X * This.Direction.Get_Y - U.Get_Y * This.Direction.Get_X;

            if (abs Determinant) > 0.0 then

               Dx := This.Vertex_A.Get_X - Segment.Get_Vertex_A.Get_X;

               Dy := This.Vertex_A.Get_Y - Segment.Get_Vertex_A.Get_Y;

               L1 := (U.Get_Y * Dx - U.Get_X * Dy) / Determinant / This.Length;

               L2 := (This.Direction.Get_Y * Dx - This.Direction.Get_X * Dy) / Determinant / LU;

               if L1 > 0.0 and L1 < 1.0 and L2 > 0.0 and L2 < 1.0 then

                  Intersect := True;

                  Intersection.Set_X (This.Vertex_A.Get_X + This.Direction.Get_X * L1 * This.Length);

                  Intersection.Set_Y (This.Vertex_A.Get_Y + This.Direction.Get_Y * L1 * This.Length);

               end if;

            end if;

         else

            Ada.Text_IO.Put_Line ("Warning: null lenght segment provided");

         end if;

      else

         Ada.Text_IO.Put_Line ("Error: attempting to use unitizalized segment");

      end if;

   end Intersection;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   procedure Publish (This : Segment_Record) is
   begin

      if This.Initialized then

         Ada.Text_IO.Put ("A -> ");

         This.Vertex_A.Publish;

         Ada.Text_IO.Put ("B -> ");

         This.Vertex_B.Publish;

      end if;

   end Publish;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   procedure Set_Vertices (This : in out Segment_Record; A, B : Vector2_Access) is
   begin

      if A /= null and B /= null then

         if A = B then

            Ada.Text_IO.Put_Line ("Error: attempting to set a segment with double reference to a unique vertex");

            This.Initialized := False;

            Abort_Meshing := True;

         else

            This.Vertex_A := A;

            This.Vertex_B := B;

            This.Initialized := True;

            This.Update_Properties;

            if This.Length = 0.0 then

               Ada.Text_IO.Put_Line ("Warning: segment set with zero lenght");

               This.Publish;

            end if;

         end if;

      else

         This.Initialized := False;

         Abort_Meshing := True;

         Ada.Text_IO.Put_Line ("Error: attempting to set a segment with a null reference");

      end if;

   end Set_Vertices;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   procedure Update_Properties (This : in out Segment_Record) is
   begin

      if This.Initialized then

         This.Length := This.Vertex_A.Distance (This.Vertex_B.all);

         This.Direction.Set (This.Vertex_B.Get_X - This.Vertex_A.Get_X, This.Vertex_B.Get_Y - This.Vertex_A.Get_Y);

         This.Direction.Normalize;

      end if;

   end Update_Properties;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   overriding procedure Initialize (This : in out Segment_Record) is
   begin

      This.Direction := new Vector2_Record;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   overriding procedure Adjust (This : in out Segment_Record) is
   begin

      null;

   end Adjust;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   overriding procedure Finalize (This : in out Segment_Record) is
   begin

      Free_Vector2 (This.Direction);

   end Finalize;
   -----------------------------------------------------------------------------

end Meshing.Segment;
--------------------------------------------------------------------------------
