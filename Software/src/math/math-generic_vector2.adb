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
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
-- Gnav
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Math.Generic_Vector2 is


   package Math is new Ada.Numerics.Generic_Elementary_Functions (Float_Type);


   --===========================================================================
   --
   --===========================================================================
   procedure Set (This : in out Vector2_Record; X, Y : Float_Type) is
   begin

      This.X := X;

      This.Y := Y;

   end Set; pragma Inline (Set);
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_From_Polar (This : in out Vector2_Record; Orientation, Norm : Float_Type) is
   begin

      This.X := Norm * Math.Cos (Orientation);

      This.Y := Norm * Math.Sin (Orientation);

   end Set_From_Polar; pragma Inline (Set_From_Polar);
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_From_Difference (This : in out Vector2_Record; A, B : Vector2_Record'Class) is
   begin

      This.X := A.X - B.X;

      This.Y := A.Y - B.Y;

   end Set_From_Difference; pragma Inline (Set_From_Difference);
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_From_Addition (This : in out Vector2_Record; A, B : Vector2_Record'Class) is
   begin

      This.X := A.X + B.X;

      This.Y := A.Y + B.Y;

   end Set_From_Addition; pragma Inline (Set_From_Addition);
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_X (This : in out Vector2_Record; X : Float_Type) is
   begin

      This.X := X;

   end Set_X;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Y (This : in out Vector2_Record; Y : Float_Type) is
   begin

      This.Y := Y;

   end Set_Y;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Id (This : in out Vector2_Record; Id : Point_Identifier) is
   begin

      This.Id := Id;

   end Set_Id;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Normalize (This : in out Vector2_Record) is

      Norm : Float_Type := This.Norm2;

   begin

      if Norm > 0.0 then

         This.X := This.X / Norm;

         This.Y := This.Y / Norm;

      end if;

   end Normalize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Normalize (This : in out Vector2_Record; Norm : out Float_Type) is
   begin

      Norm := This.Norm2;

      if Norm > 0.0 then

         This.X := This.X / Norm;

         This.Y := This.Y / Norm;

      end if;

   end Normalize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_X (This : Vector2_Record) return Float_Type is
   begin

      return This.X;

   end Get_X;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Y (This : Vector2_Record) return Float_Type is
   begin

      return This.Y;

   end Get_Y;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Id (This : Vector2_Record) return Point_Identifier is
   begin

      return This.Id;

   end Get_Id;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Distance (This : Vector2_Record; Other : Vector2_Record'Class) return Float_Type is

      dx : Float_Type := This.X - Other.X;
      dy : Float_Type := This.Y - Other.Y;

   begin

      return Math.Sqrt(dx * dx + dy * dy);

   end Distance;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Distance (This : Vector2_Record; Other : Vector2_Access) return Float_Type is

      dx : Float_Type := This.X - Other.X;
      dy : Float_Type := This.Y - Other.Y;

   begin

      return Math.Sqrt(dx * dx + dy * dy);

   end Distance;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Distance (This : Vector2_Record; PointA, PointB : Vector2_Access) return Float_Type is

      Distance_AB : Float_Type := PointA.Distance (PointB.all);

      APx, APy, ABx, ABy, Projection, Distance_AP, Distance_BP : Float_Type;

      D : Float_Type := 0.0;

   begin

      if Distance_AB > 0.0 then

         APx := This.Get_X - PointA.Get_X;

         APy := This.Get_Y - PointA.Get_Y;

         ABx := PointB.Get_X - PointA.Get_X;

         ABy := PointB.Get_Y - PointA.Get_Y;

         Projection := (APx * ABx + APy * ABy) / Distance_AB;

         if Projection > 0.0 and then Projection < Distance_AB then

            D := (APy * ABx - APx * ABy) / (Distance_AB);

            if D >= 0.0 then

               return  D;

            else

               return -D;

            end if;

         else

            Distance_AP := PointA.Distance (This);

            Distance_BP := PointB.Distance (This);

            return Float_Type'Min (Distance_AP, Distance_BP);

         end if;

      else

         return PointA.Distance (This);

      end if;

   end Distance;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Contained (This : Vector2_Record; PointA, PointB : Vector2_Access; Offset : Float_Type) return Boolean is

      L, P : Float_Type;
      U, V : Vector2_Record;

   begin

      U.Set (PointB.Get_X - PointA.Get_X, PointB.Get_Y - PointA.Get_Y);

      U.Normalize (L);

      if L > 0.0 then

         V.Set (This.Get_X - PointA.Get_X, This.Get_Y - PointA.Get_Y);

         P := U.Dot_Product (V);

         if P >= 0.0 and P <= L then

            return (abs V.Cross_Product (U)) < Offset;

         end if;

      end if;

      return False;

   end Contained;
   --------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Norm2 (This : Vector2_Record) return Float_Type is
   begin

      return Math.Sqrt(This.X * This.X + This.Y * This.Y);

   end Norm2;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Dot_Product (This : Vector2_Record; Other : Vector2_Record'Class) return Float_Type is
   begin

      return This.X * Other.X + This.Y * Other.Y;

   end Dot_Product;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Dot_Product (This : Vector2_Record; Other : Vector2_Access) return Float_Type is
   begin

      return This.X * Other.X + This.Y * Other.Y;

   end Dot_Product;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Cross_Product (This : Vector2_Record; Other : Vector2_Record'Class) return Float_Type is
   begin

      return This.X * Other.Y - This.Y * Other.X;

   end Cross_Product;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Add (This : in out Vector2_Record; Other : Vector2_Record'Class) is
   begin

      This.X := This.X + Other.X;
      This.Y := This.Y + Other.Y;

   end Add;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Add (This : in out Vector2_Record; X, Y : Float_Type) is
   begin

      This.X := This.X + X;
      This.Y := This.Y + Y;

   end Add;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Revert (This : in out Vector2_Record) is
   begin

      This.X := - This.X;
      This.Y := - This.Y;

   end Revert;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Rotate (This : in out Vector2_Record; Angle : Float_Type) is

      x : Float_Type := This.X;
      y : Float_Type := This.Y;

      cos : Float_Type := Math.Cos (Angle);
      sin : Float_Type := Math.Sin (Angle);

   begin

      This.X := x * cos - y * sin;
      This.Y := x * sin + y * cos;

   end Rotate;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Rotate (This : in out Vector2_Record; Matrix : Rotation_Matrix) is

      x : Float_Type := This.X;
      y : Float_Type := This.Y;

   begin

      This.X := x * Matrix.Cos - y * Matrix.Sin;
      This.Y := x * Matrix.Sin + y * Matrix.Cos;

   end Rotate;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   procedure Scale (This : in out Vector2_Record; Factor : Float_Type) is
   begin

      This.X := Factor * This.X;

      This.Y := Factor * This.Y;

   end Scale;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Scale (This : in out Vector2_Record; Factor : Vector2_Record) is
   begin

      This.X := Factor.X * This.X;

      This.Y := Factor.Y * This.Y;

   end Scale;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Angle (This : Vector2_Record; Other_Vector : Vector2_Record) return Float_Type is

      L : Float_Type := This.Norm2 * Other_Vector.Norm2;

      X, Y : Float_Type;

   begin

      if L > 0.0 then

         X := This.Dot_Product (Other_Vector) / L;

         Y := This.Cross_Product (Other_Vector) / L;

         return Math.Arctan (Y, X);

      else

         return 0.0;

      end if;

   end Angle;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Orientation (This : Vector2_Record) return Float_Type is
   begin

      if This.Y = 0.0 and This.X = 0.0 then
         return 0.0;
      end if;

      return Math.Arctan (This.Y, This.X);

   end Orientation;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Bearing (This : Vector2_Record) return Float_Type is

      Angle : Float_Type;

   begin

      if This.Y = 0.0 and This.X = 0.0 then
         return 0.0;
      end if;

      Angle := Math.Arctan (This.Y, This.X);

      if Angle < 0.0 then

         Angle := Float_Type (TwoPi) + Angle;

      end if;

      return Angle;

   end Bearing;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Image (This : Vector2_Record; Decimals : Natural := 1) return String is
   begin

      return
        "X=" & Utility.Strings.Float_Image (Float (This.X), Decimals) & "; " &
        "Y=" & Utility.Strings.Float_Image (Float (This.Y), Decimals);

   end Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Copy_From_Record (This : in out Vector2_Record; Other : Vector2_Record'Class) is
   begin

      This.X := Other.X;

      This.Y := Other.Y;

   end Copy_From_Record;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Copy_From_Access (This : in out Vector2_Record; Other : Vector2_Access) is
   begin

      This.X := Other.X;

      This.Y := Other.Y;

   end Copy_From_Access;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Publish (This : Vector2_Record) is
   begin

      Ada.Text_IO.Put_Line ("(" & Point_Identifier'Image (This.Id) & ") -> " & This.Image);

   end Publish;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   overriding procedure Initialize (This : in out Vector2_Record) is
   begin

      This.X := 0.0;

      This.Y := 0.0;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   overriding procedure Adjust (This : in out Vector2_Record) is
   begin

      null;

   end Adjust;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   overriding procedure Finalize (This : in out Vector2_Record) is
   begin

      -- Ada.Text_IO.Put_Line ("Vector2 freed");

      null;

   end Finalize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function New_Vector2_Record (X, Y : Float_Type; Id : Point_Identifier := 1) return Vector2_Record is

      V : Vector2_Record := No_Vector2_Record;

   begin

      V.Set (X, Y);
      V.Id := Id;

      return V;

   end New_Vector2_Record;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Free_Vector2 (Vector2_To_Free : in out Vector2_Access) is
   begin

      Free_Vector2_Procedure (Vector2_To_Free);

   end Free_Vector2;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Build_Rotation_Matrix (Angle : Float_Type;
                                   Scale : Float_Type := 1.0) return Rotation_Matrix is

      Matrix : Rotation_Matrix;

   begin

      Matrix.Cos := Scale * Math.Cos (Angle);
      Matrix.Sin := Scale * Math.Sin (Angle);

      return Matrix;

   end Build_Rotation_Matrix;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- Returns a rotation matrix that aligns coordinates to the given vector.
   -- Use this method when you need to rotate many vectors by the same angle.
   --===========================================================================
   function Build_Rotation_Matrix (X1, Y1, X2, Y2 : Float_Type;
                                  Scale : Float_Type := 1.0) return Rotation_Matrix is

      Matrix : Rotation_Matrix := (Cos => 1.0, Sin => 0.0);

      X : Float_Type := (X2 - X1);
      Y : Float_Type := (Y2 - Y1);
      N : Float_Type := X * X + Y * Y;

   begin

      if N > 0.0 then

         N := Math.Sqrt (N);
         Matrix.Cos := Scale * X / N;
         Matrix.Sin := Scale * Y / N;

      end if;

      return Matrix;

   end Build_Rotation_Matrix;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   function "+" (V1 : in Vector2_Record; V2 : in Vector2_Record) return Vector2_Record is
   begin
      return (Stacks.Linked.Linked_Record with
              X  => V1.X + V2.X,
              Y  => V1.Y + V2.Y,
              Id => 1);
   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function "+" (V1 : in Vector2_Access; V2 : in Vector2_Access) return Vector2_Record is
   begin
      return (Stacks.Linked.Linked_Record with
              X  => V1.X + V2.X,
              Y  => V1.Y + V2.Y,
              Id => 1);
   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function "+" (V1 : in Vector2_Record; V2 : in Vector2_Access) return Vector2_Record is
   begin
      return (Stacks.Linked.Linked_Record with
              X  => V1.X + V2.X,
              Y  => V1.Y + V2.Y,
              Id => 1);
   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function "+" (V1 : in Vector2_Access; V2 : in Vector2_Record) return Vector2_Record is
   begin
      return (Stacks.Linked.Linked_Record with
              X  => V1.X + V2.X,
              Y  => V1.Y + V2.Y,
              Id => 1);
   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function "-" (V1 : in Vector2_Record; V2 : in Vector2_Record) return Vector2_Record is
   begin
      return (Stacks.Linked.Linked_Record with
              X  => V1.X - V2.X,
              Y  => V1.Y - V2.Y,
              Id => 1);
   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function "-" (V1 : in Vector2_Access; V2 : in Vector2_Access) return Vector2_Record is
   begin
      return (Stacks.Linked.Linked_Record with
              X  => V1.X - V2.X,
              Y  => V1.Y - V2.Y,
              Id => 1);
   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function "-" (V1 : in Vector2_Record; V2 : in Vector2_Access) return Vector2_Record is
   begin
      return (Stacks.Linked.Linked_Record with
              X  => V1.X - V2.X,
              Y  => V1.Y - V2.Y,
              Id => 1);
   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function "-" (V1 : in Vector2_Access; V2 : in Vector2_Record) return Vector2_Record is
   begin
      return (Stacks.Linked.Linked_Record with
              X  => V1.X - V2.X,
              Y  => V1.Y - V2.Y,
              Id => 1);
   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function "*" (V : in Vector2_Record; F : Float_Type) return Vector2_Record is
   begin
      return (Stacks.Linked.Linked_Record with
              X  => F * V.X,
              Y  => F * V.Y,
              Id => 1);
   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function "*" (F : Float_Type; V : in Vector2_Record) return Vector2_Record is
   begin
      return (Stacks.Linked.Linked_Record with
              X  => F * V.X,
              Y  => F * V.Y,
              Id => 1);
   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function "*" (V : in Vector2_Access; F : Float_Type) return Vector2_Record is
   begin
      return (Stacks.Linked.Linked_Record with
              X  => F * V.X,
              Y  => F * V.Y,
              Id => 1);
   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function "*" (F : Float_Type; V : in Vector2_Access) return Vector2_Record is
   begin
      return (Stacks.Linked.Linked_Record with
              X  => F * V.X,
              Y  => F * V.Y,
              Id => 1);
   end;
   -----------------------------------------------------------------------------



end Math.Generic_Vector2;
--------------------------------------------------------------------------------
