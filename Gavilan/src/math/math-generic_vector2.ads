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
with Ada.Unchecked_Deallocation;
-- Gnav
with Stacks.Linked;

--//////////////////////////////////////////////////////////////////////////////
-- Contains the definition of an object that represents a 2D point by a set
-- orthognal coordinates, and exposes geometrical operations on it.
--//////////////////////////////////////////////////////////////////////////////
generic

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The type used to represent the orthogonal coordinates and the operations.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Float_Type is digits <>;

package Math.Generic_Vector2 is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The identification of an item
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Point_Identifier is Positive;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A Vector2 object.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Vector2_Record is new Stacks.Linked.Linked_Record with private;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A reference to a Vector2 object.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Vector2_Access is access all Vector2_Record'Class;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A rotation matrix
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Rotation_Matrix is record

      Sin, Cos : Float_Type;

   end record;


   --===========================================================================
   -- Sets the orthogonal coordinates.
   --===========================================================================
   procedure Set (This : in out Vector2_Record; X, Y : Float_Type);

   --===========================================================================
   -- Sets the vector from polar coordinates. Orientation in radians.
   --===========================================================================
   procedure Set_From_Polar (This : in out Vector2_Record; Orientation, Norm : Float_Type);

   --===========================================================================
   -- Sets this vector as the substraction A - B
   --===========================================================================
   procedure Set_From_Difference (This : in out Vector2_Record; A, B : Vector2_Record'Class);

   --===========================================================================
   -- Sets this vector as the addition A + B
   --===========================================================================
   procedure Set_From_Addition (This : in out Vector2_Record; A, B : Vector2_Record'Class);

   --===========================================================================
   -- Sets the X coordinate.
   --===========================================================================
   procedure Set_X (This : in out Vector2_Record; X : Float_Type);

   --===========================================================================
   -- Sets the Y coordinate.
   --===========================================================================
   procedure Set_Y (This : in out Vector2_Record; Y : Float_Type);

   --===========================================================================
   -- Sets the Id.
   --===========================================================================
   procedure Set_Id (This : in out Vector2_Record; Id : Point_Identifier);

   --===========================================================================
   -- Sets the module of the vector to 1, while keeping its direction constant.
   --===========================================================================
   procedure Normalize (This : in out Vector2_Record);

   --===========================================================================
   -- Sets the module of the vector to 1, while keeping its direction constant.
   --===========================================================================
   procedure Normalize (This : in out Vector2_Record; Norm : out Float_Type);

   --===========================================================================
   -- Gets the X coordinate.
   --===========================================================================
   function Get_X (This : Vector2_Record) return Float_Type;

   --===========================================================================
   -- Gets the Y coordinate.
   --===========================================================================
   function Get_Y (This : Vector2_Record) return Float_Type;

   --===========================================================================
   -- Gets the Id.
   --===========================================================================
   function Get_Id (This : Vector2_Record) return Point_Identifier;

   --===========================================================================
   -- Gets the distance between this vector and the provided one.
   --===========================================================================
   function Distance (This : Vector2_Record; Other : Vector2_Record'Class) return Float_Type;

   --===========================================================================
   -- Gets the distance between this vector and the provided one.
   --===========================================================================
   function Distance (This : Vector2_Record; Other : Vector2_Access) return Float_Type;

   --===========================================================================
   -- Gets the distance between this vector and the segment represented by the
   -- given points.
   --===========================================================================
   function Distance (This : Vector2_Record; PointA, PointB : Vector2_Access) return Float_Type;

   --===========================================================================
   -- Indicates if this point is contained within the segment given by the two
   -- points at a distance less than the offset.
   --===========================================================================
   function Contained (This : Vector2_Record; PointA, PointB : Vector2_Access; Offset : Float_Type) return Boolean;

   --===========================================================================
   -- Returns the norm2 of this vector.
   --===========================================================================
   function Norm2 (This : Vector2_Record) return Float_Type;

   --===========================================================================
   -- Calculates the inner product between this and the provided vector.
   --===========================================================================
   function Dot_Product (This : Vector2_Record; Other : Vector2_Record'Class) return Float_Type;

   --===========================================================================
   -- Calculates the inner product between this and the provided vector.
   --===========================================================================
   function Dot_Product (This : Vector2_Record; Other : Vector2_Access) return Float_Type;

   --===========================================================================
   -- Returns the normal component of the cross product between the two vectors.
   --===========================================================================
   function Cross_Product (This : Vector2_Record; Other : Vector2_Record'Class) return Float_Type;

   --===========================================================================
   -- Adds the given vector.
   --===========================================================================
   procedure Add (This : in out Vector2_Record; Other : Vector2_Record'Class);

   --===========================================================================
   -- Adds the given coordinates.
   --===========================================================================
   procedure Add (This : in out Vector2_Record; X, Y : Float_Type);

   --===========================================================================
   -- Switches the direction.
   --===========================================================================
   procedure Revert (This : in out Vector2_Record);

   --===========================================================================
   -- Rotates the vector (the angle must be in radians).
   --===========================================================================
   procedure Rotate (This : in out Vector2_Record; Angle : Float_Type);

   --===========================================================================
   -- Rotates the vector using the given rotation matrix.
   --===========================================================================
   procedure Rotate (This : in out Vector2_Record; Matrix : Rotation_Matrix);

   --===========================================================================
   -- Scales the vector.
   --===========================================================================
   procedure Scale (This : in out Vector2_Record; Factor : Float_Type);

   --===========================================================================
   -- Scales the coordinates of the vector by independent factors.
   --===========================================================================
   procedure Scale (This : in out Vector2_Record; Factor : Vector2_Record);

   --===========================================================================
   -- Returns the angle of this vector relative to the given vector.
   -- The angle varies in the range [-Pi;Pi]. The sign depends on how This
   -- vector should rotate to align with the Other_Vector:
   -- > Counterclockwise > (+)
   -- > Clockwise        > (-)
   --===========================================================================
   function Angle (This : Vector2_Record; Other_Vector : Vector2_Record) return Float_Type;

   --===========================================================================
   -- Returns the orientation of the vector relative to the X axis.
   -- The result is in the range [-Pi;Pi].
   --===========================================================================
   function Orientation (This : Vector2_Record) return Float_Type;

   --===========================================================================
   -- Returns the angle of the vector relative to the X axis.
   -- The result is in the range [0;2.Pi].
   --===========================================================================
   function Bearing (This : Vector2_Record) return Float_Type;

   --===========================================================================
   -- Returns a string of this vector.
   --===========================================================================
   function Image (This : Vector2_Record; Decimals : Natural := 1) return String;

   --===========================================================================
   -- Copies the coordinates from the given vector.
   --===========================================================================
   procedure Copy_From_Record (This : in out Vector2_Record; Other : Vector2_Record'Class);

   --===========================================================================
   -- Copies the coordinates from the given vector.
   --===========================================================================
   procedure Copy_From_Access (This : in out Vector2_Record; Other : Vector2_Access);

   --===========================================================================
   -- Outputs the coordinate and id of the vector
   --===========================================================================
   procedure Publish (This : Vector2_Record);

   --===========================================================================
   --
   --===========================================================================
   overriding procedure Initialize (This : in out Vector2_Record);

   --===========================================================================
   --
   --===========================================================================
   overriding procedure Adjust (This : in out Vector2_Record);

   --===========================================================================
   --
   --===========================================================================
   overriding procedure Finalize (This : in out Vector2_Record);

   --===========================================================================
   -- Array of Vector2.
   --===========================================================================
   type Vector2_Array is array (Positive range <>) of Vector2_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Initialization value.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Vector2_Record : constant Vector2_Record;

   --===========================================================================
   -- Returns a Vector2_Record with the provided coordinates.
   --===========================================================================
   function New_Vector2_Record (X, Y : Float_Type; Id : Point_Identifier := 1) return Vector2_Record;

   --===========================================================================
   -- Frees the memory of a Vector2.
   --===========================================================================
   procedure Free_Vector2 (Vector2_To_Free : in out Vector2_Access);

   --===========================================================================
   -- Returns a rotation matrix to rotate vectors by the given angle (radians)
   -- and apply the given scale factor at the same time.
   -- Use this method when you need to rotate many vectors by the same angle.
   --===========================================================================
   function Build_Rotation_Matrix (Angle : Float_Type;
                                   Scale : Float_Type := 1.0) return Rotation_Matrix;

   --===========================================================================
   -- Returns a rotation matrix that aligns coordinates to the given vector.
   -- Use this method when you need to rotate many vectors by the same angle.
   --===========================================================================
   function Build_Rotation_Matrix (X1, Y1, X2, Y2 : Float_Type;
                                   Scale : Float_Type := 1.0) return Rotation_Matrix;

   --===========================================================================
   -- Addition of 2 vectors
   --===========================================================================
   function "+" (V1 : in Vector2_Record; V2 : in Vector2_Record) return Vector2_Record;
   function "+" (V1 : in Vector2_Access; V2 : in Vector2_Access) return Vector2_Record;
   function "+" (V1 : in Vector2_Access; V2 : in Vector2_Record) return Vector2_Record;
   function "+" (V1 : in Vector2_Record; V2 : in Vector2_Access) return Vector2_Record;

   --===========================================================================
   -- Subtraction of 2 vectors
   --===========================================================================
   function "-" (V1 : in Vector2_Record; V2 : in Vector2_Record) return Vector2_Record;
   function "-" (V1 : in Vector2_Access; V2 : in Vector2_Access) return Vector2_Record;
   function "-" (V1 : in Vector2_Access; V2 : in Vector2_Record) return Vector2_Record;
   function "-" (V1 : in Vector2_Record; V2 : in Vector2_Access) return Vector2_Record;

   --===========================================================================
   -- Multiply the given vector by a factor F.
   --===========================================================================
   function "*" (V : in Vector2_Record; F : Float_Type) return Vector2_Record;
   function "*" (F : Float_Type; V : in Vector2_Record) return Vector2_Record;
   function "*" (V : in Vector2_Access; F : Float_Type) return Vector2_Record;
   function "*" (F : Float_Type; V : in Vector2_Access) return Vector2_Record;



private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The Vector2
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Vector2_Record is new Stacks.Linked.Linked_Record with record

      X  : Float_Type := 0.0;

      Y  : Float_Type := 0.0;

      Id : Point_Identifier := 1;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Frees the memory of a Vector2.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Vector2_Record : constant Vector2_Record := (Stacks.Linked.Linked_Record with
                                                   X  => 0.0,
                                                   Y  => 0.0,
                                                   Id => 1);

   --===========================================================================
   -- Frees the memory of a Vector2.
   --===========================================================================
   procedure Free_Vector2_Procedure is new Ada.Unchecked_Deallocation(Object => Vector2_Record'Class, Name => Vector2_Access);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The Rotation_Matrix
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --type Rotation_Matrix is record

   --   Sin, Cos : Float_Type;

   --end record;

end Math.Generic_Vector2;
