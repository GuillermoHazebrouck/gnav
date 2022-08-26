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
with Ada.Unchecked_Conversion;
with Ada.Environment_Variables;
-- Gnav
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Gl is


   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Error traces are logged via a callback
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Log_Via_Callback : Boolean := False;


   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Basic math package for Gl_Float
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   package Math is new Ada.Numerics.Generic_Elementary_Functions (Float_Type => Gl_Float);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- String conversion
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   function To_Chars_Ptr is new Ada.Unchecked_Conversion (Source => Gl_Ubyte_Ptr,
                                                          Target => chars_ptr);

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Setup_Environment is
   begin

      Ada.Environment_Variables.Clear ("LIBGL_ALWAYS_INDIRECT");
      Ada.Environment_Variables.Set ("MESA_GL_VERSION_OVERRIDE", "3.3");

   end Setup_Environment;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Gets the element R, C of the matrix
   --===========================================================================
   function Get_Item (Matrix : Gl_Mat_4; R, C : Positive) return Gl_Float is
   begin
      return Matrix (4 * (C - 1) + R);
   end Get_Item;
   pragma Inline (Get_Item);
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Sets the element R, C of the matrix
   --===========================================================================
   procedure Set_Item (Matrix : in out Gl_Mat_4; R, C : Positive; Value : Gl_Float) is
   begin
      Matrix (4 * (C - 1) + R) := Value;
   end Set_Item;
   pragma Inline (Set_Item);
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Scale (Matrix : in out Gl_Mat_4; X, Y, Z : Float) is

      Scaling : Gl_Mat_4 := Gl_Mat_4_Identity;

   begin

      Set_Item (Scaling, 1, 1, X);
      Set_Item (Scaling, 2, 2, Y);
      Set_Item (Scaling, 3, 3, Z);

      Matrix := Multiply (Matrix, Scaling);

   end Scale;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Translate (Matrix : in out Gl_Mat_4; X, Y, Z : Float) is

      Offset : Gl_Mat_4 := Gl_Mat_4_Identity;

   begin

      Set_Item (Offset, 1, 4, X);
      Set_Item (Offset, 2, 4, Y);
      Set_Item (Offset, 3, 4, Z);

      Matrix := Multiply (Matrix, Offset);

   end Translate;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Postmultiplies Matrix by a flat rotation matrix about the Z axis
   --===========================================================================
   procedure Rotate (Matrix : in out Gl_Mat_4; Rz : Float) is

      Rotatation : Gl_Mat_4 := Gl_Mat_4_Identity;

      C : Gl_Float := Math.Cos (Rz);
      S : Gl_Float := Math.Sin (Rz);

   begin

      Set_Item (Rotatation, 1, 1, C);
      Set_Item (Rotatation, 1, 2,-S);
      Set_Item (Rotatation, 2, 1, S);
      Set_Item (Rotatation, 2, 2, C);

      Matrix := Multiply (Matrix, Rotatation);

   end Rotate;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Multiply (Matrix_1, Matrix_2 : Gl_Mat_4) return Gl_Mat_4 is

      Matrix : Gl_Mat_4;
      Sum    : Gl_Float;

   begin

      for I in 1..4 loop
         for J in 1..4 loop
            Sum := 0.0;
            for K in 1..4 loop
               Sum := Sum + Get_Item (Matrix_1, I, K) * Get_Item (Matrix_2, K, J);
            end loop;
            Set_Item (Matrix, I, J, Sum);
         end loop;
      end loop;

      return Matrix;

   end Multiply;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Displays the content in the standard output
   --===========================================================================
   procedure Dump (Matrix : Gl_Mat_4) is

   begin
      for I in 0..3 loop

         Utility.Log.Put_Message      (Float'Image (Matrix (I * 4 + 1)));
         Utility.Log.Put_Message      (Float'Image (Matrix (I * 4 + 2)));
         Utility.Log.Put_Message      (Float'Image (Matrix (I * 4 + 3)));
         Utility.Log.Put_Message      (Float'Image (Matrix (I * 4 + 4)));

      end loop;

      Utility.Log.Put_Message ("");

   end Dump;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- Displays the content in the standard output
   --===========================================================================
   function Get_String (Name : Gl_Enum) return String is

      function To_Chars_Ptr is new Ada.Unchecked_Conversion (Source => Gl_Ubyte_Ptr,
                                                             Target => chars_ptr);

      Data : Gl_Ubyte_Ptr := glGetString (Name);

      Er : Gl_Enum;

   begin
      if Data = null then
         Er := Gl.Get_Error;

         if    Er = GL_INVALID_ENUM  then return "invalid enum for "  & Gl_Enum'Image (Name);
         elsif Er = GL_INVALID_VALUE then return "invalid value for " & Gl_Enum'Image (Name);
         else                             return "unknown error for " & Gl_Enum'Image (Name);
         end if;
      end if;

      return Interfaces.C.Strings.Value (To_Chars_Ptr (Data));

   end Get_String;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Get shader source code
   --===========================================================================
   function Get_Shader_Source (Shader : Gl_Uint) return String is

      Data : String (1..1024) := (others => ' ');
      Length : aliased Gl_Sizei;

   begin

      glGetShaderSource (Shader, Data'Length, Length'Access, Data'Address);

      return Data (1..Natural (Length));

   end Get_Shader_Source;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Get shader info log. Usefull to get the compile status
   --===========================================================================
   function Get_Shader_Info_Log (Shader : Gl_Uint) return String is

      Data : String (1..1024) := (others => ' ');
      Length : aliased Gl_Sizei;

   begin

      glGetShaderInfoLog (Shader, Data'Length, Length'Access, Data'Address);

      return Data (1..Natural (Length));

   end Get_Shader_Info_Log;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Uniform_Location (Program  : Gl_Uint; Name : String) return Gl_Int is

   begin
      return glGetUniformLocation (Program, Interfaces.C.To_C (Name)'Address);

   end Get_Uniform_Location;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Gen_Textures (N : Gl_Sizei; Textures : in out Gl_Uint_Vec) is

      use type Gl_Sizei;

   begin
      if N <= Textures'Length
         then glGenTextures (N, Textures'Address);
         else Textures := (others => 0);
      end if;

   end Gen_Textures;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Log_Trace (Message : String) is
   begin

      Utility.Log.Put_Message ("OpenGL " & Message);

   end Log_Trace;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Log_Error (Message : String := "") is

      Er : Gl_Enum;
      I : Integer := 1;

   begin
      if Log_Via_Callback then return;
      end if;

      Er := Gl.Get_Error;

      while Er /= GL_NO_ERROR loop

         Utility.Log.Put_Message ("error (" & Integer'Image (I) & "):" & Gl_Enum'Image (Er));

         case Er is
            when GL_INVALID_VALUE     => Utility.Log.Put_Message (" (GL_INVALID_VALUE)");
            when GL_INVALID_ENUM      => Utility.Log.Put_Message (" (Gl_INVALID_ENUM)");
            when GL_INVALID_OPERATION => Utility.Log.Put_Message (" (GL_INVALID_OPERATION)");
            when GL_STACK_OVERFLOW    => Utility.Log.Put_Message (" (GL_STACK_OVERFLOW)");
            when GL_STACK_UNDERFLOW   => Utility.Log.Put_Message (" (GL_STACK_UNDERFLOW)");
            when GL_OUT_OF_MEMORY     => Utility.Log.Put_Message (" (GL_OUT_OF_MEMORY)");
            when others => null;
         end case;

         I := I + 1;

         Er := Gl.Get_Error;

      end loop;

   end Log_Error;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Trace error callback
   --===========================================================================
   procedure Error_Callback (Source    : Gl_Enum;
                             Gl_Type   : Gl_Enum;
                             Id        : Gl_Uint;
                             Severity  : Gl_Enum;
                             Length    : Gl_Sizei;
                             Message   : Gl_Ubyte_Ptr;
                             Userparam : System.Address);
   pragma Convention (C, Error_Callback);

   procedure Error_Callback (Source    : Gl_Enum;
                             Gl_Type   : Gl_Enum;
                             Id        : Gl_Uint;
                             Severity  : Gl_Enum;
                             Length    : Gl_Sizei;
                             Message   : Gl_Ubyte_Ptr;
                             Userparam : System.Address) is

   begin

      if Gl_Type = GL_DEBUG_TYPE_ERROR then

         Utility.Log.Put_Message ("GL error:" & Interfaces.C.Strings.Value (To_Chars_Ptr (message)));

      end if;

   exception
      when E : others =>
         Utility.Log.Put_Message (E, "when handling GL error callback");

   end Error_Callback;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Trace_Errors (Enable : Boolean) is

   begin

      if Enable then

         Gl.Log_Trace ("enabling debug output");

         Gl.Enable (GL_DEBUG_OUTPUT);

         Gl.Log_Error ("while enabling debug output");

         glDebugMessageCallback (Error_Callback'Address, System.Null_Address);

         Gl.Log_Error ("while loading error callback");

         Log_Via_Callback := True;

      else

         Log_Via_Callback := False;

         Gl.Log_Trace ("disabling debug output");

         Gl.Disable (GL_DEBUG_OUTPUT);

         Gl.Log_Error ("while disabling debug output");

      end if;

   end Trace_Errors;
   -----------------------------------------------------------------------------

begin

   Setup_Environment;

end Gl;
--------------------------------------------------------------------------------
