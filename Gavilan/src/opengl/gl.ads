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
with Interfaces.C;
with Interfaces.C.Strings;
use  Interfaces.C.Strings;
with System;

--//////////////////////////////////////////////////////////////////////////////
-- This is an Ada binding to the libGL.so library using the "core profile"
-- functions.
-- This package is compatible with OpenGL versions 3.x and 4.x. OpenGL 1.x 2.x
-- versions are based in the legacy routines glBegin/glEnd, and do not support
-- the vertex buffering technique provided here.
--//////////////////////////////////////////////////////////////////////////////
package Gl is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Data types (may be architecture dependent in some cases)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type    Void             is null record;
   subtype Gl_Void          is Void;
   subtype Gl_Boolean       is Interfaces.C.unsigned_char;
   subtype Gl_Byte          is Interfaces.C.signed_char;            --  1-byte signed
   subtype Gl_Short         is Interfaces.C.short;                  --  2-byte signed
   subtype Gl_Int           is Interfaces.C.int;                    --  4-byte signed
   type    Gl_Int_Vec_4     is array (0 .. 3) of Gl_Int;
   subtype Gl_Ubyte         is Interfaces.C.unsigned_char;          --  1-byte unsigned
   type    Gl_Ubyte_Ptr     is access Gl_Ubyte;
   subtype Gl_Ushort        is Interfaces.C.unsigned_short;         --  2-byte unsigned
   type    Gl_Ushort_Vec    is array (Natural range <>) of Gl_Ushort;
   subtype Gl_Uint          is Interfaces.C.unsigned;               --  4-byte unsigned
   type    Gl_Uint_Vec      is array (Natural range <>) of Gl_Uint;
   subtype Gl_Sizei         is Interfaces.C.int;                    --  4-byte signed
   type    Gl_Sizeiptr      is new Long_Integer; for Gl_Sizeiptr'Size use Standard'Address_Size;
   subtype Gl_Float         is Float;                               --  single precision float
   type    Gl_Float_Vec     is array (Positive range <>) of Gl_Float;
   type    Gl_Float_Vec_16  is array (0 .. 15) of Gl_Float;
   type    Gl_Float_Vec_4   is array (0 .. 3)  of Gl_Float;
   type    Gl_Float_Vec_2   is array (0 .. 1)  of Gl_Float;
   subtype Gl_Clampf        is Float;                               --  single precision float in [0;1]
   subtype Gl_Double        is Long_Float;                          --  double precision float
   type    Gl_Double_Vec_3  is array (0 .. 2)  of Gl_Double;
   type    Gl_Double_Vec_16 is array (0 .. 15) of Gl_Double;
   subtype Gl_Clampd        is Long_Float;                          --  double precision float in [0;1]
   type    Gl_Enum          is new Integer; for Gl_Enum'Size use 32;
   type    Gl_Bitfield      is mod 16#100000#;
   subtype Gl_Outline       is Gl_Enum range 0..3;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The number of bytes in a Gl_Float
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Gl_Float_Size : constant Gl_Sizei := Gl_Sizei (Gl_Float'Size / 8);

   --///////////////////////////////////////////////////////////////////////////
   -- OpenGL enumerations and other constants
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Boolean values
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_FALSE : constant Gl_Enum := 0;
   GL_TRUE  : constant Gl_Enum := 1;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Data types
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_TYPE_BYTE           : constant Gl_Enum := 16#1400#;
   GL_TYPE_UNSIGNED_BYTE  : constant Gl_Enum := 16#1401#;
   GL_TYPE_SHORT          : constant Gl_Enum := 16#1402#;
   GL_TYPE_UNSIGNED_SHORT : constant Gl_Enum := 16#1403#;
   GL_TYPE_INT            : constant Gl_Enum := 16#1404#;
   GL_TYPE_UNSIGNED_INT   : constant Gl_Enum := 16#1405#;
   GL_TYPE_FLOAT          : constant Gl_Enum := 16#1406#;
   GL_TYPE_2_BYTES        : constant Gl_Enum := 16#1407#;
   GL_TYPE_3_BYTES        : constant Gl_Enum := 16#1408#;
   GL_TYPE_4_BYTES        : constant Gl_Enum := 16#1409#;
   GL_TYPE_DOUBLE         : constant Gl_Enum := 16#140A#;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Primitives
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_POINTS         : constant Gl_Enum := 0;
   GL_LINES          : constant Gl_Enum := 1;
   GL_LINE_LOOP      : constant Gl_Enum := 2;
   GL_LINE_STRIP     : constant Gl_Enum := 3;
   GL_TRIANGLES      : constant Gl_Enum := 4;
   GL_TRIANGLE_STRIP : constant Gl_Enum := 5;
   GL_TRIANGLE_FAN   : constant Gl_Enum := 6;
   GL_POLYGON        : constant Gl_Enum := 9;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Points
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_POINT_SIZE : constant Gl_Enum := 2833;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Blending
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_BLEND               : constant Gl_Enum := 3042;
   GL_BLEND_SRC           : constant Gl_Enum := 3041;
   GL_SRC_ALPHA           : constant Gl_Enum := 770;
   GL_ONE_MINUS_SRC_ALPHA : constant Gl_Enum := 771;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Utility
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_VENDOR     : constant Gl_Enum := 7936;
   GL_RENDERER   : constant Gl_Enum := 7937;
   GL_VERSION    : constant Gl_Enum := 7938;
   GL_EXTENSIONS : constant Gl_Enum := 7939;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Depth buffer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_LESS : constant Gl_Enum := 513;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Others
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_ARRAY_BUFFER       : constant Gl_Enum := 16#8892#;
   GL_READ_ONLY          : constant Gl_Enum := 16#88B8#;
   GL_WRITE_ONLY         : constant Gl_Enum := 16#88B9#;
   GL_READ_WRITE         : constant Gl_Enum := 16#88BA#;
   GL_BUFFER_ACCESS      : constant Gl_Enum := 16#88BB#;
   GL_BUFFER_MAPPED      : constant Gl_Enum := 16#88BC#;
   GL_BUFFER_MAP_POINTER : constant Gl_Enum := 16#88BD#;
   GL_STREAM_DRAW        : constant Gl_Enum := 16#88E0#;
   GL_STREAM_READ        : constant Gl_Enum := 16#88E1#;
   GL_STREAM_COPY        : constant Gl_Enum := 16#88E2#;
   GL_STATIC_DRAW        : constant Gl_Enum := 16#88E4#;
   GL_STATIC_READ        : constant Gl_Enum := 16#88E5#;
   GL_STATIC_COPY        : constant Gl_Enum := 16#88E6#;
   GL_DYNAMIC_DRAW       : constant Gl_Enum := 16#88E8#;
   GL_DYNAMIC_READ       : constant Gl_Enum := 16#88E9#;
   GL_DYNAMIC_COPY       : constant Gl_Enum := 16#88EA#;
   GL_SAMPLES_PASSED     : constant Gl_Enum := 16#8914#;

   GL_FRAGMENT_SHADER          : constant Gl_Enum := 16#8B30#;
   GL_VERTEX_SHADER            : constant Gl_Enum := 16#8B31#;
   GL_GEOMETRY_SHADER          : constant Gl_Enum := 16#8DD9#;
   GL_SHADER_TYPE              : constant Gl_Enum := 16#8B4F#;
   GL_COMPILE_STATUS           : constant Gl_Enum := 16#8B81#;
   GL_SHADING_LANGUAGE_VERSION : constant Gl_Enum := 16#8B8C#;
   GL_MAX_GEOMETRY_OUTPUT_VERTICES : constant Gl_Enum := 16#8DE0#;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Attribute fields
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_CURRENT_BIT         : constant Gl_Bitfield := 16#00000001#;
   GL_POINT_BIT           : constant Gl_Bitfield := 16#00000002#;
   GL_LINE_BIT            : constant Gl_Bitfield := 16#00000004#;
   GL_POLYGON_BIT         : constant Gl_Bitfield := 16#00000008#;
   GL_POLYGON_STIPPLE_BIT : constant Gl_Bitfield := 16#00000010#;
   GL_PIXEL_MODE_BIT      : constant Gl_Bitfield := 16#00000020#;
   GL_LIGHTING_BIT        : constant Gl_Bitfield := 16#00000040#;
   GL_FOG_BIT             : constant Gl_Bitfield := 16#00000080#;
   GL_DEPTH_BUFFER_BIT    : constant Gl_Bitfield := 16#00000100#;
   GL_ACCUM_BUFFER_BIT    : constant Gl_Bitfield := 16#00000200#;
   GL_STENCIL_BUFFER_BIT  : constant Gl_Bitfield := 16#00000400#;
   GL_VIEWPORT_BIT        : constant Gl_Bitfield := 16#00000800#;
   GL_TRANSFORM_BIT       : constant Gl_Bitfield := 16#00001000#;
   GL_ENABLE_BIT          : constant Gl_Bitfield := 16#00002000#;
   GL_COLOR_BUFFER_BIT    : constant Gl_Bitfield := 16#00004000#;
   GL_HINT_BIT            : constant Gl_Bitfield := 16#00008000#;
   GL_EVAL_BIT            : constant Gl_Bitfield := 16#00010000#;
   GL_LIST_BIT            : constant Gl_Bitfield := 16#00020000#;
   GL_TEXTURE_BIT         : constant Gl_Bitfield := 16#00040000#;
   GL_SCISSOR_BIT         : constant Gl_Bitfield := 16#00080000#;
   GL_ALL_ATTRIB_BITS     : constant Gl_Bitfield := 16#000fffff#;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Texture mapping
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_TEXTURE_2D             : constant Gl_Enum := 3553;
   GL_TEXTURE_WRAP_S         : constant Gl_Enum := 10242;
   GL_TEXTURE_WRAP_T         : constant Gl_Enum := 10243;
   GL_TEXTURE_MAG_FILTER     : constant Gl_Enum := 10240;
   GL_TEXTURE_MIN_FILTER     : constant Gl_Enum := 10241;
   GL_TEXTURE_WIDTH          : constant Gl_Enum := 4096;
   GL_TEXTURE_HEIGHT         : constant Gl_Enum := 4097;
   GL_NEAREST_MIPMAP_NEAREST : constant Gl_Enum := 9984;
   GL_NEAREST_MIPMAP_LINEAR  : constant Gl_Enum := 9986;
   GL_LINEAR_MIPMAP_NEAREST  : constant Gl_Enum := 9985;
   GL_LINEAR_MIPMAP_LINEAR   : constant Gl_Enum := 9987;
   GL_NEAREST                : constant Gl_Enum := 9728;
   GL_LINEAR                 : constant Gl_Enum := 9729;
   GL_REPEAT                 : constant Gl_Enum := 10497;
   GL_CLAMP                  : constant Gl_Enum := 10496;
   GL_CLAMP_TO_EDGE          : constant Gl_Enum := 16#812F#;
   GL_BGRA                   : constant Gl_Enum := 32993;
   GL_RGBA                   : constant Gl_Enum := 6408;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Errors
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_NO_ERROR          : constant := 0;
   GL_INVALID_VALUE     : constant Gl_Enum := 1281;
   GL_INVALID_ENUM      : constant Gl_Enum := 1280;
   GL_INVALID_OPERATION : constant Gl_Enum := 1282;
   GL_STACK_OVERFLOW    : constant Gl_Enum := 1283;
   GL_STACK_UNDERFLOW   : constant Gl_Enum := 1284;
   GL_OUT_OF_MEMORY     : constant Gl_Enum := 1285;

   GL_DEBUG_TYPE_ERROR  : constant Gl_Enum := 16#824C#;
   GL_DEBUG_OUTPUT      : constant Gl_Enum := 16#92E0#;

   --///////////////////////////////////////////////////////////////////////////
   -- Suplementaty types and transformations
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A 4x4 matrix
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Gl_Mat_4 is private;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- 4x4 identity matrix
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Gl_Mat_4_Identity : constant Gl_Mat_4;

   --===========================================================================
   -- Postmultiplies the matrix by a scaling matrix
   --===========================================================================
   procedure Scale (Matrix : in out Gl_Mat_4; X, Y, Z : Float);

   --===========================================================================
   -- Postmultiplies the matrix by an offset matrix
   --===========================================================================
   procedure Translate (Matrix : in out Gl_Mat_4; X, Y, Z : Float);

   --===========================================================================
   -- Postmultiplies the matrix by a flat rotation matrix about the Z axis
   -- NOTE: the angle must be in radians.
   --===========================================================================
   procedure Rotate (Matrix : in out Gl_Mat_4; Rz : Float);

   --===========================================================================
   -- Returns the matrix product
   --===========================================================================
   function Multiply (Matrix_1, Matrix_2 : Gl_Mat_4) return Gl_Mat_4;

   --===========================================================================
   -- Displays the content in the standard output
   --===========================================================================
   procedure Dump (Matrix : Gl_Mat_4);

   --///////////////////////////////////////////////////////////////////////////
   -- Standard OpenGL calls
   --///////////////////////////////////////////////////////////////////////////

   --===========================================================================
   -- General
   --===========================================================================
   procedure Enable     (Cap  : Gl_Enum);
   procedure Disable    (Cap  : Gl_Enum);
   function  Get_Error return Gl_Enum;
   function  Get_String  (Name : Gl_Enum) return String;

   --===========================================================================
   -- Viewport configuration
   --===========================================================================
   procedure Viewport (X, Y : Gl_Int; W, H : Gl_Sizei);

   --===========================================================================
   -- Drawing
   --===========================================================================
   procedure Clear        (Mask : Gl_Bitfield);
   procedure Clear_Color  (R, G, B, A : Gl_Clampf);
   procedure Point_Size   (Value : Gl_Float);
   procedure Line_Width   (Value : Gl_Float);

   --===========================================================================
   -- Uniforms
   --===========================================================================
   function  Get_Uniform_Location (Program  : Gl_Uint; Name : String) return Gl_Int;
   procedure Uniform_1f           (Location : Gl_Int; V1 : Gl_Float);
   procedure Uniform_2f           (Location : Gl_Int; V1, V2 : Gl_Float);
   procedure Uniform_3f           (Location : Gl_Int; V1, V2, V3 : Gl_Float);
   procedure Uniform_4f           (Location : Gl_Int; V1, V2, V3, V4 : Gl_Float);
   procedure Uniform_Matrix_4fv   (Location : Gl_Int; Size : Gl_Sizei; Transpose : Gl_Boolean; Matrix : not null access Gl_Mat_4);

   --===========================================================================
   -- Functions
   --===========================================================================
   procedure Blend_Func (Sfactor : Gl_Enum; Dfactor : Gl_Enum);

   --===========================================================================
   -- Buffers
   --===========================================================================
   procedure Gen_Buffers    (N : Gl_Sizei; Buffers : not null access Gl_Uint_Vec);
   procedure Delete_Buffers (N : Gl_Sizei; Buffers : not null access Gl_Uint_Vec);
   procedure Bind_Buffer    (Target : Gl_Enum; Buffer : Gl_Uint);
   procedure Buffer_Data    (Target : Gl_Enum;
                             Size   : Gl_Sizeiptr;
                             Data   : access Gl_Float_Vec;
                             Usage  : Gl_Enum);

   --===========================================================================
   -- Vertex array
   --===========================================================================
   procedure Gen_Vertex_Arrays (N : Gl_Sizei; Arrays : access Gl_Uint_Vec);
   procedure Bind_Vertex_Array (Vertex_Array : Gl_Uint);
   procedure Draw_Arrays       (Mode  : Gl_Enum;
                                First : Gl_Int;
                                Count : Gl_Sizei);
   procedure Draw_Elements     (Mode    : Gl_Enum;
                                Count   : Gl_Sizei;
                                Type_Id : Gl_Enum;
                                Indices : access Gl_Ushort_Vec);

   procedure Enable_Vertex_Attrib_Array  (Index : Gl_Uint);
   procedure Disable_Vertex_Attrib_Array (Index : Gl_Uint);
   procedure Vertex_Attrib_Pointer       (Index      : Gl_Uint;
                                          Size       : Gl_Int;
                                          Kind       : Gl_Enum;
                                          Normalized : Gl_Enum;
                                          Stride     : Gl_Sizei;
                                          Offset     : Gl_Sizei := 0);

   --===========================================================================
   -- Shader procedures
   --===========================================================================
   function  Create_Shader       (Kind   : Gl_Enum) return Gl_Uint;
   procedure Shader_Source       (Shader : Gl_Uint; Count : Gl_Sizei; Code : not null access Chars_Ptr_Array; Length : access Gl_Int);
   function  Get_Shader_Source   (Shader : Gl_Uint) return String;
   procedure Compile_Shader      (Shader : Gl_Uint);
   function  Get_Shader_Info_Log (Shader : Gl_Uint) return String;

   --===========================================================================
   -- Shader programs
   --===========================================================================
   function  Create_Program return Gl_Uint;
   procedure Attach_Shader (Program : Gl_Uint; Shader : Gl_Uint);
   procedure Link_Program  (Program : Gl_Uint);
   procedure Use_Program   (Program : Gl_Uint);

   --===========================================================================
   -- Textures
   --===========================================================================
   procedure Tex_Image_2D    (Target          : Gl_Enum;
                              Level           : Gl_Int;
                              Internal_Format : Gl_Int;
                              Width           : Gl_Sizei;
                              Height          : Gl_Sizei;
                              Border          : Gl_Int;
                              Format          : Gl_Enum;
                              Type_Id         : Gl_Enum;
                              Pixels          : System.Address);
   procedure Gen_Textures    (N : Gl_Sizei; Textures : in out Gl_Uint_Vec);
   procedure Delete_Textures (N : Gl_Sizei; Textures : not null access Gl_Uint);
   procedure Bind_Texture    (Target  : Gl_Enum;
                              Texture : Gl_Uint);
   procedure Tex_Parameter_I (Target : Gl_Enum;
                              Pname  : Gl_Enum;
                              Param  : Gl_Enum);

   --===========================================================================
   -- Log to the trace file with a header ("OpenGL ")
   --===========================================================================
   procedure Log_Trace (Message : String);

   --===========================================================================
   -- Checks if there is an error and logs the message into the error trace
   --===========================================================================
   procedure Log_Error (Message : String := "");

   --===========================================================================
   -- Trace all errors with a callback
   --===========================================================================
   procedure Trace_Errors (Enable : Boolean);


private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Global functions and procedures
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   pragma Import (Stdcall, Enable,                      "glEnable");
   pragma Import (Stdcall, Disable,                     "glDisable");
   pragma Import (Stdcall, Clear,                       "glClear");
   pragma Import (Stdcall, Clear_Color,                 "glClearColor");
   pragma Import (Stdcall, Get_Error,                   "glGetError");
   pragma Import (Stdcall, Viewport,                    "glViewport");
   pragma Import (Stdcall, Point_Size,                  "glPointSize");
   pragma Import (Stdcall, Line_Width,                  "glLineWidth");
   pragma Import (Stdcall, Uniform_1f,                  "glUniform1f");
   pragma Import (Stdcall, Uniform_2f,                  "glUniform2f");
   pragma Import (Stdcall, Uniform_3f,                  "glUniform3f");
   pragma Import (Stdcall, Uniform_4f,                  "glUniform4f");
   pragma Import (Stdcall, Uniform_Matrix_4fv,          "glUniformMatrix4fv");
   pragma Import (Stdcall, Blend_Func,                  "glBlendFunc");
   pragma Import (Stdcall, Gen_Buffers,                 "glGenBuffers");
   pragma Import (Stdcall, Delete_Buffers,              "glDeleteBuffers");
   pragma Import (Stdcall, Bind_Buffer,                 "glBindBuffer");
   pragma Import (Stdcall, Buffer_Data,                 "glBufferData");
   pragma Import (Stdcall, Gen_Vertex_Arrays,           "glGenVertexArrays");
   pragma Import (Stdcall, Bind_Vertex_Array,           "glBindVertexArray");
   pragma Import (Stdcall, Draw_Arrays,                 "glDrawArrays");
   pragma Import (Stdcall, Draw_Elements,               "glDrawElements");
   pragma Import (Stdcall, Enable_Vertex_Attrib_Array,  "glEnableVertexAttribArray");
   pragma Import (Stdcall, Disable_Vertex_Attrib_Array, "glDisableVertexAttribArray");
   pragma Import (Stdcall, Vertex_Attrib_Pointer,       "glVertexAttribPointer");
   pragma Import (Stdcall, Create_Shader,               "glCreateShader");
   pragma Import (Stdcall, Shader_Source,               "glShaderSource");
   pragma Import (Stdcall, Compile_Shader,              "glCompileShader");
   pragma Import (Stdcall, Create_Program,              "glCreateProgram");
   pragma Import (Stdcall, Attach_Shader,               "glAttachShader");
   pragma Import (Stdcall, Link_Program,                "glLinkProgram");
   pragma Import (Stdcall, Use_Program,                 "glUseProgram");
   pragma Import (Stdcall, Tex_Image_2D,                "glTexImage2D");
   pragma Import (Stdcall, Delete_Textures,             "glDeleteTextures");
   pragma Import (Stdcall, Bind_Texture,                "glBindTexture");
   pragma Import (Stdcall, Tex_Parameter_I,             "glTexParameteri");


   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Local functions and procedures
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   function  glGetString            (Name : Gl_Enum) return Gl_Ubyte_Ptr;
   function  glGetUniformLocation   (Program : Gl_Uint; Name : System.Address) return Gl_Int;
   procedure glGetShaderiv          (Shader : Gl_Uint; Pname : Gl_Enum; Params : access Gl_Int);
   procedure glGetShaderInfoLog     (Shader : Gl_Uint; maxLength : Gl_Sizei; length : access Gl_Sizei; infoLog : System.Address);
   procedure glGetShaderSource      (Shader : Gl_Uint; bufSize : Gl_Sizei; length : access Gl_Sizei; source : System.Address);
   procedure glDebugMessageCallback (Callback : System.Address; userParam : System.Address);
   procedure glGenTextures          (n : Gl_Sizei; textures : System.Address);

   pragma Import (Stdcall, glGetString,                 "glGetString");
   pragma Import (Stdcall, glGetUniformLocation,        "glGetUniformLocation");
   pragma Import (Stdcall, glGetShaderiv,               "glGetShaderiv");
   pragma Import (Stdcall, glGetShaderSource,           "glGetShaderSource");
   pragma Import (Stdcall, glGetShaderInfoLog,          "glGetShaderInfoLog");
   pragma Import (Stdcall, glDebugMessageCallback,      "glDebugMessageCallback");
   pragma Import (Stdcall, glGenTextures,               "glGenTextures");


   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The plain storage of the matrix items
   -- OpenGL stores matrices by columns.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Gl_Mat_4 is array (1..16) of Gl_Float;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- 4x4 identity matrix
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Gl_Mat_4_Identity : constant Gl_Mat_4 :=  (1.0, 0.0, 0.0, 0.0,
                                              0.0, 1.0, 0.0, 0.0,
                                              0.0, 0.0, 1.0, 0.0,
                                              0.0, 0.0, 0.0, 1.0);

end Gl;
--------------------------------------------------------------------------------
