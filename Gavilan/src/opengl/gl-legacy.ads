--------------------------------------------------------------------------------
-- LIBRARY_UNIT_NAME : Gl.Legacy
--
-- DESIGNER          : Guillermo HAZEBROUCK
--
-- CREATION_DATE     : 24 Mar 2020
--
-- LAST_MODIFICATION : 24 Mar 2020 Initial package creation
--------------------------------------------------------------------------------
with Interfaces.C;





--******************************************************************************
-- This is an Ada binding to the libGL.so library using the "compatibility mode"
-- functions.
-- This package is compatible with OpenGL versions 1.x and 2.x.
--******************************************************************************
package Gl.Legacy is

   GL_QUADS                  : constant Gl_Enum := 7;
   GL_QUAD_STRIP             : constant Gl_Enum := 8;
   GL_PROJECTION             : constant Gl_Enum := 16#1701#;
   GL_RENDER                 : constant Gl_Enum := 7168;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Points
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_POINT_SMOOTH           : constant Gl_Enum := 2832;
   GL_POINT_SIZE_GRANULARITY : constant Gl_Enum := 2835;
   GL_POINT_SIZE_RANGE       : constant Gl_Enum := 2834;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Lines
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_LINE_SMOOTH            : constant Gl_Enum := 2848;
   GL_LINE_WIDTH             : constant Gl_Enum := 2849;
   GL_LINE_WIDTH_GRANULARITY : constant Gl_Enum := 2851;
   GL_LINE_WIDTH_RANGE       : constant Gl_Enum := 2850;
   GL_LINE_STIPPLE           : constant Gl_Enum := 2852;
   GL_LINE_STIPPLE_PATTERN   : constant Gl_Enum := 2853;
   GL_LINE_STIPPLE_REPEAT    : constant Gl_Enum := 2854;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Display lists
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_COMPILE                : constant Gl_Enum := 4864;
   GL_COMPILE_AND_EXCECUTE   : constant Gl_Enum := 4865;

   --///////////////////////////////////////////////////////////////////////////
   -- Standard OpenGL calls
   --///////////////////////////////////////////////////////////////////////////

   --===========================================================================
   -- Lists
   --===========================================================================
   function  Gen_Lists    (Count   : Gl_Sizei) return Gl_Uint;
   procedure Delete_Lists (List_Id : Gl_Uint; Count : Gl_Sizei);
   procedure Call_List    (List_Id : Gl_Uint);
   procedure New_List     (List_Id : Gl_Uint; Mode : Gl_Enum);
   procedure End_List;

   --===========================================================================
   -- Transformations
   --===========================================================================
   procedure Load_Matrix (Matrix : Gl_Mat_4);

   --===========================================================================
   -- Attributes
   --===========================================================================
   procedure Push_Attrib (Mask : Gl_Bitfield);
   procedure Pop_Attrib;
   procedure Line_Stipple (Factor : Gl_Int; Pattern : Gl_Ushort);

   --===========================================================================
   -- Vertex data
   --===========================================================================
   procedure Gl_Begin (Mode : Gl_Enum);
   procedure Gl_End;
   procedure Vertex_2f    (X, Y : Gl_Float);
   procedure Vertex_2i    (X, Y : Gl_Int);
   procedure Color_3f     (R, G, B : Gl_Float);
   procedure Color_4f     (R, G, B, A : Gl_Float);
   procedure Tex_Coord_2i (U, V : Gl_Int);

   procedure Flush;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Global functions and procedures
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   pragma Import (Stdcall, Gen_Lists,                   "glGenLists");
   pragma Import (Stdcall, Delete_Lists,                "glDeleteLists");
   pragma Import (Stdcall, Call_List,                   "glCallList");
   pragma Import (Stdcall, New_List,                    "glNewList");
   pragma Import (Stdcall, End_List,                    "glEndList");

   pragma Import (Stdcall, Push_Attrib,                 "glPushAttrib");
   pragma Import (Stdcall, Pop_Attrib,                  "glPopAttrib");
   pragma Import (Stdcall, Line_Stipple,                "glLineStipple");

   pragma Import (Stdcall, Gl_Begin,                    "glBegin");
   pragma Import (Stdcall, Gl_End,                      "glEnd");

   pragma Import (Stdcall, Vertex_2f,                   "glVertex2f");
   pragma Import (Stdcall, Vertex_2i,                   "glVertex2i");
   pragma Import (Stdcall, Color_3f,                    "glColor3f");
   pragma Import (Stdcall, Color_4f,                    "glColor4f");
   pragma Import (Stdcall, Tex_Coord_2i,                "glTexCoord2i");

   pragma Import (Stdcall, Flush,                       "glFlush");

end Gl.Legacy;
--------------------------------------------------------------------------------
