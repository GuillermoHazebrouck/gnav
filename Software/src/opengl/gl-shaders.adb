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
use  Interfaces.C;
-- Local
with Gl.Shaders_330;
use  Gl.Shaders_330;
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Gl.Shaders is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Active_Shader : Shader_Types := Monochrome_2D;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Description of the possible uniforms in a shader
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Uniforms_Record is record

      Matrix       : Gl_Mat_4;

      Color        : Gl_Float_Vec_4;

      Aspect       : Gl_Float_Vec_2;

      Width        : Gl_Float;

      Diameter     : Gl_Float;

      Stride       : Gl_Float;

      Stroke       : Gl_Float;

      Line_Fading  : Gl_Float;

      Point_Fading : Gl_Float;

   end record;

   No_Uniform_Record : Uniforms_Record := (Matrix       => Gl_Mat_4_Identity,
                                           Color        => (0.0, 0.0, 0.0, 0.0),
                                           Aspect       => (0.0, 0.0),
                                           Width        => 1.0,
                                           Diameter     => 1.0,
                                           Stride       => 1.0,
                                           Stroke       => 1.0,
                                           Line_Fading  => 1.0,
                                           Point_Fading => 1.0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The active uniforms
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Active_Uniforms : Uniforms_Record := No_Uniform_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The scale of one pixel in drawing coordinates (used for stroking)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Pixel_Scale : Float := 1.0;



   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Glsl_Version : Glsl_Versions := Glsl_330;




   --===========================================================================
   -- Initializes the context
   --===========================================================================
   function Glsl_Version_Name return string is
   begin

      case Glsl_Version is
         when Glsl_330 =>
            return "330";
      end case;

   end Glsl_Version_Name;
   -----------------------------------------------------------------------------

   --///////////////////////////////////////////////////////////////////////////
   -- Monocrome 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --===========================================================================
   --
   --===========================================================================
   function V_Shader_Monochrome_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return V_Shader_Monochrome_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end V_Shader_Monochrome_2D_Glsl;
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   function F_Shader_Monochrome_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

         return F_Shader_Monochrome_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end F_Shader_Monochrome_2D_Glsl;
   -----------------------------------------------------------------------------

   --///////////////////////////////////////////////////////////////////////////
   -- Colormap 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --===========================================================================
   --
   --===========================================================================
   function V_Shader_Colormap_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return V_Shader_Colormap_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end V_Shader_Colormap_2D_Glsl;
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   function F_Shader_Colormap_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return F_Shader_Colormap_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end F_Shader_Colormap_2D_Glsl;
   -----------------------------------------------------------------------------

   --///////////////////////////////////////////////////////////////////////////
   -- Texture 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --===========================================================================
   --
   --===========================================================================
   function V_Shader_Texture_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return V_Shader_Texture_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end V_Shader_Texture_2D_Glsl;
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   function F_Shader_Texture_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return F_Shader_Texture_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end F_Shader_Texture_2D_Glsl;
   -----------------------------------------------------------------------------

   --///////////////////////////////////////////////////////////////////////////
   -- Smooth line 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --===========================================================================
   --
   --===========================================================================
   function V_Shader_Lines_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return V_Shader_Lines_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end V_Shader_Lines_2D_Glsl;
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   function G_Shader_Lines_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return G_Shader_Lines_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end G_Shader_Lines_2D_Glsl;
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   function F_Shader_Lines_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return F_Shader_Lines_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end F_Shader_Lines_2D_Glsl;
   -----------------------------------------------------------------------------

   --///////////////////////////////////////////////////////////////////////////
   -- Smooth stroked line 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --===========================================================================
   --
   --===========================================================================
   function V_Shader_Stroked_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return V_Shader_Stroked_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end V_Shader_Stroked_2D_Glsl;
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   function G_Shader_Stroked_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return G_Shader_Stroked_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end G_Shader_Stroked_2D_Glsl;
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   function F_Shader_Stroked_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return F_Shader_Stroked_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end F_Shader_Stroked_2D_Glsl;
   -----------------------------------------------------------------------------

   --///////////////////////////////////////////////////////////////////////////
   -- Smooth point 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --===========================================================================
   --
   --===========================================================================
   function V_Shader_Points_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return V_Shader_Points_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end V_Shader_Points_2D_Glsl;
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   function G_Shader_Points_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return G_Shader_Points_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end G_Shader_Points_2D_Glsl;
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   function F_Shader_Points_2D_Glsl return string is
   begin

      case Glsl_Version is

         when Glsl_330 =>

            return F_Shader_Points_2D_Glsl_330;

         when others =>

            return "";

      end case;

   end F_Shader_Points_2D_Glsl;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Contains all the identifiers to the GPU shaders
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Shader_Program_Record is record
      --------------------------------------------------------------------------
      -- Program Id
      --------------------------------------------------------------------------
      Id : Gl_Uint;

      --------------------------------------------------------------------------
      -- Shaders Ids
      --------------------------------------------------------------------------
      Vertex_Id   : Gl_Uint;

      Fragment_Id : Gl_Uint;

      Geometry_Id : Gl_Uint;

      --------------------------------------------------------------------------
      -- Uniforms Ids
      --------------------------------------------------------------------------
      Color_Id        : Gl_Int;

      Matrix_Id       : Gl_Int;

      Aspect_Id       : Gl_Int;

      Width_Id        : Gl_Int;

      Line_Fading_Id  : Gl_Int;

      Diameter_Id     : Gl_Int;

      Point_Fading_Id : Gl_Int;

      Stride_Id       : Gl_Int;

      Stroke_Id       : Gl_Int;

   end record;

   No_Shader_Program_Record :constant Shader_Program_Record := (0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Shader_Program_Array is array (Shader_Types) of Shader_Program_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Shader_Program_Matrix is array (Resources.Context_Range) of Shader_Program_Array;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Contains all the shader info required for rendering on each context
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Shaders : Shader_Program_Matrix;




   --===========================================================================
   -- Compile a shader + show error
   --===========================================================================
   procedure Compile_Shader (Shader : Gl_Uint) is

      Shader_Type    : aliased Gl_Int;
      Compile_Status : aliased Gl_Int;

      Shader_Name : String (1..8) := (others => ' ');

   begin
      -----------------------------------------------------------------------------
      -- Get shader type just to have nicer logging
      -----------------------------------------------------------------------------
      glGetShaderiv (Shader, GL_SHADER_TYPE, Shader_Type'Access);

      case Gl_Enum (Shader_Type) is
         when GL_FRAGMENT_SHADER => Shader_Name := "fragment";
         when GL_GEOMETRY_SHADER => Shader_Name := "geometry";
         when GL_VERTEX_SHADER   => Shader_Name := "vertex  ";
         when others             => null;
      end case;

      -----------------------------------------------------------------------------
      -- Compile the shader and log errors
      -----------------------------------------------------------------------------
      Gl.Log_Trace ("compiling " & Shader_Name & " shader" & Gl_Uint'Image (Shader));

      Gl.Compile_Shader (Shader);

      Gl.Log_Error ("while compiling " & Shader_Name & " shader" & Gl_Uint'Image (Shader));

      glGetShaderiv (Shader, GL_COMPILE_STATUS, Compile_Status'Access);

      if Gl_Enum (Compile_Status) = GL_FALSE then
         Utility.Log.Put_Message ("error while compiling " & Shader_Name & " shader" & Gl_Uint'Image (Shader));
         Utility.Log.Put_Message (Get_Shader_Source (Shader));
         Utility.Log.Put_Message ("---> " & Get_Shader_Info_Log (Shader));
      end if;

   end Compile_Shader;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Load shader into the GPU
   --===========================================================================
   procedure Load_Shader (Context         : Resources.Context_Range;
                          Shader_Type     : Shader_Types;
                          Vertex_Shader   : String;
                          Fragment_Shader : String;
                          Geometry_Shader : String  := "";
                          Color           : Boolean := False;
                          Aspect          : Boolean := False;
                          Width           : Boolean := False;
                          Diameter        : Boolean := False;
                          Stride          : Boolean := False;
                          Stroke          : Boolean := False;
                          Line_Fading     : Boolean := False;
                          Point_Fading    : Boolean := False) is

      use Interfaces.C;

      Vertex_Code   : aliased Chars_Ptr_Array := (1 => Interfaces.C.Strings.New_String (Vertex_Shader));
      Fragment_Code : aliased Chars_Ptr_Array := (1 => Interfaces.C.Strings.New_String (Fragment_Shader));
      Geometry_Code : aliased Chars_Ptr_Array := (1 => Interfaces.C.Strings.New_String (Geometry_Shader));

      Program : Shader_Program_Record := No_Shader_Program_Record;

   begin
      Gl.Log_Trace ("loading shader program for " & Shader_Types'Image (Shader_Type));

      --------------------------------------------------------------------------
      -- Vertex shader
      --------------------------------------------------------------------------
      Program.Vertex_Id := Gl.Create_Shader (GL_VERTEX_SHADER);

      Gl.Shader_Source (Program.Vertex_Id, 1, Vertex_Code'Access, null);

      Gl.Log_Error ("while loading vertex shader");

      Compile_Shader (Program.Vertex_Id);

      --------------------------------------------------------------------------
      -- Geometry shader
      --------------------------------------------------------------------------
      if Geometry_Shader /= "" then

         Program.Geometry_Id := Gl.Create_Shader (GL_GEOMETRY_SHADER);

         Gl.Shader_Source (Program.Geometry_Id, 1, Geometry_Code'Access, null);

         Gl.Log_Error ("while loading geometry shader");

         Compile_Shader (Program.Geometry_Id);

      end if;

      --------------------------------------------------------------------------
      -- Fragment shader
      --------------------------------------------------------------------------
      Program.Fragment_Id := Gl.Create_Shader (GL_FRAGMENT_SHADER);

      Gl.Shader_Source (Program.Fragment_Id, 1, Fragment_Code'Access, null);

      Gl.Log_Error ("while loading fragment shader");

      Compile_Shader (Program.Fragment_Id);


      --------------------------------------------------------------------------
      -- Create program, attach shaders and link the program
      --------------------------------------------------------------------------
      Program.Id := Gl.Create_Program;

      Gl.Log_Error ("while creating shader program");

      Gl.Attach_Shader (Program.Id, Program.Fragment_Id);
      Gl.Log_Error ("while attaching fragment shader");

      Gl.Attach_Shader (Program.Id, Program.Vertex_Id);
      Gl.Log_Error ("while attaching vertex shader");

      if Program.Geometry_Id > 0 then
         Gl.Attach_Shader (Program.Id, Program.Geometry_Id);
         Gl.Log_Error ("while attaching geometry shader");
      end if;

      Gl.Log_Trace ("linking program ->" & Gl_Uint'Image (Program.Id));
      Gl.Link_Program  (Program.Id);
      Gl.Log_Error ("while linking shader program");

      --TODO: once the program is linked: glDetachShader -> glDeleteShader

      --------------------------------------------------------------------------
      -- Get matrix uniform
      --------------------------------------------------------------------------
      Program.Matrix_Id := Gl.Get_Uniform_Location (Program.Id, "Matrix");
      Gl.Log_Error ("while requesting transform location");

      --------------------------------------------------------------------------
      -- Get color uniform
      --------------------------------------------------------------------------
      if Color then
         Program.Color_Id := Gl.Get_Uniform_Location (Program.Id, "Color");
         Gl.Log_Error ("while requesting color location");
      end if;

      --------------------------------------------------------------------------
      -- Get aspect uniform
      --------------------------------------------------------------------------
      if Aspect then
         Program.Aspect_Id := Gl.Get_Uniform_Location (Program.Id, "Aspect");
         Gl.Log_Error ("while requesting aspect location");
      end if;

      --------------------------------------------------------------------------
      -- Get width uniform
      --------------------------------------------------------------------------
      if Width then
         Program.Width_Id := Gl.Get_Uniform_Location (Program.Id, "Width");
         Gl.Log_Error ("while requesting width location");
      end if;

      --------------------------------------------------------------------------
      -- Get width uniform
      --------------------------------------------------------------------------
      if Diameter then
         Program.Diameter_Id := Gl.Get_Uniform_Location (Program.Id, "Diameter");
         Gl.Log_Error ("while requesting diameter location");
      end if;

      --------------------------------------------------------------------------
      -- Get stride uniform
      --------------------------------------------------------------------------
      if Stride then
         Program.Stride_Id := Gl.Get_Uniform_Location (Program.Id, "Stride");
         Gl.Log_Error ("while requesting stride location");
      end if;

      --------------------------------------------------------------------------
      -- Get stroke uniform
      --------------------------------------------------------------------------
      if Stroke then
         Program.Stroke_Id := Gl.Get_Uniform_Location (Program.Id, "Stroke");
         Gl.Log_Error ("while requesting stroke location");
      end if;

      --------------------------------------------------------------------------
      -- Get stroke uniform
      --------------------------------------------------------------------------
      if Line_Fading then
         Program.Line_Fading_Id := Gl.Get_Uniform_Location (Program.Id, "LineFading");
         Gl.Log_Error ("while requesting line fading location");
      end if;

      --------------------------------------------------------------------------
      -- Get stroke uniform
      --------------------------------------------------------------------------
      if Point_Fading then
         Program.Point_Fading_Id := Gl.Get_Uniform_Location (Program.Id, "PointFading");
         Gl.Log_Error ("while requesting point fading location");
      end if;
      --------------------------------------------------------------------------
      -- Store the shader program
      --------------------------------------------------------------------------
      Shaders (Context) (Shader_Type) := Program;

      --------------------------------------------------------------------------
      -- Free strings
      --------------------------------------------------------------------------
      Interfaces.C.Strings.Free (Vertex_Code   (1));
      Interfaces.C.Strings.Free (Fragment_Code (1));
      Interfaces.C.Strings.Free (Geometry_Code (1));

   end Load_Shader;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Initializes the context
   --===========================================================================
   procedure Init (Version : Glsl_Versions; Context : Resources.Context_Range) is

      Vao : aliased Gl_Uint_Vec := (1 => 0);

   begin

      Glsl_Version := Version;

      --------------------------------------------------------------------------
      -- Trace OpenGL info
      --------------------------------------------------------------------------
      Gl.Log_Trace ("version  -> " & Gl.Get_String (GL_VERSION));

      Gl.Log_Trace ("vendor   -> " & Gl.Get_String (GL_VENDOR));

      Gl.Log_Trace ("rendered -> " & Gl.Get_String (GL_RENDERER));

      Gl.Log_Trace ("requesting GLSL version -> " & Glsl_Version_Name & " (max version : " & Gl.Get_String (GL_SHADING_LANGUAGE_VERSION) & ")");

      Gl.Log_Trace ("Gl_Float size:" & Integer'Image (Gl_Float'Size));
      Gl.Log_Trace ("Gl_Int   size:" & Integer'Image (Gl_Int'Size));
      Gl.Log_Trace ("Gl_Uint  size:" & Integer'Image (Gl_Uint'Size));

      --------------------------------------------------------------------------
      -- Trace context info
      --------------------------------------------------------------------------
      Gl.Log_Trace ("initializing context ->" & Resources.Context_Range'Image (Context));


      --------------------------------------------------------------------------
      -- Set active context
      --------------------------------------------------------------------------
      Resources.Active_Context := Context;

      --------------------------------------------------------------------------
      -- Vertex array object
      --------------------------------------------------------------------------
      Gl.Gen_Vertex_Arrays (1, Vao'Access);

      Gl.Bind_Vertex_Array (Vao (1));

      Gl.Log_Trace ("vertext array object VAO ->" & Gl_Uint'Image (Vao (1)));

      Gl.Log_Error ("while loading vertex array object");


      --------------------------------------------------------------------------
      -- Shaders for the Monochrome_2D format
      --------------------------------------------------------------------------
      Load_Shader (Context         => Context,
                   Shader_Type     => Monochrome_2D,
                   Vertex_Shader   => V_Shader_Monochrome_2D_Glsl,
                   Fragment_Shader => F_Shader_Monochrome_2D_Glsl,
                   Color           => True);

      Bind_Shader (Monochrome_2D);


      --------------------------------------------------------------------------
      -- Shaders for the Colormap_2D format
      --------------------------------------------------------------------------
      Load_Shader (Context         => Context,
                   Shader_Type     => Colormap_2D,
                   Vertex_Shader   => V_Shader_Colormap_2D_Glsl,
                   Fragment_Shader => F_Shader_Colormap_2D_Glsl);


      --------------------------------------------------------------------------
      -- Shaders for the Texture_2D format
      --------------------------------------------------------------------------
      Load_Shader (Context         => Context,
                   Shader_Type     => Texture_2D,
                   Vertex_Shader   => V_Shader_Texture_2D_Glsl,
                   Fragment_Shader => F_Shader_Texture_2D_Glsl);

      --------------------------------------------------------------------------
      -- Shaders for the Lines_2D format
      --------------------------------------------------------------------------
      Load_Shader (Context         => Context,
                   Shader_Type     => Lines_2D,
                   Vertex_Shader   => V_Shader_Lines_2D_Glsl,
                   Fragment_Shader => F_Shader_Lines_2D_Glsl,
                   Geometry_Shader => G_Shader_Lines_2D_Glsl,
                   Color           => True,
                   Aspect          => True,
                   Width           => True,
                   Line_Fading     => True);

      --------------------------------------------------------------------------
      -- Shaders for the Stroked_2D format
      --------------------------------------------------------------------------
      Load_Shader (Context         => Context,
                   Shader_Type     => Stroked_2D,
                   Vertex_Shader   => V_Shader_Stroked_2D_Glsl,
                   Fragment_Shader => F_Shader_Stroked_2D_Glsl,
                   Geometry_Shader => G_Shader_Stroked_2D_Glsl,
                   Color           => True,
                   Aspect          => True,
                   Width           => True,
                   Stride          => True,
                   Stroke          => True);

      --------------------------------------------------------------------------
      -- Shaders for the Points_2D format
      --------------------------------------------------------------------------
      Load_Shader (Context         => Context,
                   Shader_Type     => Points_2D,
                   Vertex_Shader   => V_Shader_Points_2D_Glsl,
                   Fragment_Shader => F_Shader_Points_2D_Glsl,
                   Geometry_Shader => G_Shader_Points_2D_Glsl,
                   Color           => True,
                   Aspect          => True,
                   Diameter        => True,
                   Point_Fading    => True);

   exception
      when E : others =>
         Utility.Log.Put_Message (E, "error while initializing open gl context");

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file).
   --===========================================================================
   procedure Bind_Shader (Shader : Shader_Types; Forced : Boolean := False) is

      use Interfaces.C;

      Program : Shader_Program_Record renames Shaders (Resources.Active_Context) (Shader);

   begin

      if Shader /= Active_Shader or else Forced then

         Gl.Log_Error ("before using shader program" & Gl_Uint'Image (Program.Id));

         Active_Shader := Shader;

         Gl.Use_Program (Program.Id);
         Gl.Log_Error ("while using shader program" & Gl_Uint'Image (Program.Id));

         -- Preload active matrix
         -----------------------------------------------------------------------
         if Program.Matrix_Id >= 0 then
            Gl.Uniform_Matrix_4fv (Program.Matrix_Id, 1, 0, Active_Uniforms.Matrix'Unrestricted_Access);
            Gl.Log_Error ("while reloading matrix" & Gl_Int'Image (Program.Matrix_Id) & " for program " & Gl_Uint'Image (Program.Id));
         end if;

         -- Preload active color
         -----------------------------------------------------------------------
         if Program.Color_Id >= 0 then
            Gl.Uniform_4f (Program.Color_Id,
                           Active_Uniforms.Color (0),
                           Active_Uniforms.Color (1),
                           Active_Uniforms.Color (2),
                           Active_Uniforms.Color (3));
            Gl.Log_Error ("while reloading color" & Gl_Int'Image (Program.Color_Id) & " for program " & Gl_Uint'Image (Program.Id));
         end if;

         -- Preload active aspect
         -----------------------------------------------------------------------
         if Program.Aspect_Id >= 0 then
            Gl.Uniform_2f (Program.Aspect_Id,
                           Active_Uniforms.Aspect (0),
                           Active_Uniforms.Aspect (1));
            Gl.Log_Error ("while reloading aspect" & Gl_Int'Image (Program.Aspect_Id) & " for program " & Gl_Uint'Image (Program.Id));
         end if;

         -- Preload active width
         -----------------------------------------------------------------------
         if Program.Width_Id >= 0 then
            Gl.Uniform_1f (Program.Width_Id,
                           Active_Uniforms.Width);
            Gl.Log_Error ("while reloading width" & Gl_Int'Image (Program.Width_Id) & " for program " & Gl_Uint'Image (Program.Id));
         end if;

         -- Preload active line fading
         -----------------------------------------------------------------------
         if Program.Line_Fading_Id >= 0 then
            Gl.Uniform_1f (Program.Line_Fading_Id,
                           Active_Uniforms.Line_Fading);
            Gl.Log_Error ("while reloading line fading" & Gl_Int'Image (Program.Line_Fading_Id) & " for program " & Gl_Uint'Image (Program.Id));
         end if;

         -- Preload active diameter
         -----------------------------------------------------------------------
         if Program.Diameter_Id >= 0 then
            Gl.Uniform_1f (Program.Diameter_Id,
                           Active_Uniforms.Diameter);
            Gl.Log_Error ("while reloading diameter" & Gl_Int'Image (Program.Width_Id) & " for program " & Gl_Uint'Image (Program.Id));
         end if;

         -- Preload active point fading
         -----------------------------------------------------------------------
         if Program.Point_Fading_Id >= 0 then
            Gl.Uniform_1f (Program.Point_Fading_Id,
                           Active_Uniforms.Point_Fading);
            Gl.Log_Error ("while reloading point fading" & Gl_Int'Image (Program.Point_Fading_Id) & " for program " & Gl_Uint'Image (Program.Id));
         end if;

         -- Preload active stride
         -----------------------------------------------------------------------
         if Program.Stride_Id >= 0 then
            Gl.Uniform_1f (Program.Stride_Id,
                           Pixel_Scale * Active_Uniforms.Stride);
            Gl.Log_Error ("while reloading stride" & Gl_Int'Image (Program.Stride_Id) & " for program " & Gl_Uint'Image (Program.Id));
         end if;

         -- Preload active stroke
         -----------------------------------------------------------------------
         if Program.Stroke_Id >= 0 then
            Gl.Uniform_1f (Program.Stroke_Id,
                           Pixel_Scale * Active_Uniforms.Stroke);
            Gl.Log_Error ("while reloading stroke" & Gl_Int'Image (Program.Stroke_Id) & " for program " & Gl_Uint'Image (Program.Id));
         end if;

         case Shader is
            when Monochrome_2D =>
               Gl.Enable_Vertex_Attrib_Array  (0);
               Gl.Disable_Vertex_Attrib_Array (1);
            when Colormap_2D =>
               Gl.Enable_Vertex_Attrib_Array  (0);
               Gl.Enable_Vertex_Attrib_Array  (1);
            when Texture_2D =>
               Gl.Enable_Vertex_Attrib_Array  (0);
               Gl.Enable_Vertex_Attrib_Array  (1);
            when Points_2D =>
               Gl.Enable_Vertex_Attrib_Array  (0);
               Gl.Disable_Vertex_Attrib_Array (1);
            when Lines_2D =>
               Gl.Enable_Vertex_Attrib_Array  (0);
               Gl.Disable_Vertex_Attrib_Array (1);
            when Stroked_2D =>
               Gl.Enable_Vertex_Attrib_Array  (0);
               Gl.Disable_Vertex_Attrib_Array (1);
         end case;

      end if;

   end Bind_Shader;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Matrix (Matrix : Gl_Mat_4) is

      use Interfaces.C;
      Program : Shader_Program_Record renames Shaders (Resources.Active_Context) (Active_Shader);

   begin

      if Program.Matrix_Id >= 0 then

         Gl.Use_Program (Program.Id);

         Gl.Uniform_Matrix_4fv (Program.Matrix_Id, 1, 0, Matrix'Unrestricted_Access);

      end if;

      Active_Uniforms.Matrix := Matrix;

   end Load_Matrix;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Color (R, G, B, A : Gl_Float) is

      use Interfaces.C;
      Program : Shader_Program_Record renames Shaders (Resources.Active_Context) (Active_Shader);

   begin

      if Program.Color_Id >= 0 then

         Gl.Use_Program (Program.Id);

         Gl.Uniform_4f (Program.Color_Id, R, G, B, A);

      end if;

      Active_Uniforms.Color := (R, G, B, A);

   end Load_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Color (C : Color_Record) is

      use Interfaces.C;
      Program : Shader_Program_Record renames Shaders (Resources.Active_Context) (Active_Shader);

   begin

      if Program.Color_Id >= 0 then

         Gl.Use_Program (Program.Id);

         Gl.Uniform_4f (Program.Color_Id, C.R, C.G, C.B, C.A);

      end if;

      Active_Uniforms.Color := (C.R, C.G, C.B, C.A);

   end Load_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Aspect (W, H : Gl_Float) is

      use Interfaces.C;
      Program : Shader_Program_Record renames Shaders (Resources.Active_Context) (Active_Shader);

   begin

      if Program.Aspect_Id >= 0 then

         Gl.Use_Program (Program.Id);

         Gl.Uniform_2f (Program.Aspect_Id, W, H);

      end if;

      Active_Uniforms.Aspect := (W, H);

   end Load_Aspect;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Width (W : Gl_Float; F : Gl_Float := 1.0) is

      use Interfaces.C;
      Program : Shader_Program_Record renames Shaders (Resources.Active_Context) (Active_Shader);

   begin

      if Program.Width_Id >= 0 then

         Gl.Use_Program (Program.Id);

         Gl.Uniform_1f (Program.Width_Id, W);

      end if;

      Active_Uniforms.Width := W;

      if Program.Line_Fading_Id >= 0 then

         Gl.Use_Program (Program.Id);

         Gl.Uniform_1f (Program.Line_Fading_Id, F);

      end if;

      Active_Uniforms.Line_Fading := F;

   end Load_Width;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Diameter (D : Gl_Float; F : Gl_Float := 1.0) is

      use Interfaces.C;
      Program : Shader_Program_Record renames Shaders (Resources.Active_Context) (Active_Shader);

   begin

      if Program.Diameter_Id >= 0 then

         Gl.Use_Program (Program.Id);

         Gl.Uniform_1f (Program.Diameter_Id, D);

      end if;

      Active_Uniforms.Diameter := D;

      if Program.Point_Fading_Id >= 0 then

         Gl.Use_Program (Program.Id);

         Gl.Uniform_1f (Program.Point_Fading_Id, F);

      end if;

      Active_Uniforms.Point_Fading := F;

   end Load_Diameter;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Stride (S : Gl_Float) is

      use Interfaces.C;
      Program : Shader_Program_Record renames Shaders (Resources.Active_Context) (Active_Shader);

   begin

      if Program.Stride_Id >= 0 then

         Gl.Use_Program (Program.Id);

         Gl.Uniform_1f (Program.Stride_Id, Pixel_Scale * S);

      end if;

      Active_Uniforms.Stride := S;

   end Load_Stride;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Stroke (S : Gl_Float) is

      use Interfaces.C;
      Program : Shader_Program_Record renames Shaders (Resources.Active_Context) (Active_Shader);

   begin

      if Program.Stroke_Id >= 0 then

         Gl.Use_Program (Program.Id);

         Gl.Uniform_1f (Program.Stroke_Id, Pixel_Scale * S);

      end if;

      Active_Uniforms.Stroke := S;

   end Load_Stroke;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Pixel_Scale (S : Gl_Float) is
   begin

      Pixel_Scale := S;

      Load_Stride (Active_Uniforms.Stride);

      Load_Stroke (Active_Uniforms.Stroke);

   end Set_Pixel_Scale;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Active_Matrix return Gl_Mat_4 is
   begin

      return Active_Uniforms.Matrix;

   end Get_Active_Matrix;
   -----------------------------------------------------------------------------




end Gl.Shaders;
--------------------------------------------------------------------------------
