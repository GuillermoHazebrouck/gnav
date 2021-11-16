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

-- Gnav
with Gl.Resources;

--//////////////////////////////////////////////////////////////////////////////
-- This package provides useful shading functions on top of the OpenGL binding.
-- There are three different shaders:
--
-- > Monochrome_2D: used to draw any kind of primitives with a uniform color.
--
-- > Colormap_2D  : used to draw any kind of primitives with a per vertex color.
--
-- > Texture_2D   : used to plot textures.
--
-- > Points_2D    : used to plot smooth points using a geometry shader.
--                  NOTE: this shader only accepts GL_POINTS.
--
-- > Lines_2D     : used to plot smooth lines using a geometry shader
--                  NOTE: this shader only accepts GL_LINES.
--
-- > Stroked_2D   : used to plot smooth stroked lines using a geometry shader
--                  NOTE: this shader only accepts GL_LINES.
--                  NOTE: in order to make the stipple continuous, 3 floats must
--                  be set per segment vertex: X, Y and the cumulated distance.
--//////////////////////////////////////////////////////////////////////////////
package Gl.Shaders is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Glsl_Versions is (Glsl_330);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The different types of shading configurations
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Shader_Types is (Monochrome_2D,
                         Colormap_2D,
                         Texture_2D,
                         Points_2D,
                         Lines_2D,
                         Stroked_2D);

   --===========================================================================
   -- Initializes the context by loading the shader programs and
   -- vertex array objects.
   --===========================================================================
   procedure Init (Version : Glsl_Versions; Context : Resources.Context_Range);

   --===========================================================================
   -- This procedure sets necessary shader programs for the given shader type.
   -- NOTE: always force a shader after changing the context.
   -- NOTE: core profile only.
   --===========================================================================
   procedure Bind_Shader (Shader : Shader_Types; Forced : Boolean := False);

   --===========================================================================
   -- Sets the uniform "Matrix"
   --===========================================================================
   procedure Load_Matrix (Matrix : Gl_Mat_4);

   --===========================================================================
   -- Sets the uniform "Color" of the active context
   --===========================================================================
   procedure Load_Color (R, G, B, A : Gl_Float);

   --===========================================================================
   -- Sets the uniform "Aspect" of the active context
   --===========================================================================
   procedure Load_Aspect (W, H : Gl_Float);

   --===========================================================================
   -- Sets the uniform "Width" of the active context
   -- NOTE: F is the fading exponent, used to make the lines look sharper.
   --===========================================================================
   procedure Load_Width (W : Gl_Float; F : Gl_Float := 1.0);

   --===========================================================================
   -- Sets the uniform "Diameter" of the active context
   -- NOTE: F is the fading exponent, used to make the dots look sharper.
   --===========================================================================
   procedure Load_Diameter (D : Gl_Float; F : Gl_Float := 1.0);

   --===========================================================================
   -- Sets the uniform "Stride" of the active context
   --===========================================================================
   procedure Load_Stride (S : Gl_Float);

   --===========================================================================
   -- Sets the uniform "Stroke" of the active context
   --===========================================================================
   procedure Load_Stroke (S : Gl_Float);

   --===========================================================================
   -- Sets the pixel scale (the ratio between 1 pixel and 1 drawing unit).
   -- NOTE: this will be used to scale the stride and stroke.
   --===========================================================================
   procedure Set_Pixel_Scale (S : Gl_Float);

   --===========================================================================
   -- Returns the transformation matrix for the active context and shader.
   --===========================================================================
   function Get_Active_Matrix return Gl_Mat_4;


end Gl.Shaders;
--------------------------------------------------------------------------------
