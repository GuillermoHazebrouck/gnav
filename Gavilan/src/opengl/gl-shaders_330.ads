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

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Gl.Shaders_330 is

   --///////////////////////////////////////////////////////////////////////////
   -- Monocrome 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Vertex shader GLSL function for monochrome
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   V_Shader_Monochrome_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform mat4 Matrix;"                                                    &
     "layout (location = 0) in vec2 Vertex;"                                   & -- 1st attribute

     "void main () {"                                                          &
     "  gl_Position = Matrix * vec4(Vertex, 0.0f, 1.0f);"                      &
     "}";

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Fragment shader GLSL function for monochrome
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   F_Shader_Monochrome_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform vec4 Color;"                                                     &
     "out vec4 FragColor;"                                                     &

     "void main () {"                                                          &
     "  FragColor = Color;"                                                    &
     "}";
 
   --///////////////////////////////////////////////////////////////////////////
   -- Colormap 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Vertex shader GLSL function for colormaps
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   V_Shader_Colormap_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform mat4 Matrix;"                                                    &
     "layout (location = 0) in vec2 Vertex;"                                   & -- 1st attribute
     "layout (location = 1) in vec3 Color;"                                    & -- 2nd attribute
     "out vec4 VertexColor;"                                                   &

     "void main () {"                                                          &
     "  VertexColor = vec4 (Color, 1.0);"                                      &
     "  gl_Position = Matrix * vec4(Vertex, 0.0f, 1.0f);"                      &
     "}";

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Fragment shader GLSL function for colormaps
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   F_Shader_Colormap_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "in  vec4 VertexColor;"                                                   &
     "out vec4 FragColor;"                                                     &

     "void main () {"                                                          &
     "  FragColor = VertexColor;"                                              &
     "}";
   
   --///////////////////////////////////////////////////////////////////////////
   -- Texture 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Vertex shader GLSL function for textures
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   V_Shader_Texture_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform mat4 Matrix;"                                                    &
     "layout (location = 0) in vec2 Position;"                                 & -- 1st attribute
     "layout (location = 1) in vec2 Vertex;"                                   & -- 2nd attribute
     "out vec2 TexturePoint;"                                                  &

     "void main () {"                                                          &
     "  TexturePoint = Vertex;"                                                &
     "  gl_Position  = Matrix * vec4(Position, 0.0f, 1.0f);"                   &
     "}";

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Fragment shader GLSL function for textures
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   F_Shader_Texture_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform sampler2D TextureSampler;"                                       &
     "in  vec2 TexturePoint;"                                                  &
     "out vec4 FragColor;"                                                     &

     "void main () {"                                                          &
     "  FragColor = texture(TextureSampler, TexturePoint);"                    &
     "}";

   --///////////////////////////////////////////////////////////////////////////
   -- Smooth line 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Vertex shader GLSL function for smooth lines using two triangles
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   V_Shader_Lines_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform mat4 Matrix;"                                                    &
     "layout (location = 0) in vec2 Vertex;"                                   & -- 1st attribute

     "void main () {"                                                          &
     "  gl_Position = Matrix * vec4(Vertex, 0.0f, 1.0f);"                      &
     "}";

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Geometry shader GLSL function for smooth lines using two triangles
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   G_Shader_Lines_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform mat4  Matrix;"                                                   &
     "uniform vec2  Aspect;"                                                   &
     "uniform float Width = 1.0;"                                              &
     "layout (lines) in;"                                                      &
     "layout (triangle_strip, max_vertices = 4) out;"                          &
     "out vec2 Gradient;"                                                      &

     "void main () {"                                                          &
     "  vec2 P0 = vec2 (gl_in[0].gl_Position);"                                &
     "  vec2 P1 = vec2 (gl_in[1].gl_Position);"                                &
     "  vec2 T  = normalize (Aspect * (P1 - P0));"                             &
     "  vec2 N  = vec2(-T.y, T.x);"                                            &
     "  vec2 Q  = Width * N / Aspect;"                                         &

     "  Gradient = N;"                                                         &
     "  gl_Position.x = P0.x + Q.x;"                                           &
     "  gl_Position.y = P0.y + Q.y;"                                           &
     "  gl_Position.z = 0.0;"                                                  &
     "  gl_Position.w = 1.0;"                                                  &
     "  EmitVertex();"                                                         &

     "  Gradient =-N;"                                                         &
     "  gl_Position.x = P0.x - Q.x;"                                           &
     "  gl_Position.y = P0.y - Q.y;"                                           &
     "  gl_Position.z = 0.0;"                                                  &
     "  gl_Position.w = 1.0;"                                                  &
     "  EmitVertex();"                                                         &

     "  Gradient = N;"                                                         &
     "  gl_Position.x = P1.x + Q.x;"                                           &
     "  gl_Position.y = P1.y + Q.y;"                                           &
     "  gl_Position.z = 0.0;"                                                  &
     "  gl_Position.w = 1.0;"                                                  &
     "  EmitVertex();"                                                         &

     "  Gradient =-N;"                                                         &
     "  gl_Position.x = P1.x - Q.x;"                                           &
     "  gl_Position.y = P1.y - Q.y;"                                           &
     "  gl_Position.z = 0.0;"                                                  &
     "  gl_Position.w = 1.0;"                                                  &
     "  EmitVertex();"                                                         &

     "  EndPrimitive();"                                                       &
     "}";

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Fragment shader GLSL function for smooth lines using two triangles
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   F_Shader_Lines_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform vec4  Color;"                                                    &
     "uniform float LineFading;"                                               &
     "in  vec2 Gradient;"                                                      &
     "out vec4 FragColor;"                                                     &

     "void main () {"                                                          &
     "  float Grade = dot (Gradient, Gradient);"                               &
     "  if (LineFading != 1.0) {Grade = pow (Grade, LineFading);}"             &
     "  FragColor = vec4(Color.x, Color.y, Color.z, 1 - Grade);"               &
     "}";

   --///////////////////////////////////////////////////////////////////////////
   -- Smooth stroked line 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Vertex shader GLSL function for smooth stroked lines using two triangles
   -- NOTE: the distance is passed as the Z coordinate of the first attribute
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   V_Shader_Stroked_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform mat4 Matrix;"                                                    &
     "layout (location = 0) in vec3 Vertex;"                                   & -- 1st attribute

     "void main () {"                                                          &
     "  gl_Position = Matrix * vec4(Vertex.x, Vertex.y, Vertex.z, 1.0f);"      &
     "}";

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Geometry shader GLSL function for smooth stroked lines using two triangles
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   G_Shader_Stroked_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform mat4  Matrix;"                                                   &
     "uniform vec2  Aspect;"                                                   &
     "uniform float Width  = 1.0;"                                             &
     "layout (lines) in;"                                                      &
     "layout (triangle_strip, max_vertices = 4) out;"                          &
     "out vec2  Gradient;"                                                     &
     "out float Field;"                                                        &

     "void main () {"                                                          &
     "  vec2 P0 = vec2 (gl_in[0].gl_Position);"                                &
     "  vec2 P1 = vec2 (gl_in[1].gl_Position);"                                &
     "  vec2 T  = normalize (Aspect * (P1 - P0));"                             &
     "  vec2 N  = vec2(-T.y, T.x);"                                            &
     "  vec2 Q  = Width * N / Aspect;"                                         &

   -- NOTE: the S coordinate (Field) comes in the Z register

     "  Gradient = N;"                                                         &
     "  Field    = gl_in[0].gl_Position.z;"                                    &
     "  gl_Position.x = P0.x + Q.x;"                                           &
     "  gl_Position.y = P0.y + Q.y;"                                           &
     "  gl_Position.z = 0.0;"                                                  &
     "  gl_Position.w = 1.0;"                                                  &
     "  EmitVertex();"                                                         &

     "  Gradient =-N;"                                                         &
     "  Field    = gl_in[0].gl_Position.z;"                                    &
     "  gl_Position.x = P0.x - Q.x;"                                           &
     "  gl_Position.y = P0.y - Q.y;"                                           &
     "  gl_Position.z = 0.0;"                                                  &
     "  gl_Position.w = 1.0;"                                                  &
     "  EmitVertex();"                                                         &

     "  Gradient = N;"                                                         &
     "  Field    = gl_in[1].gl_Position.z;"                                    &
     "  gl_Position.x = P1.x + Q.x;"                                           &
     "  gl_Position.y = P1.y + Q.y;"                                           &
     "  gl_Position.z = 0.0;"                                                  &
     "  gl_Position.w = 1.0;"                                                  &
     "  EmitVertex();"                                                         &

     "  Gradient =-N;"                                                         &
     "  Field    = gl_in[1].gl_Position.z;"                                    &
     "  gl_Position.x = P1.x - Q.x;"                                           &
     "  gl_Position.y = P1.y - Q.y;"                                           &
     "  gl_Position.z = 0.0;"                                                  &
     "  gl_Position.w = 1.0;"                                                  &
     "  EmitVertex();"                                                         &

     "  EndPrimitive();"                                                       &
     "}";

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Fragment shader GLSL function for smooth stroked lines using two triangles
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   F_Shader_Stroked_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform vec4  Color;"                                                    &
     "uniform float Stride = 10.0;"                                            &
     "uniform float Stroke =  5.0;"                                            &
     "in  vec2  Gradient;"                                                     &
     "in  float Field;"                                                        &
     "out vec4  FragColor;"                                                    &

     "void main () {"                                                          &
     "  float Alpha = 0;"                                                      &
     "  if (mod (Field, Stride) > Stroke) discard;"                            &
     "  Alpha = 1 - dot (Gradient, Gradient);"                                 &
     "  FragColor = vec4(Color.x, Color.y, Color.z, Alpha);"                   &
     "}";

   --///////////////////////////////////////////////////////////////////////////
   -- Smooth point 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Vertex shader GLSL function for smooth points using two triangles
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   V_Shader_Points_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform mat4 Matrix;"                                                    &
     "layout (location = 0) in vec2 Vertex;"                                   & -- 1st attribute

     "void main () {"                                                          &
     "  gl_Position = Matrix * vec4(Vertex, 0.0f, 1.0f);"                      &
     "}";

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Geometry shader GLSL function for smooth points using two triangles
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   G_Shader_Points_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform mat4  Matrix;"                                                   &
     "uniform vec2  Aspect;"                                                   &
     "uniform float Diameter;"                                                 &
     "layout (points) in;"                                                     &
     "layout (triangle_strip, max_vertices = 4) out;"                          &
     "out vec2 Gradient;"                                                      &

     "void main () {"                                                          &
     "  vec2 P = vec2 (gl_in[0].gl_Position);"                                 &
     "  vec2 Q = Diameter * vec2(1.0, 1.0) / Aspect;"                          &

     "  Gradient = vec2(1.0, 1.0);"                                            &
     "  gl_Position.x = P.x + Q.x;"                                            &
     "  gl_Position.y = P.y + Q.y;"                                            &
     "  gl_Position.z = 0.0;"                                                  &
     "  gl_Position.w = 1.0;"                                                  &
     "  EmitVertex();"                                                         &

     "  Gradient = vec2(1.0,-1.0);"                                            &
     "  gl_Position.x = P.x + Q.x;"                                            &
     "  gl_Position.y = P.y - Q.y;"                                            &
     "  gl_Position.z = 0.0;"                                                  &
     "  gl_Position.w = 1.0;"                                                  &
     "  EmitVertex();"                                                         &

     "  Gradient = vec2(-1.0, 1.0);"                                           &
     "  gl_Position.x = P.x - Q.x;"                                            &
     "  gl_Position.y = P.y + Q.y;"                                            &
     "  gl_Position.z = 0.0;"                                                  &
     "  gl_Position.w = 1.0;"                                                  &
     "  EmitVertex();"                                                         &

     "  Gradient = vec2(-1.0,-1.0);"                                           &
     "  gl_Position.x = P.x - Q.x;"                                            &
     "  gl_Position.y = P.y - Q.y;"                                            &
     "  gl_Position.z = 0.0;"                                                  &
     "  gl_Position.w = 1.0;"                                                  &
     "  EmitVertex();"                                                         &

     "  EndPrimitive();"                                                       &
     "}";

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Fragment shader GLSL function for smooth points using two triangles
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   F_Shader_Points_2D_Glsl_330 : constant String :=

     "#version 330" & Ascii.Lf                                                 &
     "uniform vec4  Color;"                                                    &
     "uniform float PointFading;"                                              &
     "in  vec2 Gradient;"                                                      &
     "out vec4 FragColor;"                                                     &

     "void main () {"                                                          &
     "  float Grade = dot(Gradient, Gradient);"                                &
     "  if (PointFading != 1.0) {Grade = pow (Grade, PointFading);}"           &
     "  float Alpha = 1 - min (1.0, Grade * Grade);"                           &
     "  FragColor = vec4(Color.x, Color.y, Color.z, Alpha);"                   &
     "}";

end Gl.Shaders_330;
--------------------------------------------------------------------------------
