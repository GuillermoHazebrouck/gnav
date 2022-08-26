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
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
-- Gnav
with Flight.Aircraft;
with Flight.Plan;
with Gl;
use  Gl;
with Gl.Fonts;
with Gl.Resources;
with Gl.Shaders;
with Math;
with Utility.Colors;
use  Utility.Colors;
with Utility.Strings;
with Utility.Units;
use  Utility.Units;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Wind is

   -- Speed adaptation
   ---------------------------
   
   Btn_Plus : Button_Record;
   
   Btn_Less : Button_Record;
   
   -- Needle vertex ids
   ---------------------------

   North_Id : Gl_Uint;

   Wind_Id  : Gl_Uint;

   -- Quadrant vertex data
   ---------------------------

   Quadrant_Id : Gl_Uint;

   Rosetta_Id  : Gl_Uint;

   Allocation : Allocation_Record;

   Quadrant_Size : constant Natural := 36;

   -- Fonts
   ---------------------------------
   Font_1 : Gl.Fonts.Font_Style_Record := (Width     => 0.020,
                                           Height    => 0.060,
                                           Space     => 0.008,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular,
                                           Line_R    => 0.0,
                                           Line_G    => 1.0,
                                           Line_B    => 1.0,
                                           Glow_R    => 0.1,
                                           Glow_G    => 0.1,
                                           Glow_B    => 0.8);
   
   -- Fonts
   ---------------------------------
   Font_2 : Gl.Fonts.Font_Style_Record := (Width     => 0.010,
                                           Height    => 0.040,
                                           Space     => 0.008,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular,
                                           Line_R    => 0.0,
                                           Line_G    => 1.0,
                                           Line_B    => 1.0,
                                           Glow_R    => 0.1,
                                           Glow_G    => 0.1,
                                           Glow_B    => 0.8);
   
   --===========================================================================
   --
   --===========================================================================
   procedure Init is

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;

      S : constant Float := 2.0 * Pi / Float (Quadrant_Size);

      Quadrant_Buffer : Gl.Gl_Float_Vec (1 .. 2 * Quadrant_Size);

      Rosetta_Buffer  : Gl.Gl_Float_Vec (1 .. 20);

      Angle           : Float := 0.0;

      North_Buffer    : Gl.Gl_Float_Vec (1 .. 6);

      Wind_Buffer     : Gl.Gl_Float_Vec (1 .. 8);

      A : Allocation_Record;

   begin

      -- Load quadrant buffer in GPU
      --------------------------------------------------------------------------

      for I in 1..Quadrant_Size loop

         Quadrant_Buffer (2 * I - 1) := 0.5 * Cos (Angle);
         Quadrant_Buffer (2 * I    ) := 0.5 * Sin (Angle);

         Angle := Angle + S;

      end loop;

      Gl.Resources.Update_Resource (Quadrant_Id, Quadrant_Buffer'Unrestricted_Access);

      -- Rosetta (NESW deshes)
      --------------------------------------------------------------------------

      Rosetta_Buffer (1)  := 0.55;
      Rosetta_Buffer (2)  := 0.00;
      Rosetta_Buffer (3)  := 0.40;
      Rosetta_Buffer (4)  := 0.00;

      Rosetta_Buffer (5)  :=-0.55;
      Rosetta_Buffer (6)  := 0.00;
      Rosetta_Buffer (7)  :=-0.40;
      Rosetta_Buffer (8)  := 0.00;

      Rosetta_Buffer (9)  := 0.00;
      Rosetta_Buffer (10) := 0.55;
      Rosetta_Buffer (11) := 0.00;
      Rosetta_Buffer (12) := 0.40;

      Rosetta_Buffer (13) := 0.00;
      Rosetta_Buffer (14) :=-0.55;
      Rosetta_Buffer (15) := 0.00;
      Rosetta_Buffer (16) :=-0.40;

      Rosetta_Buffer (17) := 0.00;
      Rosetta_Buffer (18) := 0.20;
      Rosetta_Buffer (19) := 0.00;
      Rosetta_Buffer (20) := 0.45;

      Gl.Resources.Update_Resource (Rosetta_Id, Rosetta_Buffer'Unrestricted_Access);

      -- Load north arrow buffer in GPU
      --------------------------------------------------------------------------

      North_Buffer (1) := 0.00;
      North_Buffer (2) := 0.60;

      North_Buffer (3) := 0.10;
      North_Buffer (4) := 0.45;

      North_Buffer (5) :=-0.10;
      North_Buffer (6) := 0.45;

      Gl.Resources.Update_Resource (North_Id, North_Buffer'Unrestricted_Access);

      -- Load wind arrow buffer in GPU
      --------------------------------------------------------------------------

      Wind_Buffer (1) :=-0.20;
      Wind_Buffer (2) := 0.00;

      Wind_Buffer (3) :=-0.50;
      Wind_Buffer (4) := 0.08;

      Wind_Buffer (5) :=-0.45;
      Wind_Buffer (6) := 0.00;

      Wind_Buffer (7) :=-0.50;
      Wind_Buffer (8) :=-0.08;

      Gl.Resources.Update_Resource (Wind_Id, Wind_Buffer'Unrestricted_Access);

      -- Buttons
      
      Btn_Plus.Set_Label ("+");

      Btn_Plus.Set_Background_Color (Color_Sky);

      Btn_Plus.Set_Border_Color (Color_Blue);

      Btn_Plus.Set_Label_Color (Color_White);

      Btn_Plus.Set_Font_Size (0.7, 0.5);

      A.X := 0.8;

      A.W := 0.1;

      A.Y := 0.6;

      A.H := 0.1;

      Btn_Plus.Set_Allocation (A);

      --

      Btn_Less.Set_Label ("-");

      Btn_Less.Set_Background_Color (Color_Sky);

      Btn_Less.Set_Border_Color (Color_Blue);

      Btn_Less.Set_Label_Color (Color_White);

      Btn_Less.Set_Font_Size (0.7, 0.5);

      A.Y := A.Y - 0.3;

      Btn_Less.Set_Allocation (A);
      
   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      use Ada.Numerics;
      use Gl.Fonts;
         
      X      : Float := 0.5;
      
      Y      : Float := 0.5;
      
      Size   : Float := 0.4;
      
      Aspect : Float := Width / Height;
      
      M1     : Gl.Gl_Mat_4 := Gl.Shaders.Get_Active_Matrix;

      M2     : Gl.Gl_Mat_4 := Gl.Gl_Mat_4_Identity;

      M3     : Gl.Gl_Mat_4 := Gl.Gl_Mat_4_Identity;

      Angle  : Float;

      Wind   : Float := Convert (Float (Flight.Data.Wind.Norm2), 
                                 Unit_Meter_Second,
                                 Unit_Kilometer_Hour);
      
   begin

      Translate (M2, X, Y, 0.0);

      Scale     (M2, Size, Size * Aspect, 1.0);

      Rotate    (M2, 0.0);

      M2 := Multiply  (M1, M2);

      Gl.Shaders.Load_Matrix (M2);

      Gl.Bind_Buffer (GL_ARRAY_BUFFER, Quadrant_Id);

      Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Monochrome_2D);

      Gl.Shaders.Load_Color (Color_Black.R,
                             Color_Black.G,
                             Color_Black.B,
                             0.5);

      Gl.Draw_Arrays (GL_TRIANGLE_FAN, 0, Gl_Sizei (Quadrant_Size));

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (Color_Black.R,
                             Color_Black.G,
                             Color_Black.B,
                             Color_Black.A);

      Gl.Shaders.Load_Width (3.0);

      Gl.Draw_Arrays (GL_LINE_LOOP, 0, Gl_Sizei (Quadrant_Size));

      Gl.Shaders.Bind_Shader (Gl.Shaders.Points_2D);

      Gl.Shaders.Load_Diameter (2.5);

      Gl.Draw_Arrays (GL_POINTS, 0, Gl_Sizei (Quadrant_Size));

      Gl.Shaders.Load_Color (Color_Gray_6.R,
                             Color_Gray_6.G,
                             Color_Gray_6.B,
                             Color_Gray_6.A);

      Gl.Shaders.Load_Diameter (1.5);

      Gl.Draw_Arrays (GL_POINTS, 0, Gl_Sizei (Quadrant_Size));

      -- N-E-S-W marks
      --------------------------------------------------------------------------

      Gl.Bind_Buffer (GL_ARRAY_BUFFER, Rosetta_Id);

      Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

      -- Reference mark

      Gl.Shaders.Bind_Shader (Gl.Shaders.Monochrome_2D);

      Gl.Shaders.Load_Color (Color_Black.R,
                             Color_Black.G,
                             Color_Black.B,
                             Color_Black.A);

      M3 := Gl_Mat_4_Identity;

      Translate (M3, X, Y, 0.0);

      Scale     (M3, Size, Size * Aspect, 1.0);

      M3 := Multiply  (M1, M3);

      Gl.Shaders.Load_Matrix (M3);

      Gl.Line_Width (3.0);

      Gl.Shaders.Load_Color (Font_1.Glow_R,
                             Font_1.Glow_G,
                             Font_1.Glow_B,
                             1.0);

      Gl.Draw_Arrays (GL_LINES, 8, 2);

      Gl.Line_Width (1.0);

      Gl.Shaders.Load_Color (Font_1.Line_R,
                             Font_1.Line_G,
                             Font_1.Line_B,
                             1.0);

      Gl.Draw_Arrays (GL_LINES, 8, 2);

      -- End reference mark

      Gl.Shaders.Load_Matrix (M2);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (Color_Black.R,
                             Color_Black.G,
                             Color_Black.B,
                             Color_Black.A);

      Gl.Shaders.Load_Width (3.0);

      Gl.Draw_Arrays (GL_LINES, 0, 8);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Points_2D);

      Gl.Shaders.Load_Diameter (3.0);

      Gl.Draw_Arrays (GL_POINTS, 0, 8);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (Color_White.R,
                             Color_White.G,
                             Color_White.B,
                             Color_White.A);

      Gl.Shaders.Load_Width (1.0);

      Gl.Draw_Arrays (GL_LINES, 0, 8);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Points_2D);

      Gl.Shaders.Load_Diameter (1.0);

      Gl.Draw_Arrays (GL_POINTS, 0, 8);

      -- North arrow
      --------------------------------------------------------------------------

      Gl.Bind_Buffer (GL_ARRAY_BUFFER, North_Id);

      Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Monochrome_2D);

      Gl.Shaders.Load_Color (1.0, 1.0, 1.0, 1.0);

      Gl.Draw_Arrays (GL_TRIANGLES, 0, 3);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (0.0, 0.0, 0.0, 1.0);

      Gl.Shaders.Load_Width (1.5);

      Gl.Draw_Arrays (GL_LINE_LOOP, 0, 3);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Points_2D);

      Gl.Shaders.Load_Diameter (1.5);

      Gl.Draw_Arrays (GL_POINTS, 0, 3);

      -- Wind direction indicator
      --------------------------------------------------------------------------

      Angle := Float (Flight.Data.Wind.Orientation * 180.0 / Pi);

      if Angle < 0.0 then

         Angle := 360.0 + Angle;

      end if;

      Angle := Angle * Pi / 180.0;

      M2 := Gl_Mat_4_Identity;

      Translate (M2, X, Y, 0.0);

      Scale     (M2, Size, Size * Aspect, 1.0);

      Rotate    (M2, Angle);

      M2 := Multiply  (M1, M2);

      Gl.Shaders.Load_Matrix (M2);

      Gl.Bind_Buffer (GL_ARRAY_BUFFER, Wind_Id);

      Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Monochrome_2D);

      Gl.Shaders.Load_Color (0.1, 0.1, 0.4, 1.0);

      Gl.Draw_Arrays (GL_TRIANGLE_FAN, 0, 4);

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Color (0.2, 0.2, 0.9, 1.0);

      Gl.Shaders.Load_Width (1.0);

      Gl.Draw_Arrays (GL_LINE_LOOP, 0, 4);
      
      Gl.Shaders.Load_Matrix (M1);

      -- Course indicator
      --------------------------------------------------------------------------

      Gl.Shaders.Load_Matrix (M1);
      
      Angle := 270.0 - 180.0 * Float (Flight.Data.Wind.Bearing / Math.Pi);

      if Angle < 0.0 then

         Angle := 360.0 + Angle;

      end if;

      Gl.Fonts.Draw (Utility.Strings.Float_Image (Angle, 0),
                     X,
                     Y,
                     Font_1,
                     Alignment_CC);

      -- Meassurement data (when it was taken and how)
      --------------------------------------------------------------------------
      
      declare
         Wind_Age         : Duration := Flight.Data.Age (Flight.Field_Wind);
         Wind_Age_Minutes : Natural  := Natural (Wind_Age / 60.0);
      begin
         
         X := 0.50;
         Y := 0.08;
         
         if Wind_Age_Minutes = 1 then
            
            Gl.Fonts.Draw ("LAST MEASURED" & Natural'Image (Wind_Age_Minutes) & " MINUTE AGO",
                           X,
                           Y,
                           Font_2,
                           Alignment_CC);
            
         elsif Wind_Age_Minutes > 1 then
            
            Gl.Fonts.Draw ("LAST MEASURED" & Natural'Image (Wind_Age_Minutes) & " MINUTES AGO",
                           X,
                           Y,
                           Font_2,
                           Alignment_CC);
            
         else
            
            Gl.Fonts.Draw ("MEASURED NOW",
                           X,
                           Y,
                           Font_2,
                           Alignment_CC);
            
         end if;

      end;
                        
      -- Allocation
      --------------------------------------------------------------------------

      Allocation.X := X - 0.5 * Size;
      Allocation.W := Size;
      Allocation.Y := Y - 0.5 * Size * Aspect;
      Allocation.H := Size * Aspect;

      -- Buttons
      --------------------------------------------------------------------------
      
      Btn_Plus.Draw;
      
      Btn_Less.Draw;
      
      Gl.Fonts.Draw (Utility.Strings.Float_Image (Wind, 0),
                     0.85,
                     0.50,
                     Font_1,
                     Alignment_CC);

   end Draw;
   -----------------------------------------------------------------------------

   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
      
      Step   : constant Long_Float := 5.0 / 3.6;
      Norm   : Long_Float := Flight.Data.Wind.Norm2;    
      Aspect : Long_Float := Long_Float (Width / Height);
      
   begin
      
      if Btn_Less.Contains (X, Y) then
         
         if Norm > Step then
            
            Flight.Data.Wind.Normalize;
      
            Flight.Data.Wind.Scale (Norm - Step);
         
         else
            
            Flight.Data.Wind.Set (0.0, 0.0);
            
         end if;
         
      elsif Btn_Plus.Contains (X,Y) then
         
         if Norm > 0.0 then
            
            Flight.Data.Wind.Normalize;
      
            Flight.Data.Wind.Scale (Norm + Step);
         
         else
            
            Flight.Data.Wind.Set (0.0,-Step);
            
         end if;
         
      else
                       
         -- Recompute wind
         ------------------------------------------------------
      
         Flight.Data.Wind.Set (Long_Float (0.5 - X) * Aspect, 
                               Long_Float (0.5 - Y));
      
         Flight.Data.Wind.Normalize;
      
         Flight.Data.Wind.Scale (Norm);
      
      end if;
      
      Refresh := True;
      
      Flight.Data.Origin (Flight.Field_Wind) := Flight.Origin_Internal;
      
      Flight.Data.Ages   (Flight.Field_Wind) := Ada.Calendar.Clock;
         
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Handles a key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin
      
      null;
      
   end Key_Changed;
   -----------------------------------------------------------------------------
          
     
     
end Display.Pages.Wind;
--------------------------------------------------------------------------------
