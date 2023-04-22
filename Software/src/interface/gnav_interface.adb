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
with Ada.Real_Time;
use  Ada.Real_Time;
-- Gnav
with Display;
with Display.Menu;
with Timing;
with Timing.Events;
with Flight.Aircraft;
with Flight.Plan;
with Flight.Representation;
with Flight.Stream;
with Gl;
with Gl.Shaders;
with Gl.Fonts;
with Utility;
with Utility.Log;
with Utility.Strings;
with Widgets.Keyboard;

--==============================================================================
--
--==============================================================================
package body Gnav_Interface is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The transformation matrix that converts pixel to normalized coordinates.
   -- In this application, the origin is on the left button corner and the
   -- coordinates of the opposite corner are (1.0, 1.0)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Matrix : Gl.Gl_Mat_4 := Gl.Gl_Mat_4_Identity;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A factor that corrects a possible distorsion of the image when the
   -- window manager does not provide the actual size of the screen.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Distorsion : Float := 1.0;


   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Start_Data (Env : J_Environ_Access; Obj : J_Object_Access) is
   begin

      --########################################################################
      --# Initialization
      --########################################################################

      Utility.Log.Init;

      Utility.Log.Put_Message ("*** Starting Gavilan interface - V01A 29/03/2023 ***");

      Flight.Stream.Init;

      Flight.Aircraft.Init;

      Flight.Plan.Init;

      Display.Refresh := True;

   end Gnav_Start_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Initializes the rendering context
   --===========================================================================
   procedure Gnav_Start_Graphics (Env : J_Environ_Access; Obj : J_Object_Access) is

      use Gl;

   begin

      Display.Height := 200.0;

      Display.Width  := 350.0;

      Utility.Log.Put_Message ("initializing OpenGL");

      Gl.Shaders.Init (Gl.Shaders.Glsl_330, 1);

      Utility.Log.Put_Message ("initializing OpenGL fonts");

      Gl.Fonts.Init;

      Utility.Log.Put_Message ("initializing gnav");

      Widgets.Keyboard.Init;

      Display.Menu.Init;

      Gl.Translate (Matrix, -1.0, -1.0, 0.0);

      Gl.Scale     (Matrix,  2.0,  2.0, 0.0);

      Display.Refresh := True;

   end Gnav_Start_Graphics;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Size_Changed (Env : J_Environ_Access; Obj : J_Object_Access; W, H : J_Int) is
   begin

      Display.Width   := Distorsion * Float (W);

      Display.Height  := Float (H);

      Display.Refresh := True;

   end Gnav_Size_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Gnav_Screen_Pressed (Env : J_Environ_Access; Obj : J_Object_Access; X, Y : long) is
   begin

      Display.Menu.Screen_Pressed (Float (X) , Float (Y));

   end Gnav_Screen_Pressed;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Gnav_Refresh_Pending (Env : J_Environ_Access; Obj : J_Object_Access) return int is
   begin

      if Display.Refresh then

         return 1;

      else

         return 0;

      end if;

   end Gnav_Refresh_Pending;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Draw_Frame (Env : J_Environ_Access; Obj : J_Object_Access) is

      use Gl;

   begin

      if Display.Refresh then

         -- Setup OpenGL
         -----------------------------------------------------

         Gl.Enable      (GL_BLEND);
         Gl.Blend_Func  (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
         Gl.Clear_Color (0.2, 0.2, 0.2, 1.0);
         Gl.Clear       (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
         Gl.Viewport    (0, 0,
                         Gl_Sizei (Display.Width),
                         Gl_Sizei (Display.Height));

         Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D, True);

         Gl.Shaders.Load_Matrix (Matrix);

         Gl.Shaders.Load_Aspect (0.5 * Gl_Float (Display.Width) * Distorsion,
                                 0.5 * Gl_Float (Display.Height));

         -- Update the path buffer on the GPU
         -----------------------------------------------------

         Flight.Representation.Flush_Path;

         -- Draw display (the Menu has the root call)
         -----------------------------------------------------

         Display.Menu.Draw;

         Display.Refresh := False;

      end if;

   end Gnav_Draw_Frame;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Process_Timer (Env : J_Environ_Access; Obj : J_Object_Access) is
   begin

      Timing.Events.Tick;

      Display.Refresh := True;

      Display.Blink := not Display.Blink;

   end Gnav_Process_Timer;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Set_Position (Env : J_Environ_Access; Obj : J_Object_Access;
                                Lat, Lon, Alt : J_Double;
                                Spd, Brg      : J_Float) is
   begin

      Flight.Stream.Set_Data (Long_Float (Lat),
                              Long_Float (Lon),
                              Float (Alt),
                              Float (Spd),
                              Float (Brg));

   end Gnav_Set_Position;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Stop (Env : J_Environ_Access; Obj : J_Object_Access) is
   begin

      Flight.Stream.Finalize;

      Utility.Log.Put_Message ("program closed");

   end Gnav_Stop;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   function Gnav_Test_Interface (Env : J_Environ_Access; Obj : J_Object_Access; X, Y : J_Int) return J_Int is
   begin

      return X + Y;

   end Gnav_Test_Interface;
   -----------------------------------------------------------------------------

end Gnav_Interface;
--------------------------------------------------------------------------------
