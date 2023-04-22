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
with Ada.Command_Line;
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
with Sdl;
with Sdl.Events;
with Sdl.Events.Events;
with Sdl.Events.Keyboards;
with Sdl.Events.Mice;
with Sdl.Video.Gl;
with Sdl.Video.Surfaces;
with Sdl.Video.Windows;
with Sdl.Video.Windows.Makers;
with Sdl.Timers;
with Utility.Log;
with Utility.Strings;
with Widgets.Keyboard;

--===========================================================================
-- GNAV under SDL
--===========================================================================
procedure Gnav_Sdl is

   use Gl;
   use Sdl.Timers;
   use Sdl.Events;
   use Sdl.Events.Keyboards;
   use Sdl.Events.Mice;
   use type Sdl.Sizes;
   use type Sdl.Video.Windows.Window_Flags;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- SDL window
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Main_Window  : aliased Sdl.Video.Windows.Window;
   Main_Context : Sdl.Video.Gl.Contexts;
   Main_Surface : Sdl.Video.Surfaces.Surface;
   Main_Size    : Sdl.Positive_Sizes := (400, 200);
   Last_Tick    : Sdl.Timers.Milliseconds;

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

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Other variables
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Event      : Sdl.Events.Events.Events;
   Size       : Sdl.Sizes               := Main_Window.Get_Size;
   Delta_Step : Sdl.Timers.Milliseconds := Sdl.Timers.Milliseconds (1000.0 * Timing.Time_Delta);
   Position   : Sdl.Natural_Coordinates := (  0,   0);

begin

   --########################################################################
   --# Initialization
   --########################################################################

   Utility.Log.Init;

   Utility.Log.Put_Message ("*** Starting G-NAV under SDL - V01A 29/03/2023 ***");

   if not Sdl.Initialise then

      Utility.Log.Put_Message ("error loading the SDL window");

      return;

   else
      Utility.Log.Put_Message ("SDL initialized");

      Last_Tick := Sdl.Timers.Ticks;

   end if;

   Utility.Log.Put_Message ("loading command line config");

   for I in 1..Ada.Command_Line.Argument_Count loop

      declare
         Argument : Utility.Strings.String_Buffer (100);
      begin

         Argument.Load (Ada.Command_Line.Argument (I));

         declare
            Key : String := Argument.Read_Next ('=');
            Val : String := Argument.Read_Next ('=');
         begin

            Utility.Log.Put_Message ("checking option " & key & " with " &Val);

            if Key = "ON_BOARD" then

               Main_Window.Maximise;

            elsif Key = "DISTORSION" then

               Utility.Log.Put_Message ("correcting distorsion by factor " & Val);

               Distorsion := Float'Value (Val);

            elsif Key = "RESOLUTION_X" then

               Utility.Log.Put_Message ("horizontal resolution " & Val);

               Main_Size.Width := Sdl.Coordinate'Value (Val);

            elsif Key = "RESOLUTION_Y" then

               Utility.Log.Put_Message ("vertical resolution " & Val);

               Main_Size.Height := Sdl.Coordinate'Value (Val);

            end if;

         end;

      end;

   end loop;

   -- Setup GL attributes
   --------------------------------------------------------------------------
   Utility.Log.Put_Message ("setting OpenGL attributes...");

   Sdl.Video.GL.Set_Red_Size   (5);
   Sdl.Video.GL.Set_Green_Size (5);
   Sdl.Video.GL.Set_Blue_Size  (5);
   Sdl.Video.GL.Set_Depth_Buffer_Size (16);
   Sdl.Video.GL.Set_Double_Buffer     (True);

   --------------------------------------------------------------------------
   --NOTE: in Raspberry Pi, the window name is used by openbox to apply the
   --      application decoration, so it must match the name on the
   --      configuration XML.
   --------------------------------------------------------------------------
   Utility.Log.Put_Message ("creating main window...");

   Sdl.Video.Windows.Makers.Create (Main_Window, "gnav",
                                    Position, Main_Size,
                                    Sdl.Video.Windows.Resizable or
                                    Sdl.Video.Windows.OpenGL    or
                                    Sdl.Video.Windows.Shown);

   -- Make context for main window
   --------------------------------------------------------------------------
   Main_Context.Create (Main_Window);

   -- Make surface for window
   --------------------------------------------------------------------------
   Main_Surface := Main_Window.Get_Surface;

   -- Initialization sequence
   --------------------------

   Utility.Log.Put_Message ("initializing OpenGL");

   Gl.Shaders.Init (Gl.Shaders.Glsl_330, 1);

   Gl.Fonts.Init;

   Widgets.Keyboard.Init;

   Flight.Stream.Init;

   Flight.Aircraft.Init;

   Flight.Plan.Init;

   Display.Menu.Init;

   Gl.Translate (Matrix, -1.0, -1.0, 0.0);

   Gl.Scale     (Matrix,  2.0,  2.0, 0.0);

   Display.Refresh := True;

   --########################################################################
   --# Main loop
   --########################################################################
   Main : loop

      if Display.Refresh then

         -- Setup OpenGL
         -----------------------------------------------------
         Size := Main_Window.Get_Size;
         if Size /= Main_Size then
            Main_Size := Size;
            Main_Surface := Main_Window.Get_Surface;
         end if;

         Display.Width  := Distorsion * Float (Size.Width);

         Display.Height := Float (Size.Height);

         Gl.Enable      (GL_BLEND);
         Gl.Blend_Func  (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
         Gl.Clear_Color (0.2, 0.2, 0.2, 1.0);
         Gl.Clear       (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
         Gl.Viewport    (0, 0, Size.Width, Size.Height);

         Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D, True);

         Gl.Shaders.Load_Matrix (Matrix);

         Gl.Shaders.Load_Aspect (0.5 * Gl_Float (Size.Width) * Distorsion,
                                 0.5 * Gl_Float (Size.Height));

         -- Draw display (the Menu has the root call)
         -----------------------------------------------------

         Flight.Representation.Flush_Path;

         Display.Menu.Draw;

         Main_Window.Update_Surface;

         Display.Refresh := False;

      end if;

      if Sdl.Timers.Ticks - Last_Tick >= Delta_Step then

         Last_Tick := Sdl.Timers.Ticks;

         -- Do cyclic stuff
         -----------------------------------------------------

         Timing.Events.Tick;

         Display.Refresh := True;

         Display.Blink := not Display.Blink;

      end if;

      -- Run events
      -----------------------------------------------------

      while Sdl.Events.Events.Poll (Event) loop

         case Event.Common.Event_Type is

            when Sdl.Events.Quit =>

               exit Main;

            when Sdl.Events.Keyboards.Key_Down =>

               if
                 Event.Keyboard.Key_Sym.Key_Code = Sdl.Events.Keyboards.Code_Plus or
                 Event.Keyboard.Key_Sym.Key_Code = Sdl.Events.Keyboards.Code_Kp_Plus
               then

                  Display.Menu.Key_Changed (Display.Panel_Wheel_Right);

               elsif
                 Event.Keyboard.Key_Sym.Key_Code = Sdl.Events.Keyboards.Code_Minus or
                 Event.Keyboard.Key_Sym.Key_Code = Sdl.Events.Keyboards.Code_Kp_Minus
               then

                  Display.Menu.Key_Changed (Display.Panel_Wheel_Left);

               elsif Event.Keyboard.Key_Sym.Key_Code = Sdl.Events.Keyboards.Code_Return then

                  Display.Menu.Key_Changed (Display.Panel_Wheel_Button);

               elsif Event.Keyboard.Key_Sym.Key_Code = Sdl.Events.Keyboards.Code_R then

                  Display.Menu.Key_Changed (Display.Panel_Button_Right);

               elsif Event.Keyboard.Key_Sym.Key_Code = Sdl.Events.Keyboards.Code_L then

                  Display.Menu.Key_Changed (Display.Panel_Button_Left);

               end if;

            when Sdl.Events.Mice.Button_Down =>
               declare

                  Mouse_X, Mouse_Y : Sdl.Natural_Coordinate;
                  X, Y             : Float;

               begin

                  if Event.Mouse_Button.Button = Sdl.Events.Mice.Left then

                     Mouse_X := Event.Mouse_Button.X;
                     Mouse_Y := Event.Mouse_Button.Y;

                     X :=       Float (Mouse_X) / Float (Main_Size.Width);
                     Y := 1.0 - Float (Mouse_Y) / Float (Main_Size.Height);

                     Display.Menu.Screen_Pressed (X, Y);

                  end if;

               end;

            when others =>
               null;
         end case;

      end loop;

      Sdl.Timers.Wait_Delay (10);

      exit when Display.Stop or Display.Reset;

   end loop Main;

   Flight.Stream.Finalize;

   Sdl.Finalise;

   if Display.Stop then

      Ada.Command_Line.Set_Exit_Status (9);

   elsif Display.Reset then

      Ada.Command_Line.Set_Exit_Status (8);

   end if;

   Utility.Log.Put_Message ("program closed");

end Gnav_Sdl;
-----------------------------------------------------------------------------

