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
with Glfw;
with Glfw.Monitors;
with Glfw.Windows;
with Glfw.Windows.Context;
with Glfw.Input;
with Glfw.Input.Mouse;
with Glfw.Input.Keys;
with Gl;
with Gl.Shaders;
with Gl.Fonts;
with Glfw.Input.Mouse;
with Utility.Log;
with Utility.Strings;
with Widgets.Keyboard;



--==============================================================================
-- (See specification file)
--==============================================================================
procedure Gnav_Glfw is

   use Gl;
   use type Glfw.Seconds;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Main_Window : aliased Glfw.Windows.Window;

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
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   W : Glfw.Size := 800;
   H : Glfw.Size := 480;

   --===========================================================================
   -- (See predeclarations)
   --===========================================================================
   procedure Mouse_Button_Changed (Button  : Glfw.Input.Mouse.Button;
                                   State   : Glfw.Input.Button_State;
                                   Mods    : Glfw.Input.Keys.Modifiers) is

      use Glfw.Input;
      use Glfw.Input.Mouse;

      H, W             : Glfw.Size;
      Mouse_X, Mouse_Y : Glfw.Input.Mouse.Coordinate;
      X, Y             : Float;

   begin

      if
        Button = Left_Button and
        State  = Pressed
      then

         Main_Window.Get_Cursor_Pos (Mouse_X, Mouse_Y);

         Main_Window.Get_Size (W, H);

         X :=       Float (Mouse_X) / Float (W);
         Y := 1.0 - Float (Mouse_Y) / Float (H);

         Display.Menu.Screen_Pressed (X, Y);

      end if;

   end Mouse_Button_Changed;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Event overflow prevention
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   Last_Key      : Glfw.Input.Keys.Key := Glfw.Input.Keys.Key'First;

   Last_Key_Time : Time := Clock;

   --===========================================================================
   -- (See predeclarations)
   --===========================================================================
   procedure Key_Changed (Key      : Glfw.Input.Keys.Key;
                          Scancode : Glfw.Input.Keys.Scancode;
                          Action   : Glfw.Input.Keys.Action;
                          Mods     : Glfw.Input.Keys.Modifiers) is

      use Glfw.Input;
      use Glfw.Input.Keys;

      Pause   : constant Time_Span := Milliseconds (10);
      Elapsed : Time_Span := Clock - Last_Key_Time;

   begin

      -- Only press allowed
      --------------------------------------------------------------------------

      if Action /= Press then

         return;

      end if;

      -- Block duplicated events to prevent overflow
      --------------------------------------------------------------------------

      if Key = Last_Key and then Elapsed < Pause then

         return;

      end if;

      --------------------------------------------------------------------------

      Utility.Log.Put_Message ("key pressed: " & Glfw.Input.Keys.Key'Image (Key));

      case Key is

         when Keys.Numpad_Add =>

            Display.Menu.Key_Changed (Display.Panel_Wheel_Right);

         when Keys.Numpad_Substract =>

            Display.Menu.Key_Changed (Display.Panel_Wheel_Left);

         when Keys.Enter =>

            Display.Menu.Key_Changed (Display.Panel_Wheel_Button);

         when Keys.R =>

            Display.Menu.Key_Changed (Display.Panel_Button_Right);

         when Keys.L =>

            Display.Menu.Key_Changed (Display.Panel_Button_Left);

         when others =>

            null;

      end case;

      Last_Key      := Key;
      Last_Key_Time := Clock;

   end Key_Changed;
   -----------------------------------------------------------------------------

begin

   --###########################################################################
   --# Initialization
   --###########################################################################

   Utility.Log.Init;

   Utility.Log.Put_Message ("*** Starting Gavilan under GLFW - V01A 29/03/2023 ***");

   Glfw.Init;

   Utility.Log.Put_Message ("GLFW initialized");

   -----------------------------------------------------------------------------
   --NOTE: in Raspberry Pi, the window name is used by openbox to apply the
   --      application decoration, so it must match the name on the
   --      configuration XML.
   -----------------------------------------------------------------------------

   Glfw.Windows.Init (Main_Window'Access, W, H, "gnav");

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

               -- NOTE: this requires GLFW 3.4
               -----------------------------------------------------------------

               --Main_Window.Set_Decorated (False);

               --Main_Window.Set_Cursor_Mode (Glfw.Input.Mouse.Hidden);

               --Main_Window.Maximize;

               null;

            elsif Key = "DISTORSION" then

               Utility.Log.Put_Message ("correcting distorsion by factor " & Val);

               Distorsion := Float'Value (Val);

            elsif Key = "RESOLUTION_X" then

               Utility.Log.Put_Message ("horizontal resolution " & Val);

               W := Glfw.Size'Value (Val);

            elsif Key = "RESOLUTION_Y" then

               Utility.Log.Put_Message ("vertical resolution " & Val);

               H := Glfw.Size'Value (Val);

            end if;

         end;

      end;

   end loop;

   Main_Window.Set_Position (0, 0);

   Main_Window.Set_Decorated (False);

   Utility.Log.Put_Message ("GLFW version: " & Glfw.Version_String);

   Main_Window.Set_Mouse_Button_Changed (Mouse_Button_Changed'Unrestricted_Access);

   Main_Window.Set_Key_Changed (Key_Changed'Unrestricted_Access);

   -- Initialization sequence
   -----------------------------------------------------------------------------

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

   --###########################################################################
   --# Main loop
   --###########################################################################

   while not Main_Window.Should_Close loop

      if Display.Refresh then

         -- Setup OpenGL
         -----------------------------------------------------------------------

         Main_Window.Get_Size (W, H);

         Display.Width := Distorsion * Float (W);

         Display.Height := Float (H);

         Gl.Enable      (GL_BLEND);
         Gl.Blend_Func  (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
         Gl.Clear_Color (0.2, 0.2, 0.2, 1.0);
         Gl.Clear       (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
         Gl.Viewport    (0, 0, W, H);

         Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D, True);

         Gl.Shaders.Load_Matrix (Matrix);

         Gl.Shaders.Load_Aspect (0.5 * Gl_Float (W) * Distorsion,
                                 0.5 * Gl_Float (H));

         -- Draw display (the Menu has the root call)
         -----------------------------------------------------------------------

         Flight.Representation.Flush_Path;

         Display.Menu.Draw;

         Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);

         Display.Refresh := False;

      end if;

      if Glfw.Time >= Glfw.Seconds (Timing.Time_Delta) then

         Glfw.Set_Time (0.0);

         -- Do cyclic stuff
         -----------------------------------------------------------------------

         Timing.Events.Tick;

         Display.Refresh := True;

         Display.Blink := not Display.Blink;

      end if;

      Glfw.Input.Poll_Events;

      delay 0.001;

      exit when Display.Stop or Display.Reset;

   end loop;

   Flight.Stream.Finalize;

   Glfw.Shutdown;

   if Display.Stop then

      Ada.Command_Line.Set_Exit_Status (9);

   elsif Display.Reset then

      Ada.Command_Line.Set_Exit_Status (8);

   end if;

   Utility.Log.Put_Message ("program closed");

end Gnav_Glfw;
-----------------------------------------------------------------------------
