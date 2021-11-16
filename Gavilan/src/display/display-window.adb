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
with Ada.Text_IO;
-- Gnav
with Display.Menu;
with Timing;
with Timing.Events;
with Flight.Aircraft;
with Flight.Plan;
with Flight.Simu;
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
with Utility.Strings;
with Widgets.Keyboard;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Window is

   Main_Window : aliased Glfw.Windows.Window;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the application is initialized and running or about to run
   -- the main loop.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Alive  : Boolean := False;

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
   -- Handles the cursor (or screen touch) events
   --===========================================================================
   procedure Mouse_Button_Changed (Button  : Glfw.Input.Mouse.Button;
                                   State   : Glfw.Input.Button_State;
                                   Mods    : Glfw.Input.Keys.Modifiers);

   --===========================================================================
   -- (See predeclarations)
   --===========================================================================
   procedure Key_Changed (Key      : Glfw.Input.Keys.Key;
                          Scancode : Glfw.Input.Keys.Scancode;
                          Action   : Glfw.Input.Keys.Action;
                          Mods     : Glfw.Input.Keys.Modifiers);

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Is_Alive return Boolean is
   begin

      return Alive;

   end Is_Alive;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Init is

      W : Glfw.Size := 600;
      H : Glfw.Size := 350;

   begin

      Glfw.Init;

      --------------------------------------------------------------------------
      --NOTE: in Raspberry Pi, the window name is used by openbox to apply the
      --      application decoration, so it must match the name on the
      --      configuration XML.
      --------------------------------------------------------------------------

      Glfw.Windows.Init (Main_Window'Access, W, H, "Gavilan");

      Ada.Text_IO.Put_Line ("loading command line config");

      for I in 1..Ada.Command_Line.Argument_Count loop

         declare
            Argument : Utility.Strings.String_Buffer (100);
         begin

            Argument.Load (Ada.Command_Line.Argument (I));

            declare
               Key : String := Argument.Read_Next ('=');
               Val : String := Argument.Read_Next ('=');
            begin

               Ada.Text_IO.Put_Line ("checking option " & key & " with " &Val);

               if Key = "ON_BOARD" then

                  -- NOTE: this requires GLFW 3.4
                  ------------------------------------------

                  --Main_Window.Set_Decorated (False);

                  --Main_Window.Set_Cursor_Mode (Glfw.Input.Mouse.Hidden);

                  --Main_Window.Maximize;

                  null;

               elsif Key = "DISTORSION" then

                  Ada.Text_IO.Put_Line ("correcting distorsion by factor " & Val);

                  Distorsion := Float'Value (Val);

               end if;

            end;

         end;

      end loop;

      Main_Window.Set_Position (0, 0);

      Main_Window.Set_Decorated (False);

      Ada.Text_IO.Put_Line ("GLFW version: " & Glfw.Version_String);

      Main_Window.Set_Mouse_Button_Changed (Mouse_Button_Changed'Access);

      Main_Window.Set_Key_Changed (Key_Changed'Access);

      Gl.Shaders.Init (Gl.Shaders.Glsl_330, 1);

      Gl.Fonts.Init;

      Flight.Aircraft.Init;

      Flight.Plan.Init;

      Flight.Simu.Init;

      Display.Menu.Init;

      Widgets.Keyboard.Init;

      Gl.Translate (Matrix, -1.0, -1.0, 0.0);

      Gl.Scale     (Matrix,  2.0,  2.0, 0.0);

      Alive   := True;

      Refresh := True;

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Enter_Main_Loop is

      use Gl;
      use type Glfw.Seconds;

      H, W : Glfw.Size;

   begin

      while not Main_Window.Should_Close loop

         if Refresh then

            -- Setup OpenGL
            -----------------------------------------------------

            Main_Window.Get_Size (W, H);

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
            -----------------------------------------------------

            Display.Menu.Draw (Distorsion * Float (W), Float (H));

            Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);

            Refresh := False;

         end if;

         if Glfw.Time >= Glfw.Seconds (Timing.Time_Delta) then

            Glfw.Set_Time (0.0);

            -- Do cyclic stuff
            -----------------------------------------------------

            Timing.Events.Tick;

            Refresh := True;

            Blink   := not Blink;

         end if;

         Glfw.Input.Poll_Events;

         delay 0.001;

         exit when Display.Stop;

      end loop;

      Glfw.Shutdown;

      Alive := False;

      if Display.Stop then

         Ada.Command_Line.Set_Exit_Status (9);

      end if;

   end Enter_Main_Loop;
   -----------------------------------------------------------------------------




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

      Ada.Text_IO.Put_Line (Glfw.Input.Keys.Key'Image (Key));

      case Key is

         when Keys.Numpad_Add =>

      		Display.Menu.Key_Changed (Panel_Wheel_Right);

         when Keys.Numpad_Substract =>

      		Display.Menu.Key_Changed (Panel_Wheel_Left);

         when Keys.Enter =>

      		Display.Menu.Key_Changed (Panel_Wheel_Button);

         when Keys.R =>

      		Display.Menu.Key_Changed (Panel_Button_Right);

         when Keys.L =>

      		Display.Menu.Key_Changed (Panel_Button_Left);

         when others =>

            null;

      end case;

      Last_Key      := Key;
      Last_Key_Time := Clock;

   end Key_Changed;
   -----------------------------------------------------------------------------

end Display.Window;
--------------------------------------------------------------------------------
