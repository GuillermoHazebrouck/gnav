--//////////////////////////////////////////////////////////////////////////////
-- G-NAV PROJECT
-- Developed by Guillermo HAZEBROUCK - gahazebrouck@gmail.com
--\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- This file is part of "G-NAV".
-- The original code has been extracted from OpenGLAda, (c) 2017 Felix Krause
-- released under the terms of the MIT license.
-- Adaptation by Guillermo Hazebrouck.
--------------------------------------------------------------------------------

-- Depencencies
--//////////////////////////////////////////////////////////////////////////////

-- Standard
with System;
-- Local
with Glfw.Input.Mouse;
with Glfw.Input.Keys;
with Glfw.Monitors;
private
with Ada.Finalization;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Glfw.Windows is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Window is tagged private;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Window_Reference is not null access all Window;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Creation_Error : exception;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   package Callbacks is
      -- avoid pollution of Glfw.Windows package with symbols

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      --
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      type Kind is (Position, Size, Close, Refresh, Focus, Iconify,
                    Framebuffer_Size, Mouse_Button, Mouse_Position,
                    Mouse_Scroll, Mouse_Enter, Key, Char);
   end Callbacks;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Coordinate is Interfaces.C.int;

   --===========================================================================
   -- NOTE: throws Creation_Error if the window cannot be created
   --===========================================================================
   procedure Init (Object        : not null access Window;
                   Width, Height : Size;
                   Title         : String; -- interpreted as UTF-8
                   Monitor       : Monitors.Monitor := Monitors.No_Monitor;
                   Share_Resources_With : access Window'Class := null);

   --===========================================================================
   --
   --===========================================================================
   function Initialized (Object : not null access Window) return Boolean;

   --===========================================================================
   --
   --===========================================================================
   procedure Destroy (Object : not null access Window);

   --===========================================================================
   --
   --===========================================================================
   procedure Show (Object : not null access Window);

   --===========================================================================
   --
   --===========================================================================
   procedure Hide (Object : not null access Window);

   --===========================================================================
   --
   --===========================================================================
   procedure Set_Title (Object : not null access Window; Value : String);

   --===========================================================================
   --
   --===========================================================================
   procedure Get_OpenGL_Version (Object : not null access Window;
                                 Major, Minor, Revision : out Natural);

   --===========================================================================
   --
   --===========================================================================
   function Key_State (Object : not null access Window; Key : Input.Keys.Key)
                       return Input.Button_State;

   --===========================================================================
   --
   --===========================================================================
   function Mouse_Button_State (Object : not null access Window;
                                Button : Input.Mouse.Button)
                                return Input.Button_State;

   --===========================================================================
   --
   --===========================================================================
   procedure Set_Input_Toggle (Object : not null access Window;
                               Kind   : Input.Sticky_Toggle;
                               Value  : Boolean);

   --===========================================================================
   --
   --===========================================================================
   function Get_Cursor_Mode (Object : not null access Window)
                             return Input.Mouse.Cursor_Mode;

   --===========================================================================
   --
   --===========================================================================
   procedure Set_Cursor_Mode (Object : not null access Window;
                              Mode   : Input.Mouse.Cursor_Mode);

   --===========================================================================
   --
   --===========================================================================
   procedure Get_Cursor_Pos (Object : not null access Window;
                             X, Y   : out Input.Mouse.Coordinate);

   --===========================================================================
   --
   --===========================================================================
   procedure Set_Cursor_Pos (Object : not null access Window;
                             X, Y   : Input.Mouse.Coordinate);

   --===========================================================================
   --
   --===========================================================================
   procedure Get_Position (Object : not null access Window;
                           X, Y : out Coordinate);

   --===========================================================================
   --
   --===========================================================================
   procedure Set_Position (Object : not null access Window;
                           X, Y : Coordinate);

   --===========================================================================
   --
   --===========================================================================
   procedure Get_Size (Object : not null access Window;
                       Width, Height : out Size);

   --===========================================================================
   --
   --===========================================================================
   procedure Set_Size (Object : not null access Window;
                       Width, Height : Size);

   --===========================================================================
   --
   --===========================================================================
   procedure Set_Decorated (Object : not null access Window; Value : Boolean);

   --===========================================================================
   --
   --===========================================================================
   procedure Restore (Object : not null access Window);

   --===========================================================================
   --
   --===========================================================================
   procedure Maximize (Object : not null access Window);

   --===========================================================================
   --
   --===========================================================================
   procedure Get_Framebuffer_Size (Object : not null access Window;
                                   Width, Height : out Size);

   --===========================================================================
   --
   --===========================================================================
   function Visible (Object : not null access Window) return Boolean;

   --===========================================================================
   --
   --===========================================================================
   function Iconified (Object : not null access Window) return Boolean;

   --===========================================================================
   --
   --===========================================================================
   function Focused (Object : not null access Window) return Boolean;

   --===========================================================================
   --
   --===========================================================================
   function Should_Close (Object : not null access Window) return Boolean;

   --===========================================================================
   --
   --===========================================================================
   procedure Set_Should_Close (Object : not null access Window;
                               Value  : Boolean);

   -----------------------------------------------------------------------------
   -- Event API
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   procedure Enable_Callback (Object  : not null access Window;
                              Subject : Callbacks.Kind);

   --===========================================================================
   --
   --===========================================================================
   procedure Disable_Callback (Object  : not null access Window;
                               Subject : Callbacks.Kind);

   --===========================================================================
   --
   --===========================================================================
   type Mouse_Button_Changed_Callback is access procedure (Button  : Input.Mouse.Button;
                                                           State   : Input.Button_State;
                                                           Mods    : Input.Keys.Modifiers);

   procedure Set_Mouse_Button_Changed (Object : not null access Window;
                                       Event  : Mouse_Button_Changed_Callback);

   --===========================================================================
   --
   --===========================================================================
   type Position_Changed_Callback is access procedure  (X, Y   : Integer);

   procedure Set_Position_Changed (Object : not null access Window;
                                   Event  : Position_Changed_Callback);

   --===========================================================================
   --
   --===========================================================================
   type Size_Changed_Callback is access procedure (Width, Height : Natural);

   procedure Set_Size_Changed (Object : not null access Window;
                               Event  : Size_Changed_Callback);

   --===========================================================================
   --
   --===========================================================================
   type Close_Requested_Callback is access procedure;

   procedure Set_Close_Requested (Object : not null access Window;
                                  Event  : Close_Requested_Callback);

   --===========================================================================
   --
   --===========================================================================
   type Refresh_Callback is access procedure;

   procedure Set_Refresh_Callback (Object : not null access Window;
                                   Event  : Refresh_Callback);

   --===========================================================================
   --
   --===========================================================================
   type Focus_Changed_Callback is access procedure (Focused : Boolean);

   procedure Set_Focus_Changed (Object : not null access Window;
                                Event  : Focus_Changed_Callback);

   --===========================================================================
   --
   --===========================================================================
   type Iconification_Changed_Callback is access procedure  (Iconified : Boolean);

   procedure Set_Iconification_Changed (Object : not null access Window;
                                        Event  : Iconification_Changed_Callback);

   --===========================================================================
   --
   --===========================================================================
   type Framebuffer_Size_Changed_Callback is access procedure (Width, Height : Natural);

   procedure Set_Framebuffer_Size_Changed (Object : not null access Window;
                                           Event  : Framebuffer_Size_Changed_Callback);

   --===========================================================================
   --
   --===========================================================================
   type Mouse_Position_Changed_Callback is access procedure (X, Y   : Input.Mouse.Coordinate);

   procedure Set_Mouse_Position_Changed (Object : not null access Window;
                                         Event  : Mouse_Position_Changed_Callback);

   --===========================================================================
   --
   --===========================================================================
   type Mouse_Scrolled_Callback is access procedure (X, Y   : Input.Mouse.Scroll_Offset);

   procedure Set_Mouse_Scrolled (Object : not null access Window;
                                 Event  : Mouse_Scrolled_Callback);

   --===========================================================================
   --
   --===========================================================================
   type Mouse_Entered_Callback is access procedure (Action : Input.Mouse.Enter_Action);

   procedure Set_Mouse_Entered (Object : not null access Window;
                                         Event  : Mouse_Entered_Callback);

   --===========================================================================
   --
   --===========================================================================
   type Key_Changed_Callback is access procedure (Key      : Input.Keys.Key;
                                                  Scancode : Input.Keys.Scancode;
                                                  Action   : Input.Keys.Action;
                                                  Mods     : Input.Keys.Modifiers);

   procedure Set_Key_Changed (Object : not null access Window;
                              Event  : Key_Changed_Callback);

   --===========================================================================
   --
   --===========================================================================
   type Character_Entered_Callback is access procedure (Char   : Wide_Wide_Character);

   procedure Set_Character_Entered (Object : not null access Window;
                                    Event  : Character_Entered_Callback);

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Window is new Ada.Finalization.Controlled with record

      Handle : System.Address := System.Null_Address;

      -- Events
      -----------------------------------------------------------------

      Mouse_Button_Changed_Event     : Mouse_Button_Changed_Callback;

      Position_Changed_Event         : Position_Changed_Callback;

      Size_Changed_Event             : Size_Changed_Callback;

      Close_Requested_Event          : Close_Requested_Callback;

      Refresh_Event                  : Refresh_Callback;

      Focus_Changed_Event            : Focus_Changed_Callback;

      Iconification_Changed_Event    : Iconification_Changed_Callback;

      Framebuffer_Size_Changed_Event : Framebuffer_Size_Changed_Callback;

      Mouse_Position_Changed_Event   : Mouse_Position_Changed_Callback;

      Mouse_Scrolled_Event           : Mouse_Scrolled_Callback;

      Mouse_Entered_Event            : Mouse_Entered_Callback;

      Key_Changed_Event              : Key_Changed_Callback;

      Character_Entered_Event        : Character_Entered_Callback;

   end record;

   --===========================================================================
   --
   --===========================================================================
   function Window_Ptr (Raw : System.Address) return not null access Window'Class;

end Glfw.Windows;
--------------------------------------------------------------------------------
