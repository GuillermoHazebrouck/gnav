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
with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;
-- Local
with Glfw.Api;
with Glfw.Enums;
with Glfw.Windows.Context;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Glfw.Windows is

   --===========================================================================
   --
   --===========================================================================
   function To_Glfw_Boolean (Value : Boolean) return Glfw.Enums.Glfw_Boolean is
   begin

      if Value then
         return Glfw.Enums.Glfw_True;
      else
         return Glfw.Enums.Glfw_False;
      end if;

   end To_Glfw_Boolean;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   package Conversion is new System.Address_To_Access_Conversions (Window'Class);




   --===========================================================================
   --
   --===========================================================================
   function Window_Ptr (Raw : System.Address)
                        return not null access Window'Class is
   begin

      return Conversion.To_Pointer (Api.Get_Window_User_Pointer (Raw));

   end Window_Ptr;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Position_Callback (Raw  : System.Address;
                                    X, Y : Interfaces.C.int);
   pragma Convention (C, Raw_Position_Callback);

   procedure Raw_Position_Callback (Raw  : System.Address;
                                    X, Y : Interfaces.C.int) is
   begin

      if Window_Ptr (Raw).Position_Changed_Event /= null then

         Window_Ptr (Raw).Position_Changed_Event (Integer (X), Integer (Y));

      end if;

   end Raw_Position_Callback;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   procedure Set_Position_Changed (Object : not null access Window;
                                   Event  : Position_Changed_Callback) is
   begin

      Object.Position_Changed_Event := Event;

   end Set_Position_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Size_Callback (Raw  : System.Address;
                                Width, Height : Interfaces.C.int);
   pragma Convention (C, Raw_Size_Callback);

   procedure Raw_Size_Callback (Raw  : System.Address;
                                Width, Height : Interfaces.C.int) is
   begin

      if Window_Ptr (Raw).Size_Changed_Event /= null then

         Window_Ptr (Raw).Size_Changed_Event (Natural (Width), Natural (Height));

      end if;

   end Raw_Size_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Size_Changed (Object : not null access Window;
                               Event  : Size_Changed_Callback) is
   begin

      Object.Size_Changed_Event := Event;

   end Set_Size_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Close_Callback (Raw  : System.Address);
   pragma Convention (C, Raw_Close_Callback);

   procedure Raw_Close_Callback (Raw  : System.Address) is
   begin

      if Window_Ptr (Raw).Close_Requested_Event /= null then

         Window_Ptr (Raw).Close_Requested_Event.all;

      end if;

   end Raw_Close_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Close_Requested (Object : not null access Window;
                                  Event  : Close_Requested_Callback) is
   begin


      Object.Close_Requested_Event := Event;

   end Set_Close_Requested;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Refresh_Callback (Raw  : System.Address);
   pragma Convention (C, Raw_Refresh_Callback);

   procedure Raw_Refresh_Callback (Raw  : System.Address) is
   begin

      if Window_Ptr (Raw).Refresh_Event /= null then

         Window_Ptr (Raw).Refresh_Event.all;

      end if;

   end Raw_Refresh_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Refresh_Callback (Object : not null access Window;
                                   Event  : Refresh_Callback) is
   begin

      Object.Refresh_Event := Event;

   end Set_Refresh_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Focus_Changed_Callback (Raw : System.Address; Focused : Bool);
   pragma Convention (C, Raw_Focus_Changed_Callback);

   procedure Raw_Focus_Changed_Callback (Raw : System.Address; Focused : Bool) is
   begin

      if Window_Ptr (Raw).Focus_Changed_Event /= null then

         Window_Ptr (Raw).Focus_Changed_Event (Boolean (Focused));

      end if;

   end Raw_Focus_Changed_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Focus_Changed (Object : not null access Window;
                                Event  : Focus_Changed_Callback) is
   begin

      Object.Focus_Changed_Event := Event;

   end Set_Focus_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Iconification_Changed_Callback (Raw : System.Address; Iconified : Bool);
   pragma Convention (C, Raw_Iconification_Changed_Callback);

   procedure Raw_Iconification_Changed_Callback (Raw : System.Address; Iconified : Bool) is
   begin

      if Window_Ptr (Raw).Iconification_Changed_Event /= null then

         Window_Ptr (Raw).Iconification_Changed_Event (Boolean (Iconified));

      end if;

   end Raw_Iconification_Changed_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Iconification_Changed (Object : not null access Window;
                                        Event  : Iconification_Changed_Callback) is
   begin

      Object.Iconification_Changed_Event := Event;

   end Set_Iconification_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Framebuffer_Size_Changed_Callback (Raw : System.Address;
                                                    Width,
                                                    Height : Interfaces.C.int);
   pragma Convention (C, Raw_Framebuffer_Size_Changed_Callback);

   procedure Raw_Framebuffer_Size_Changed_Callback (Raw : System.Address;
                                                    Width,
                                                    Height : Interfaces.C.int) is
   begin

      if Window_Ptr (Raw).Framebuffer_Size_Changed_Event /= null then

         Window_Ptr (Raw).Framebuffer_Size_Changed_Event (Natural (Width),
                                                          Natural (Height));

      end if;

   end Raw_Framebuffer_Size_Changed_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Framebuffer_Size_Changed (Object : not null access Window;
                                           Event  : Framebuffer_Size_Changed_Callback) is
   begin

      Object.Framebuffer_Size_Changed_Event := Event;

   end Set_Framebuffer_Size_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Mouse_Button_Changed_Callback (Raw    : System.Address;
                                                Button : Input.Mouse.Button;
                                                State  : Input.Button_State;
                                                Mods   : Input.Keys.Modifiers);
   pragma Convention (C, Raw_Mouse_Button_Changed_Callback);

   procedure Raw_Mouse_Button_Changed_Callback (Raw    : System.Address;
                                                Button : Input.Mouse.Button;
                                                State  : Input.Button_State;
                                                Mods   : Input.Keys.Modifiers) is
   begin

      if Window_Ptr (Raw).Mouse_Button_Changed_Event /= null then

         Window_Ptr (Raw).Mouse_Button_Changed_Event (Button, State, Mods);

      end if;

   end Raw_Mouse_Button_Changed_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Mouse_Button_Changed (Object : not null access Window; Event : Mouse_Button_Changed_Callback) is
   begin

      if Event /= null then

         Object.Mouse_Button_Changed_Event := Event;

         Object.Enable_Callback (Callbacks.Mouse_Button);

      end if;

   end Set_Mouse_Button_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Mouse_Position_Changed_Callback (Raw : System.Address;
                                                  X, Y : Input.Mouse.Coordinate);
   pragma Convention (C, Raw_Mouse_Position_Changed_Callback);

   procedure Raw_Mouse_Position_Changed_Callback (Raw : System.Address;
                                                  X, Y : Input.Mouse.Coordinate) is
   begin

      if Window_Ptr (Raw).Mouse_Position_Changed_Event /= null then

         Window_Ptr (Raw).Mouse_Position_Changed_Event (X, Y);

      end if;

   end Raw_Mouse_Position_Changed_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Mouse_Position_Changed (Object : not null access Window;
                                         Event  : Mouse_Position_Changed_Callback) is
   begin

      if Event /= null then

         Object.Mouse_Position_Changed_Event := Event;

         Object.Enable_Callback (Callbacks.Mouse_Position);

      end if;

   end Set_Mouse_Position_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Mouse_Scrolled_Callback (Raw  : System.Address;
                                          X, Y : Input.Mouse.Scroll_Offset);
   pragma Convention (C, Raw_Mouse_Scrolled_Callback);

   procedure Raw_Mouse_Scrolled_Callback (Raw  : System.Address;
                                          X, Y : Input.Mouse.Scroll_Offset) is
   begin

      if Window_Ptr (Raw).Mouse_Scrolled_Event /= null then

         Window_Ptr (Raw).Mouse_Scrolled_Event (X, Y);

      end if;

   end Raw_Mouse_Scrolled_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Mouse_Scrolled (Object : not null access Window;
                                 Event  : Mouse_Scrolled_Callback) is
   begin

      Object.Mouse_Scrolled_Event := Event;

   end Set_Mouse_Scrolled;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Mouse_Entered_Callback (Raw  : System.Address;
                                         Action : Input.Mouse.Enter_Action);
   pragma Convention (C, Raw_Mouse_Entered_Callback);

   procedure Raw_Mouse_Entered_Callback (Raw  : System.Address;
                                         Action : Input.Mouse.Enter_Action) is
   begin

      if Window_Ptr (Raw).Mouse_Entered_Event /= null then

         Window_Ptr (Raw).Mouse_Entered_Event (Action);

      end if;

   end Raw_Mouse_Entered_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Mouse_Entered (Object : not null access Window;
                                Event  : Mouse_Entered_Callback) is
   begin

      Object.Mouse_Entered_Event := Event;

   end Set_Mouse_Entered;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Key_Changed_Callback (Raw : System.Address;
                                       Key : Input.Keys.Key;
                                       Scancode : Input.Keys.Scancode;
                                       Action   : Input.Keys.Action;
                                       Mods     : Input.Keys.Modifiers);
   pragma Convention (C, Raw_Key_Changed_Callback);

   procedure Raw_Key_Changed_Callback (Raw : System.Address;
                                       Key : Input.Keys.Key;
                                       Scancode : Input.Keys.Scancode;
                                       Action   : Input.Keys.Action;
                                       Mods     : Input.Keys.Modifiers) is
   begin

      if Window_Ptr (Raw).Key_Changed_Event /= null then

         Window_Ptr (Raw).Key_Changed_Event (Key, Scancode, Action, Mods);

      end if;

   end Raw_Key_Changed_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Key_Changed (Object : not null access Window;
                              Event  : Key_Changed_Callback) is
   begin

      Object.Key_Changed_Event := Event;

      Object.Enable_Callback (Callbacks.Key);

   end Set_Key_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Raw_Character_Entered_Callback (Raw  : System.Address;
                                             Char : Interfaces.C.unsigned);
   pragma Convention (C, Raw_Character_Entered_Callback);

   procedure Raw_Character_Entered_Callback (Raw  : System.Address;
                                             Char : Interfaces.C.unsigned) is

      function Convert is new Ada.Unchecked_Conversion (Interfaces.C.unsigned,
                                                        Wide_Wide_Character);

   begin

      if Window_Ptr (Raw).Character_Entered_Event /= null then

         Window_Ptr (Raw).Character_Entered_Event (Convert (Char));

      end if;

   end Raw_Character_Entered_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Character_Entered (Object : not null access Window;
                                    Event  : Character_Entered_Callback) is
   begin

      Object.Character_Entered_Event := Event;

   end Set_Character_Entered;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Init (Object        : not null access Window;
                   Width, Height : Size;
                   Title         : String;
                   Monitor       : Monitors.Monitor := Monitors.No_Monitor;
                   Share_Resources_With : access Window'Class := null) is

      use Conversion;
      use type System.Address;

      C_Title : constant Interfaces.C.char_array := Interfaces.C.To_C (Title);
      Share : System.Address;

   begin

      if Object.Handle /= System.Null_Address then
         raise Operation_Exception with "Window has already been initialized";
      end if;

      if Share_Resources_With = null then
         Share := System.Null_Address;
      else
         Share := Share_Resources_With.Handle;
      end if;

      Object.Handle := Api.Create_Window (Interfaces.C.int (Width),
                                          Interfaces.C.int (Height),
                                          C_Title, Monitor.Raw_Pointer, Share);

      if Object.Handle = System.Null_Address then
         raise Creation_Error;
      end if;

      Api.Set_Window_User_Pointer (Object.Handle,
                                   To_Address (Object_Pointer'(Object.all'Unchecked_Access)));

      Context.Make_Current (Object);

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Initialized (Object : not null access Window) return Boolean is
      use type System.Address;
   begin
      return Object.Handle /= System.Null_Address;
   end Initialized;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Destroy (Object : not null access Window) is
   begin
      Api.Destroy_Window (Object.Handle);
      Object.Handle := System.Null_Address;
   end Destroy;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Show (Object : not null access Window) is
   begin
      Api.Show_Window (Object.Handle);
   end Show;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Hide (Object : not null access Window) is
   begin
      Api.Hide_Window (Object.Handle);
   end Hide;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Title (Object : not null access Window; Value : String) is
   begin
      Api.Set_Window_Title (Object.Handle, Interfaces.C.To_C (Value));
   end Set_Title;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Key_State (Object : not null access Window; Key : Input.Keys.Key)
                       return Input.Button_State is
   begin
      return Api.Get_Key (Object.Handle, Key);
   end Key_State;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Mouse_Button_State (Object : not null access Window;
                                Button : Input.Mouse.Button)
                                return Input.Button_State is
   begin
      return Api.Get_Mouse_Button (Object.Handle, Button);
   end Mouse_Button_State;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Input_Toggle (Object : not null access Window;
                               Kind   : Input.Sticky_Toggle;
                               Value  : Boolean) is
   begin
      Api.Set_Input_Mode (Object.Handle, Kind, Bool (Value));
   end Set_Input_Toggle;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Cursor_Mode (Object : not null access Window)
                             return Input.Mouse.Cursor_Mode is
   begin
      return Api.Get_Input_Mode (Object.Handle, Enums.Mouse_Cursor);
   end Get_Cursor_Mode;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Cursor_Mode (Object : not null access Window;
                              Mode   : Input.Mouse.Cursor_Mode) is
   begin

      Api.Set_Input_Mode (Object.Handle, Enums.Mouse_Cursor, Mode);

   end Set_Cursor_Mode;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Get_Cursor_Pos (Object : not null access Window;
                             X, Y   : out Input.Mouse.Coordinate) is
   begin

      Api.Get_Cursor_Pos (Object.Handle, X, Y);

   end Get_Cursor_Pos;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Cursor_Pos (Object : not null access Window;
                             X, Y   : Input.Mouse.Coordinate) is
   begin
      Api.Set_Cursor_Pos (Object.Handle, X, Y);
   end Set_Cursor_Pos;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Get_Position (Object : not null access Window;
                           X, Y : out Coordinate) is
   begin
      Api.Get_Window_Pos (Object.Handle, X, Y);
   end Get_Position;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Position (Object : not null access Window;
                           X, Y : Coordinate) is
   begin
      Api.Set_Window_Pos (Object.Handle, X, Y);
   end Set_Position;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Get_Size (Object : not null access Window;
                       Width, Height : out Size) is
   begin
      Api.Get_Window_Size (Object.Handle, Width, Height);
   end Get_Size;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Size (Object : not null access Window;
                       Width, Height : Size) is
   begin
      Api.Set_Window_Size (Object.Handle, Width, Height);
   end Set_Size;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Decorated (Object : not null access Window; Value : Boolean) is
   begin

      ------------------------
      -- GLFW 3.3 or higher --
      ------------------------

      if Glfw_3_3 then

         Glfw.Api.Set_Window_Attrib (Object.Handle, Glfw.Enums.Decorated, To_Glfw_Boolean (Value));

      else
         null;

      end if;

   end Set_Decorated;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Restore (Object : not null access Window) is
   begin
      Api.Restore_Window (Object.Handle);
   end Restore;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Maximize (Object : not null access Window) is
   begin

      ------------------------
      -- GLFW 3.3 or higher --
      ------------------------

      Api.Maximize_Window (Object.Handle);

   end Maximize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Get_Framebuffer_Size (Object : not null access Window;
                                   Width, Height : out Size) is
   begin
      Api.Get_Framebuffer_Size (Object.Handle, Width, Height);
   end Get_Framebuffer_Size;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Visible (Object : not null access Window) return Boolean is
   begin
      return Boolean
        (Bool'(Api.Get_Window_Attrib (Object.Handle, Enums.Visible)));
   end Visible;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Iconified (Object : not null access Window) return Boolean is
   begin
      return Boolean
        (Bool'(Api.Get_Window_Attrib (Object.Handle, Enums.Iconified)));
   end Iconified;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Focused (Object : not null access Window) return Boolean is
   begin
      return Boolean
        (Bool'(Api.Get_Window_Attrib (Object.Handle, Enums.Focused)));
   end Focused;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Should_Close (Object : not null access Window) return Boolean is
   begin
      return Boolean (Api.Window_Should_Close (Object.Handle));
   end Should_Close;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Should_Close (Object : not null access Window;
                               Value : Boolean) is
   begin
      Api.Set_Window_Should_Close (Object.Handle, Bool (Value));
   end Set_Should_Close;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Enable_Callback (Object  : not null access Window;
                              Subject : Callbacks.Kind) is
   begin

      case Subject is

         when Callbacks.Position         => Api.Set_Window_Pos_Callback       (Object.Handle, Raw_Position_Callback'Access);
         when Callbacks.Size             => Api.Set_Window_Size_Callback      (Object.Handle, Raw_Size_Callback'Access);
         when Callbacks.Close            => Api.Set_Window_Close_Callback     (Object.Handle, Raw_Close_Callback'Access);
         when Callbacks.Refresh          => Api.Set_Window_Refresh_Callback   (Object.Handle, Raw_Refresh_Callback'Access);
         when Callbacks.Focus            => Api.Set_Window_Focus_Callback     (Object.Handle, Raw_Focus_Changed_Callback'Access);
         when Callbacks.Iconify          => Api.Set_Window_Iconify_Callback   (Object.Handle, Raw_Iconification_Changed_Callback'Access);
         when Callbacks.Framebuffer_Size => Api.Set_Framebuffer_Size_Callback (Object.Handle, Raw_Framebuffer_Size_Changed_Callback'Access);
         when Callbacks.Mouse_Button     => Api.Set_Mouse_Button_Callback     (Object.Handle, Raw_Mouse_Button_Changed_Callback'Access);
         when Callbacks.Mouse_Position   => Api.Set_Cursor_Pos_Callback       (Object.Handle, Raw_Mouse_Position_Changed_Callback'Access);
         when Callbacks.Mouse_Scroll     => Api.Set_Scroll_Callback           (Object.Handle, Raw_Mouse_Scrolled_Callback'Access);
         when Callbacks.Mouse_Enter      => Api.Set_Cursor_Enter_Callback     (Object.Handle, Raw_Mouse_Entered_Callback'Access);
         when Callbacks.Key              => Api.Set_Key_Callback              (Object.Handle, Raw_Key_Changed_Callback'Access);
         when Callbacks.Char             => Api.Set_Char_Callback             (Object.Handle, Raw_Character_Entered_Callback'Access);

      end case;

   end Enable_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Disable_Callback (Object  : not null access Window;
                               Subject : Callbacks.Kind) is
   begin

      case Subject is

         when Callbacks.Position         => Api.Set_Window_Pos_Callback       (Object.Handle, null);
         when Callbacks.Size             => Api.Set_Window_Size_Callback      (Object.Handle, null);
         when Callbacks.Close            => Api.Set_Window_Close_Callback     (Object.Handle, null);
         when Callbacks.Refresh          => Api.Set_Window_Refresh_Callback   (Object.Handle, null);
         when Callbacks.Focus            => Api.Set_Window_Focus_Callback     (Object.Handle, null);
         when Callbacks.Iconify          => Api.Set_Window_Iconify_Callback   (Object.Handle, null);
         when Callbacks.Framebuffer_Size => Api.Set_Framebuffer_Size_Callback (Object.Handle, null);
         when Callbacks.Mouse_Button     => Api.Set_Mouse_Button_Callback     (Object.Handle, null);
         when Callbacks.Mouse_Position   => Api.Set_Cursor_Pos_Callback       (Object.Handle, null);
         when Callbacks.Mouse_Scroll     => Api.Set_Scroll_Callback           (Object.Handle, null);
         when Callbacks.Mouse_Enter      => Api.Set_Cursor_Enter_Callback     (Object.Handle, null);
         when Callbacks.Key              => Api.Set_Key_Callback              (Object.Handle, null);
         when Callbacks.Char             => Api.Set_Char_Callback             (Object.Handle, null);

      end case;

   end Disable_Callback;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Get_OpenGL_Version (Object : not null access Window;
                                 Major, Minor, Revision : out Natural) is
      Value : Interfaces.C.int;

   begin

      Value := Api.Get_Window_Attrib (Object.Handle, Enums.Context_Version_Major);
      Major := Natural (Value);

      Value := Api.Get_Window_Attrib (Object.Handle, Enums.Context_Version_Minor);
      Minor := Natural (Value);

      Value := Api.Get_Window_Attrib (Object.Handle, Enums.Context_Revision);
      Revision := Natural (Value);

   end Get_OpenGL_Version;
   -----------------------------------------------------------------------------


end Glfw.Windows;
--------------------------------------------------------------------------------
