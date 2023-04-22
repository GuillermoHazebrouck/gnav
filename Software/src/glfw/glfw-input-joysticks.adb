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

-- Local
with Glfw.Api;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Glfw.Input.Joysticks is

   function Index (Source : Joystick) return Joystick_Index is
   begin
      return Enums.Joystick_ID'Pos (Source.Raw_Index) + 1;
   end Index;

   procedure Set_Index (Target : in out Joystick; Value : Joystick_Index) is
   begin
      Target.Raw_Index := Enums.Joystick_ID'Val (Value - 1);
   end Set_Index;

   function Present (Source : Joystick) return Boolean is
   begin
      return Boolean (Api.Joystick_Present (Source.Raw_Index));
   end Present;

   function Positions (Source : Joystick) return Axis_Positions is
      Count : aliased Interfaces.C.int;
      Raw : constant Api.Axis_Position_List_Pointers.Pointer
        := Api.Get_Joystick_Axes (Source.Raw_Index, Count'Access);
   begin
      return Api.Axis_Position_List_Pointers.Value
        (Raw, Interfaces.C.ptrdiff_t (Count));
   end Positions;

   function Button_States (Source : Joystick) return Joystick_Button_States is
      Count : aliased Interfaces.C.int;
      Raw : constant Api.Joystick_Button_State_List_Pointers.Pointer
        := Api.Get_Joystick_Buttons (Source.Raw_Index, Count'Access);
   begin
      return Api.Joystick_Button_State_List_Pointers.Value
        (Raw, Interfaces.C.ptrdiff_t (Count));
   end Button_States;

end Glfw.Input.Joysticks;
--------------------------------------------------------------------------------
