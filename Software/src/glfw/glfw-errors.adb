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
with Interfaces.C.Strings;
-- Local
with Glfw.Api;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Glfw.Errors is

   Cur_Callback : Callback := null;

   procedure Raw_Handler (Code : Kind;
                          Description : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, Raw_Handler);

   procedure Raw_Handler (Code : Kind;
                          Description : Interfaces.C.Strings.chars_ptr) is
   begin
      if Cur_Callback /= null then
         Cur_Callback.all (Code, Interfaces.C.Strings.Value (Description));
      end if;
   end Raw_Handler;

   procedure Set_Callback (Handler : Callback) is
      Previous : Api.Error_Callback;
      pragma Warnings (Off, Previous);
   begin
      Cur_Callback := Handler;
      if Handler = null then
         Previous := Api.Set_Error_Callback (null);
      else
         Previous := Api.Set_Error_Callback (Raw_Handler'Access);
      end if;
   end Set_Callback;

end Glfw.Errors;
--------------------------------------------------------------------------------
