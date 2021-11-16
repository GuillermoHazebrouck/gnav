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

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Glfw.Windows.Clipboard is
   -- strings are UTF-8 encoded

   function Get (Object : not null access Window'Class) return String;

   procedure Set (Object : not null access Window'Class; Value : String);
end Glfw.Windows.Clipboard;
--------------------------------------------------------------------------------
