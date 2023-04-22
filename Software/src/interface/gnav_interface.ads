
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
with Interfaces.C;
use  Interfaces.C;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Gnav_Interface is

   -- Java types
   subtype J_Int    is int;
   subtype J_Long   is long;
   subtype J_Float  is c_float;
   subtype J_Double is double;

   -- Dummy Java environment
   type J_Environ_Record is null record with Convention => C;
   type J_Environ_Access is access J_Environ_Record with Convention => C;

   -- Dummy Java object
   type J_Object_Record is null record with Convention => C;
   type J_Object_Access is access J_Object_Record with Convention => C;

   --===========================================================================
   -- Initializes the graphics (loading shaders and setting up main matrix)
   --===========================================================================
   procedure Gnav_Start_Graphics (Env : J_Environ_Access; Obj : J_Object_Access);
   pragma Export (C, Gnav_Start_Graphics, "Java_gnavInterface_startGraphics");

   --===========================================================================
   -- Runs the main step (note that this uses the GPU)
   --===========================================================================
   procedure Gnav_Start_Data (Env : J_Environ_Access; Obj : J_Object_Access);
   pragma Export (C, Gnav_Start_Data, "Java_gnavInterface_startData");

   --===========================================================================
   -- Indicates GNAV that the size of the screen has changed
   --===========================================================================
   procedure Gnav_Size_Changed (Env : J_Environ_Access; Obj : J_Object_Access; W, H : J_Int);
   pragma Export (C, Gnav_Size_Changed, "Java_gnavInterface_sizeChanged");

   --===========================================================================
   -- Runs a processing step
   --===========================================================================
   procedure Gnav_Process_Timer (Env : J_Environ_Access; Obj : J_Object_Access);
   pragma Export (C, Gnav_Process_Timer, "Java_gnavInterface_processTimer");

   --===========================================================================
   -- Runs a processing step
   --===========================================================================
   procedure Gnav_Set_Position (Env : J_Environ_Access; Obj : J_Object_Access;
                                Lat, Lon, Alt : J_Double;
                                Spd, Brg      : J_Float);
   pragma Export (C, Gnav_Set_Position, "Java_gnavInterface_setPosition");

   --===========================================================================
   --
   --===========================================================================
   procedure Gnav_Screen_Pressed (Env : J_Environ_Access; Obj : J_Object_Access; X, Y : J_Long);
   pragma Export (C, Gnav_Screen_Pressed, "Java_gnavInterface_screenPressed");

   --===========================================================================
   -- Indicates if there is a drawing refresh pending
   --===========================================================================
   function Gnav_Refresh_Pending (Env : J_Environ_Access; Obj : J_Object_Access) return J_Int;
   pragma Export (C, Gnav_Refresh_Pending, "Java_gnavInterface_refreshPending");

   --===========================================================================
   -- Refreshes the screen using the active OpenGL context
   --===========================================================================
   procedure Gnav_Draw_Frame (Env : J_Environ_Access; Obj : J_Object_Access);
   pragma Export (C, Gnav_Draw_Frame, "Java_gnavInterface_drawFrame");

   --===========================================================================
   -- Runs the main step
   --===========================================================================
   procedure Gnav_Stop (Env : J_Environ_Access; Obj : J_Object_Access);
   pragma Export (C, Gnav_Stop, "Java_gnavInterface_stop");

   --===========================================================================
   --
   --===========================================================================
   function Gnav_Test_Interface (Env : J_Environ_Access; Obj : J_Object_Access; X, Y : J_Int) return J_Int;
   pragma Export (C, Gnav_Test_Interface, "Java_gnavInterface_testInterface");

end Gnav_Interface;
--------------------------------------------------------------------------------
