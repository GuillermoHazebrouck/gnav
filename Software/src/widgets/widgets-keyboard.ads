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

-- Gnav
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////

--//////////////////////////////////////////////////////////////////////////////
package Widgets.Keyboard is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The keys that are visible on the keyboard
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Keyboard_Keys is (Key_0, Key_1, Key_2, Key_3, Key_4, Key_5, Key_6, Key_7, Key_8, Key_9,
                          Key_A, Key_Z, Key_E, Key_R, Key_T, Key_Y, Key_U, Key_I, Key_O, Key_P, 
                          Key_Q, Key_S, Key_D, Key_F, Key_G, Key_H, Key_J, Key_K, Key_L, Key_M, 
                          Key_W, Key_X, Key_C, Key_V, Key_B, Key_N, Key_Minus,
                          Key_Space, Key_Back, Key_Enter);
   
   --type Keyboard_Keys is (Key_0, Key_1, Key_2, Key_3, Key_4, Key_5, Key_6, Key_7, Key_8, Key_9,
   --                       Key_A, Key_B, Key_C, Key_D, Key_E, Key_F, Key_G, Key_H, Key_I, Key_J, 
   --                       Key_K, Key_L, Key_M, Key_N, Key_O, Key_P, Key_Q, Key_R, Key_S, Key_T, 
   --                       Key_U, Key_V, Key_W, Key_X, Key_Y, Key_Z, 
   --                       Key_Space, Key_Back, Key_Enter);
   
   --===========================================================================
   -- Sets the allocation and reconfigures the layout if necessary
   --===========================================================================
   procedure Set_Allocation (Value : Allocation_Record);

   --===========================================================================
   --
   --===========================================================================
   procedure Init;
   
   --===========================================================================
   -- Sets the original text
   --===========================================================================
   procedure Set_Text (Text : String);
   
   --===========================================================================
   -- Gets the adapted text
   --===========================================================================
   function Get_Text return String;
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw;

   --===========================================================================
   --
   --===========================================================================
   function Key_Pressed (X, Y : Float) return Boolean;

end Widgets.Keyboard;
--------------------------------------------------------------------------------
