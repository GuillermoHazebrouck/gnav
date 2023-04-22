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
-- Gnav
with Display.Panels.Baro;
with Display.Panels.Radio;
with Flight;
with Flight.Aircraft;
with Gl.Colors;
use  Gl.Colors;
with Gl.Fonts;
with Maps;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Check is
        
   Font_1 : Gl.Fonts.Font_Style_Record := (Width     => 0.016,
                                           Height    => 0.050,
                                           Space     => 0.006,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular);

   --===========================================================================
   --
   --===========================================================================
   procedure Init is
   begin
        
      Display.Panels.Baro.Init  (0.010, 0.320);
      
      Display.Panels.Radio.Init (0.340, 0.650);
      
   end Init;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is
      
      use Gl.Fonts;
      
   begin
     
      -- Aircraft registration
      
      Draw (Flight.Aircraft.This_Aircraft.Registration,
            0.19, 0.92, Font_1, Line_Cyan);
      
      -- Current position registration
      
      if Flight.Data.Is_Recent (Flight.Field_Position) then
         
         Draw (Maps.Image (Flight.Data.Position),
               0.98, 0.92, Font_1, Line_Green, Alignment_LR);
         
      elsif Flight.Data.Is_Valid (Flight.Field_Position) then
         
         Draw (Maps.Image (Flight.Data.Position),
               0.98, 0.92, Font_1, Line_Gray, Alignment_LR);
         
      end if;
      
      -- Panels
            
      Display.Panels.Baro.Draw;
      
      Display.Panels.Radio.Draw;
      
   end Draw;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin
      
      Display.Panels.Baro.Screen_Pressed (X, Y);
      
      Display.Panels.Radio.Screen_Pressed (X, Y);
      
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin
      
      Display.Panels.Baro.Key_Changed (Key);
      
      Display.Panels.Radio.Key_Changed (Key);
      
   end Key_Changed;
   -----------------------------------------------------------------------------
          
     
     
end Display.Pages.Check;
--------------------------------------------------------------------------------
