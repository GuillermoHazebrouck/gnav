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
with Flight.Aircraft;
with Gl.Fonts;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Colors;
use  Utility.Colors;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.System is

   -- System page widgets
   ---------------------------------
   
   Btn_Power_Off  : Button_Record;
       
   Pnl_Power      : Panel_Record;

   Pnl_Screen     : Panel_Record;

   Pnl_Aquisition : Panel_Record;

   --===========================================================================
   --
   --===========================================================================
   procedure Init is
     
      Allocation : Allocation_Record;
      
   begin
        
      -- Screen panel
      ------------------------------------------------------

      Allocation.X := 0.010;
      Allocation.Y := 0.010;
      Allocation.W := 0.320;
      Allocation.H := 0.850;

      Pnl_Screen.Set_Allocation (Allocation);

      Pnl_Screen.Set_Background_Color (Color_Black);

      Pnl_Screen.Set_Transparency (0.60);

      Pnl_Screen.Set_Show_Border (True);

      Pnl_Screen.Set_Label ("SCREEN", Label_Left);

      Pnl_Screen.Set_Font_Size (0.03, 0.25);

      Pnl_Screen.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));
 
      -- Power panel
      ------------------------------------------------------

      Allocation.X := 0.340;
      Allocation.Y := 0.010;
      Allocation.W := 0.320;
      Allocation.H := 0.850;

      Pnl_Power.Set_Allocation (Allocation);

      Pnl_Power.Set_Background_Color (Color_Black);

      Pnl_Power.Set_Transparency (0.60);

      Pnl_Power.Set_Show_Border (True);

      Pnl_Power.Set_Label ("POWER", Label_Left);

      Pnl_Power.Set_Font_Size (0.03, 0.25);

      Pnl_Power.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      --
      
      Allocation.W := 0.9 * Pnl_Power.Get_Allocation.W;
      
      Allocation.H := 0.1;
      
      Allocation.X := Pnl_Power.Get_Allocation.X + 0.05 * Pnl_Power.Get_Allocation.W;
      
      Allocation.Y := Pnl_Power.Get_Allocation.Y + 0.02;
      
      Btn_Power_Off.Set_Allocation (Allocation);
           
      Btn_Power_Off.Set_Label_Color (Color_White);
      
      Btn_Power_Off.Set_Font_Size (0.4, 0.3);
      
      Btn_Power_Off.Set_Background_Color (Color_Reddish);
        
      Btn_Power_Off.Set_Label ("POWER OFF");
      
      -- Aquisition panel
      ------------------------------------------------------

      Allocation.X := 0.670;
      Allocation.Y := 0.010;
      Allocation.W := 0.320;
      Allocation.H := 0.850;

      Pnl_Aquisition.Set_Allocation (Allocation);

      Pnl_Aquisition.Set_Background_Color (Color_Black);

      Pnl_Aquisition.Set_Transparency (0.60);

      Pnl_Aquisition.Set_Show_Border (True);

      Pnl_Aquisition.Set_Label ("AQUISITION", Label_Left);

      Pnl_Aquisition.Set_Font_Size (0.03, 0.25);

      Pnl_Aquisition.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

   end Init;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw (Width, Height : Float) is
   begin
      
      Pnl_Screen.Draw;
      
      Pnl_Power.Draw;
      
      Btn_Power_Off.Draw;
      
      Pnl_Aquisition.Draw;
      
   end Draw;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
      
      use Flight.Aircraft;
      
   begin
      
      if Btn_Power_Off.Contains (X, Y) then
               
         Display.Stop := True;
               
      end if;
      
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Handles a key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin
      
      null;
      
   end Key_Changed;
   -----------------------------------------------------------------------------
          
     
     
end Display.Pages.System;
--------------------------------------------------------------------------------
