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
with Flight.Stream;
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
   
   Pnl_Power     : Panel_Record;

   Btn_Power_Off : Button_Record;
       
   --
   
   Pnl_Screen    : Panel_Record;

   --
   
   Pnl_Stream    : Panel_Record;

   Font_Label    : Gl.Fonts.Font_Style_Record := (Width     => 0.010,
                                                  Height    => 0.030,
                                                  Space     => 0.004,
                                                  Rendering => Gl.Fonts.Font_Glow,
                                                  Thickness => Gl.Fonts.Font_Regular,
                                                  Line_R    => 0.6,
                                                  Line_G    => 0.6,
                                                  Line_B    => 0.6,
                                                  Glow_R    => 0.1,
                                                  Glow_G    => 0.1,
                                                  Glow_B    => 0.1);
   
   Font_Ok       : Gl.Fonts.Font_Style_Record := (Width     => 0.010,
                                                  Height    => 0.030,
                                                  Space     => 0.004,
                                                  Rendering => Gl.Fonts.Font_Glow,
                                                  Thickness => Gl.Fonts.Font_Regular,
                                                  Line_R    => 0.0,
                                                  Line_G    => 1.0,
                                                  Line_B    => 0.0,
                                                  Glow_R    => 0.1,
                                                  Glow_G    => 0.1,
                                                  Glow_B    => 0.1);
   
   Font_Error    : Gl.Fonts.Font_Style_Record := (Width     => 0.010,
                                                  Height    => 0.030,
                                                  Space     => 0.004,
                                                  Rendering => Gl.Fonts.Font_Glow,
                                                  Thickness => Gl.Fonts.Font_Regular,
                                                  Line_R    => 1.0,
                                                  Line_G    => 0.0,
                                                  Line_B    => 0.0,
                                                  Glow_R    => 0.1,
                                                  Glow_G    => 0.1,
                                                  Glow_B    => 0.1);
   
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

      Pnl_Stream.Set_Allocation (Allocation);

      Pnl_Stream.Set_Background_Color (Color_Black);

      Pnl_Stream.Set_Transparency (0.60);

      Pnl_Stream.Set_Show_Border (True);

      Pnl_Stream.Set_Label ("STREAMING", Label_Left);

      Pnl_Stream.Set_Font_Size (0.03, 0.25);

      Pnl_Stream.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

   end Init;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is
      
      use Gl.Fonts;
      
      X, Y : Float;
      
   begin
      
      -- Screen and map panel
      --------------------------------------------
      
      Pnl_Screen.Draw;
      
      -- Power panel
      --------------------------------------------
      
      Pnl_Power.Draw;
      
      Btn_Power_Off.Draw;
      
      -- Acquisition panel
      --------------------------------------------
      
      Pnl_Stream.Draw;
      
      -- Source kind
      
      X := Pnl_Stream.Get_Allocation.X + 0.02;
      
      Y := Pnl_Stream.Get_Allocation.Y + Pnl_Stream.Get_Allocation.H - 0.1;
      
      Draw ("SOURCE:", X, Y, Font_Label);
      
      X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;
      
      case Flight.Stream.Get_Source_Kind is
         
         when Flight.Stream.Stream_Source_File =>
            
            Draw ("RECORDING", X, Y, Font_Label, Alignment_LR);
      
         when Flight.Stream.Stream_Source_Udp =>
            
            Draw ("UDP", X, Y, Font_Label, Alignment_LR);
      
         when Flight.Stream.Stream_Source_Serial =>
            
            Draw ("SERIAL", X, Y, Font_Label, Alignment_LR);
      
         when others =>
            
            Draw ("NOT SET", X, Y, Font_Label, Alignment_LR);
      
      end case;
      
      -- Data formating sytem
      
      X := Pnl_Stream.Get_Allocation.X + 0.02;
      
      Y := Y - 2.0 * Font_Label.Height;
      
      Draw ("FORMAT:", X, Y, Font_Label);
      
      X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;
      
      Draw (Flight.Stream.Get_Protocol_Key (Flight.Stream.Get_Protocol_Kind), X, Y, Font_Label, Alignment_LR);
               
      --
      
      X := Pnl_Stream.Get_Allocation.X + 0.02;
      
      Y := Y - 2.0 * Font_Label.Height;
      
      Draw ("STATUS:", X, Y, Font_Label); -- OK / ERROR
      
      X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;
      
      if Flight.Stream.Is_Active then
         
         Draw ("ACTIVE", X, Y, Font_Ok, Alignment_LR);
      
      elsif Blink then
         
         Draw ("ERROR", X, Y, Font_Error, Alignment_LR);
               
      end if;
      
      -- Incoming data rate
      
      X := Pnl_Stream.Get_Allocation.X + 0.02;
      
      Y := Y - 2.0 * Font_Label.Height;
      
      Draw ("RATE:", X, Y, Font_Label); -- 4P/S
        
      X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;
      
      if Flight.Stream.Get_Rate > 0 then
         
         Draw (Natural'Image (Flight.Stream.Get_Rate), X, Y, Font_Ok, Alignment_LR);
      
      else
         
         Draw ("NO INPUT", X, Y, Font_Error, Alignment_LR);
               
      end if;
      
      -- GPS fix status
      
      X := Pnl_Stream.Get_Allocation.X + 0.02;
      
      Y := Y - 2.0 * Font_Label.Height;
      
      Draw ("GPS:", X, Y, Font_Label); -- 3
      
      X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;
      
      if Flight.Data.Age (Flight.Field_Position)  < 3.0 then
         
         Draw ("OK", X, Y, Font_Ok, Alignment_LR);
      
      else
         
         Draw ("NO FIX", X, Y, Font_Error, Alignment_LR);
               
      end if;
      
   end Draw;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
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
