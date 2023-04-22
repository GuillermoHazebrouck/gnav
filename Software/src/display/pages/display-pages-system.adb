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
with Display.Panels.Stream;
with Flight.Signal;
with Flight.Stream;
with Gl.Colors;
use  Gl.Colors;
with Gl.Fonts;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Dialog;
use  Widgets.Dialog;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.System is

   -- System page widgets
   ---------------------------------
   
   Pnl_Power     : Panel_Record;

   Btn_Power_Off : Button_Record;
     
   Btn_Reset     : Button_Record;

   Btn_Utc       : Button_Record;

   Btn_Utc_Plus  : Button_Record;

   Btn_Utc_Min   : Button_Record;

   -- Screen page widgets
   ---------------------------------
   
   Pnl_Screen    : Panel_Record;
   
   -- Local time page widgets
   ---------------------------------
   
   Pnl_Time      : Panel_Record;
   
   -- Fonts
   ---------------------------------

   Font_1        : Gl.Fonts.Font_Style_Record := (Width     => 0.010,
                                                  Height    => 0.030,
                                                  Space     => 0.004,
                                                  Rendering => Gl.Fonts.Font_Glow,
                                                  Thickness => Gl.Fonts.Font_Regular);
   
   
   -- Internal variables
   ---------------------------------

   type Confirmation_Kinds is (Confim_Power_Off, Confim_Reset, Confirm_None);
   
   Pending_Confirmation : Confirmation_Kinds := Confirm_None;
   
   --===========================================================================
   --
   --===========================================================================
   procedure Update_Utc_Button is
      
      Label : String := Integer'Image (Flight.Get_Time_Zone);
      
   begin
      
      if Label (1) = ' ' then
         
         Label (1) := '+';
         
      end if;
      
      Btn_Utc.Set_Label (Label);
      
   end Update_Utc_Button;
   -----------------------------------------------------------------------------
   
   
   
   
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
      Allocation.Y := 0.440;
      Allocation.W := 0.320;
      Allocation.H := 0.400;

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
      
      Allocation.Y := Pnl_Power.Get_Allocation.Y + Pnl_Power.Get_Allocation.H - 0.30;
      
      Btn_Power_Off.Set_Allocation (Allocation);
           
      Btn_Power_Off.Set_Label_Color (Color_White);
      
      Btn_Power_Off.Set_Font_Size (0.4, 0.3);
      
      Btn_Power_Off.Set_Background_Color (Color_Reddish);
        
      Btn_Power_Off.Set_Label ("POWER OFF");
      
      --
      
      Allocation.Y := Btn_Power_Off.Get_Allocation.Y + Allocation.H + 0.02;
      
      Btn_Reset.Set_Allocation (Allocation);
           
      Btn_Reset.Set_Label_Color (Color_White);
      
      Btn_Reset.Set_Font_Size (0.4, 0.3);
      
      Btn_Reset.Set_Background_Color (Color_Reddish);
        
      Btn_Reset.Set_Label ("RESET");
      
      -- Time panel
      ------------------------------------------------------

      Allocation.X := 0.340;
      Allocation.Y := 0.010;
      Allocation.W := 0.320;
      Allocation.H := 0.400;

      Pnl_Time.Set_Allocation (Allocation);

      Pnl_Time.Set_Background_Color (Color_Black);

      Pnl_Time.Set_Transparency (0.60);

      Pnl_Time.Set_Show_Border (True);

      Pnl_Time.Set_Label ("TIME ZONE", Label_Left);

      Pnl_Time.Set_Font_Size (0.03, 0.25);

      Pnl_Time.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      -- Decrease UTC offset button

      Btn_Utc_Min.Set_Label ("-");

      Btn_Utc_Min.Set_Background_Color (Color_Sky);

      Btn_Utc_Min.Set_Border_Color (Color_Blue);

      Btn_Utc_Min.Set_Label_Color (Color_White);

      Btn_Utc_Min.Set_Font_Size (0.7, 0.5);

      Allocation.X := Pnl_Time.Get_Allocation.X + 0.01;
      Allocation.Y := Pnl_Time.Get_Allocation.Y + 0.40 * Pnl_Time.Get_Allocation.H;
      Allocation.W := 0.07;
      Allocation.H := 0.10;

      Btn_Utc_Min.Set_Allocation (Allocation);

      -- Increase UTC offset button

      Btn_Utc_Plus.Set_Label ("+");

      Btn_Utc_Plus.Set_Background_Color (Color_Sky);

      Btn_Utc_Plus.Set_Border_Color (Color_Blue);

      Btn_Utc_Plus.Set_Label_Color (Color_White);

      Btn_Utc_Plus.Set_Font_Size (0.7, 0.5);

      Allocation.X := Pnl_Time.Get_Allocation.X + Pnl_Time.Get_Allocation.W - 0.08;

      Btn_Utc_Plus.Set_Allocation (Allocation);

      -- UTC offset button     

      Allocation.X := Btn_Utc_Min.Get_Allocation.X  + Btn_Utc_Min.Get_Allocation.W + 0.01;
      Allocation.W := Btn_Utc_Plus.Get_Allocation.X - Allocation.X - 0.01;

      Btn_Utc.Set_Allocation (Allocation);
      Btn_Utc.Set_Font_Size (0.6, 0.3, 0.5);
      
      Update_Utc_Button;

      -- Aquisition panel
      ------------------------------------------------------

      Display.Panels.Stream.Init (0.670, 0.320);
      
   end Init;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is
      
      use Gl.Fonts;
  
   begin
      
      -- Screen and map panel
      --------------------------------------------
      
      Pnl_Screen.Draw;
      
      -- Power panel
      --------------------------------------------
      
      Pnl_Power.Draw;
      
      Btn_Power_Off.Draw;
      
      Btn_Reset.Draw;
      
      -- Time panel
      --------------------------------------------
      
      Pnl_Time.Draw;
      
      Btn_Utc_Min.Draw;
      
      Btn_Utc_Plus.Draw;
      
      Btn_Utc.Draw;
      
      if Flight.Time_Synchronized then
      
         Gl.Fonts.Draw ("TIME SYNCHRONIZED",
                        Btn_Utc_Min.Get_Allocation.X,
                        Btn_Utc_Min.Get_Allocation.Y - 2.0 * Font_1.Height,
                        Font_1,
                        Line_Green,
                        Gl.Fonts.Alignment_LL);
      else
         
         Gl.Fonts.Draw ("TIME NOT SYNCHRONIZED",
                        Btn_Utc_Min.Get_Allocation.X,
                        Btn_Utc_Min.Get_Allocation.Y - 2.0 * Font_1.Height,
                        Font_1,
                        Line_Gray,
                        Gl.Fonts.Alignment_LL);
      
      end if;
      
      -- Acquisition panel
      --------------------------------------------
      
      Display.Panels.Stream.Draw;
      
   end Draw;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Handle_Dialog_Action (Result : Dialog_Result_Kind) is
   begin
      
      case Result is
            
         when Dialog_Ok =>
             
            case Pending_Confirmation is
               
               when Confim_Power_Off =>
                  
                  Display.Stop := True;
                  
               when Confim_Reset =>
                  
                  Display.Reset := True;
              
               when others => null;
                  
            end case;
               
            Refresh := True;
                 
         when Dialog_Cancel =>
             
            Pending_Confirmation := Confirm_None;
               
            Refresh := True;
                  
      end case;
      
   end Handle_Dialog_Action;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
      
      use Widgets.Dialog;
      
   begin
      
      if Btn_Utc_Plus.Contains (X, Y) then
         
         Flight.Set_Time_Zone (Flight.Get_Time_Zone + 1);
         
         Update_Utc_Button;
         
         Refresh := True;
         
      elsif Btn_Utc_Min.Contains (X, Y) then
         
         Flight.Set_Time_Zone (Flight.Get_Time_Zone - 1);
         
         Update_Utc_Button;
         
         Refresh := True;
         
      elsif Btn_Power_Off.Contains (X, Y) then
            
         Pending_Confirmation := Confim_Power_Off;
            
         Widgets.Dialog.Confirm ("CONFIRM POWER OFF", Handle_Dialog_Action'Access);
         
         Refresh := True;
                  
      elsif Btn_Reset.Contains (X, Y) then
              
         Pending_Confirmation := Confim_Reset;
         
         Widgets.Dialog.Confirm ("CONFIRM RESET", Handle_Dialog_Action'Access);
           
         Refresh := True;
                
      else
         
         Display.Panels.Stream.Screen_Pressed (X, Y);
         
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
