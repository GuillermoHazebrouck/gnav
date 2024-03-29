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
with Ada.Text_IO;
-- Gnav
with Display.Pages.Check;
with Display.Pages.Glider;
with Display.Pages.Map;
with Display.Pages.Navigation;
with Display.Pages.Route_Edition;
with Display.Pages.Strips;
with Display.Pages.System;
with Display.Pages.Wind;
with Flight.Aircraft;
with Flight.Plan;
with Gl.Colors;
use  Gl.Colors;
with Gl.Fonts;
with Widgets.Widget;
use  Widgets.Widget;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Dialog;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Menu is
   
   type Page_Types is (Page_Navigation, 
                       Page_Strips, 
                       Page_Route_Edition, 
                       Page_Glider, 
                       Page_Map, 
                       Page_Wind,
                       Page_Check_List, 
                       Page_System);
   
   Active_Page : Page_Types := Page_Navigation;
      
   -- Main menu buttons
   ---------------------------------

   Btn_Strips        : Button_Record;
   
   Btn_Route_Edition : Button_Record;
   
   Btn_Glider        : Button_Record;
      
   Btn_Check_List    : Button_Record;
   
   Btn_Map_Options   : Button_Record;
     
   Btn_Wind          : Button_Record;
     
   Btn_System        : Button_Record;
      
   Btn_Exit          : Button_Record;
   
   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Init is
      
      M : constant Dimension_Float := 0.01;
      H : constant Dimension_Float := (1.0 - 9.0 * M) / 8.0;
      W : constant Dimension_Float := 0.15;

      Allocation : Allocation_Record;
      
   begin

      -- General confirmation dialog
      -----------------------------------------------------
      Widgets.Dialog.Init;
                        
      -- Flight plan page button
      ------------------------------------------------------      
      Allocation.X := M;
      
      Allocation.H := H;
      
      Allocation.Y := 1.0 -  (H + M);
      
      Allocation.W := W;

      Btn_Strips.Set_Label ("WAYPOINT");
      
      Btn_Strips.Set_Allocation (Allocation);
      
      Btn_Strips.Set_Background_Color (Color_Black.With_Alpha (0.5));
      
      Btn_Strips.Set_Label_Color (Fore => Color_White,
                                      Glow => Color_Black);
      
      Btn_Strips.Set_Font_Size (0.3, 0.28);
              
      -- Route editor page button
      ------------------------------------------------------
      Btn_Route_Edition.Set_Label ("ROUTE");
      
      Allocation.Y := Allocation.Y - H - M;
      
      Btn_Route_Edition.Set_Allocation (Allocation);
      
      Btn_Route_Edition.Set_Background_Color (Color_Black.With_Alpha (0.5));
      
      Btn_Route_Edition.Set_Label_Color (Fore => Color_White,
                                         Glow => Color_Black);
      
      Btn_Route_Edition.Set_Font_Size (0.3, 0.3);
      
      -- Glider page button
      ------------------------------------------------------
      Btn_Glider.Set_Label ("GLIDER");
      
      Allocation.Y := Allocation.Y - H - M;
      
      Btn_Glider.Set_Allocation (Allocation);
            
      Btn_Glider.Set_Background_Color (Color_Black.With_Alpha (0.5));
      
      Btn_Glider.Set_Label_Color (Fore => Color_White,
                                  Glow => Color_Black);
      
      Btn_Glider.Set_Font_Size (0.3, 0.3);
                 
      -- Map configuration page button
      ------------------------------------------------------
      --Btn_Map_Options.Set_Label ("MAP");
      
      --Allocation.Y := Allocation.Y - H - M;
      
      --Btn_Map_Options.Set_Allocation (Allocation);
            
      --Btn_Map_Options.Set_Background_Color (Color_Black.With_Alpha (0.5));
      
      --Btn_Map_Options.Set_Label_Color (Fore => Color_White,
      --                                 Glow => Color_Black);
      
      --Btn_Map_Options.Set_Font_Size (0.3, 0.3);
                    
      -- Wind info button
      ------------------------------------------------------
      Btn_Wind.Set_Label ("WIND");
      
      Allocation.Y := Allocation.Y - H - M;
      
      Btn_Wind.Set_Allocation (Allocation);
            
      Btn_Wind.Set_Background_Color (Color_Black.With_Alpha (0.5));
      
      Btn_Wind.Set_Label_Color (Fore => Color_White,
                                Glow => Color_Black);
      
      Btn_Wind.Set_Font_Size (0.3, 0.3);
                   
      -- Checklist page button
      ------------------------------------------------------
      Btn_Check_List.Set_Label ("CHECK");
      
      Allocation.Y := Allocation.Y - H - M;
      
      Btn_Check_List.Set_Allocation (Allocation);
            
      Btn_Check_List.Set_Background_Color (Color_Black.With_Alpha (0.5));
      
      Btn_Check_List.Set_Label_Color (Fore => Color_White,
                                      Glow => Color_Black);
      
      Btn_Check_List.Set_Font_Size (0.3, 0.3);
                  
      -- System settings page button
      ------------------------------------------------------
      Btn_System.Set_Label ("SYSTEM");
      
      Allocation.Y := Allocation.Y - H - M;
      
      Btn_System.Set_Allocation (Allocation);
            
      Btn_System.Set_Background_Color (Color_Black.With_Alpha (0.5));
      
      Btn_System.Set_Label_Color (Fore => Color_White,
                                     Glow => Color_Black);
      
      Btn_System.Set_Font_Size (0.3, 0.3);
            
      -- Checklist page button
      ------------------------------------------------------      
      Allocation.X := 0.01;
      
      Allocation.H := 0.10;
      
      Allocation.Y := 0.89;
      
      Allocation.W := 0.15;
        
      Btn_Exit.Set_Label ("< NAV");
      
      Btn_Exit.Set_Allocation (Allocation);
            
      Btn_Exit.Set_Label_Color (Color_White);
      
      Btn_Exit.Set_Font_Size (0.4, 0.3);
      
      Btn_Exit.Set_Background_Color (Color_Reddish);
        
      -- Sub pages
      ------------------------------------------------------
      Display.Pages.Navigation.Init;
      
      Display.Pages.Strips.Init;
      
      Display.Pages.Route_Edition.Init;
      
      Display.Pages.Glider.Init;
      
      --Display.Pages.Map.Init;
      
      Display.Pages.Wind.Init;
      
      Display.Pages.Check.Init;
      
      Display.Pages.System.Init;
      
   end Init;
   -----------------------------------------------------------------------------
      
   
   
   
   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Draw is      
   begin
      
      -- Draw the active page under the menu
      --------------------------------------
      
      case Active_Page is
            
         when Page_Navigation =>
                 
            Display.Pages.Navigation.Draw;
            
            Btn_Strips.Draw;
            
            Btn_Route_Edition.Draw;
            
            Btn_Glider.Draw;
            
            Btn_Wind.Draw;
         
            --Btn_Map_Options.Draw;
         
            Btn_Check_List.Draw;
         
            Btn_System.Draw;
         
         when Page_Route_Edition =>
            
            Display.Pages.Route_Edition.Draw;
         
            Btn_Exit.Draw;
              
         when Page_Strips =>
            
            Display.Pages.Strips.Draw;
            
            Btn_Exit.Draw;
                
         when Page_Glider =>
            
            Display.Pages.Glider.Draw;
      
            Btn_Exit.Draw;
                  
         when Page_Map =>
            
            Display.Pages.Map.Draw;
      
            Btn_Exit.Draw;
                
         when Page_Wind =>
            
            Display.Pages.Wind.Draw;
      
            Btn_Exit.Draw;
              
         when Page_Check_List =>
            
            Display.Pages.Check.Draw;
            
            Btn_Exit.Draw;
                
         when Page_System =>
              
            Display.Pages.System.Draw;
            
            Btn_Exit.Draw;
              
      end case;
      
      -- Dialog (this must always be the last one)
      -----------------------------------------------
      Widgets.Dialog.Draw;
      
   end Draw;
   -----------------------------------------------------------------------------
   



   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin

      --------------------------------------------------------------------------	
      -- Handle pending action on dialog
      --------------------------------------------------------------------------
      
      if Widgets.Dialog.Is_Open then
         
         Widgets.Dialog.Handle_Action (X, Y);
         
         return;
         
      end if;
         
      --------------------------------------------------------------------------	
      -- Check main menu page widgets first
      --------------------------------------------------------------------------
            
      if Active_Page = Page_Navigation then
         
         if Btn_Strips.Contains (X, Y) then
         
            Active_Page := Page_Strips;
            
            Display.Refresh := True;
         
            return;
        
         elsif Btn_Route_Edition.Contains (X, Y) then
            
            Active_Page := Page_Route_Edition;
            
            Flight.Plan.Jump_In_Proximity := False;
                    
            Display.Refresh := True;
         
            return;
                        
         elsif Btn_Glider.Contains (X, Y) then
         
            Active_Page := Page_Glider;
        
            Display.Refresh := True;
         
            return;
        
         elsif Btn_Wind.Contains (X, Y) then
         
            Active_Page := Page_Wind;
        
            Display.Refresh := True;
         
            return;
        
         elsif Btn_Check_List.Contains (X, Y) then
         
            Active_Page := Page_Check_List;
      
            Display.Refresh := True;
         
            return;
        
         elsif Btn_System.Contains (X, Y) then
         
            Active_Page := Page_System;
        
            Display.Refresh := True;
         
            return;
        
         end if;
          
      elsif Btn_Exit.Contains (X, Y) then
      
         if Active_Page = Page_Glider then
            
            Flight.Aircraft.Calculate_Gliding_States;
            
         end if;
                  
         Active_Page := Page_Navigation;
        
         Flight.Plan.Jump_In_Proximity := True;
      
         Display.Refresh := True;
         
         return;
        
      end if;
      
      --------------------------------------------------------------------------	
      -- Redirect event to other pages
      --------------------------------------------------------------------------
      
      case Active_Page is
         
         when Page_Navigation =>
            
            Display.Pages.Navigation.Screen_Pressed (X, Y);
        
         when Page_Strips =>
        
            Display.Pages.Strips.Screen_Pressed (X, Y);
         
         when Page_Route_Edition =>
            
            Display.Pages.Route_Edition.Screen_Pressed (X, Y);
                       
         when Page_Glider =>
            
            Display.Pages.Glider.Screen_Pressed (X, Y);
            
         when Page_Map =>
            
            Display.Pages.Map.Screen_Pressed (X, Y);
            
         when Page_Wind =>
            
            Display.Pages.Wind.Screen_Pressed (X, Y);
            
         when Page_System =>
            
            Display.Pages.System.Screen_Pressed (X, Y);
            
         when Page_Check_List =>
            
            Display.Pages.Check.Screen_Pressed (X, Y);
            
         when others =>
            
            null;
            
      end case;
                  
   end Screen_Pressed;
   -----------------------------------------------------------------------------

   
   
   
   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
      
      use Widgets.Dialog;
      
   begin
      
      -- Dialog
      ----------------------------------------------
      
      if Widgets.Dialog.Is_Open then
         
         case Key is
            
            when Panel_Wheel_Left =>
               
               Widgets.Dialog.Preselect_Action (Dialog_Ok);
         
               Refresh := True;
               
            when Panel_Wheel_Right =>
               
               Widgets.Dialog.Preselect_Action (Dialog_Cancel);
         
               Refresh := True;
               
            when Panel_Wheel_Button =>
               
               Widgets.Dialog.Expedite_Action;
         
               Refresh := True;
               
            when others => null;
               
         end case;
         
         return;
         
      end if;
      
      -- Return to navigation with left button
      ----------------------------------------------
      
      if Active_Page /= Page_Navigation and Key = Panel_Button_Left then
         
         Active_Page := Page_Navigation;
         
         Display.Refresh := True;
         
         return;
         
      end if;
      
      -- Page cases
      ----------------------------------------------
      
      case Active_Page is
         
         when Page_Navigation =>
            
            Pages.Navigation.Key_Changed (Key);
            
         when Page_Strips =>
            
            Pages.Strips.Key_Changed (Key);
            
         when Page_Route_Edition =>
            
            Pages.Route_Edition.Key_Changed (Key);
            
         when Page_Glider =>
            
            Pages.Glider.Key_Changed (Key);
            
         when Page_Map =>
            
            Pages.Map.Key_Changed (Key);
            
         when Page_Check_List =>
            
            Pages.Check.Key_Changed (Key);
            
         when Page_System =>
            
            Pages.System.Key_Changed (Key);
            
         when Page_Wind =>
            
            Pages.Wind.Key_Changed (Key);
            
         when others =>
            
            null;
            
      end case;
            
   end Key_Changed;
   -----------------------------------------------------------------------------

end Display.Menu;
--------------------------------------------------------------------------------
