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
with Display.Flight_Panel;
with Display.Compass;
with Gl.Fonts;
with Flight.Plan;
use  Flight.Plan;
with Utility.Colors;
use  Utility.Colors;
with Utility.Strings;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Strips is

   -- Fonts
   ---------------------------------
   Font_1 : Gl.Fonts.Font_Style_Record := (Width     => 0.014, 
                                           Height    => 0.036, 
                                           Space     => 0.006,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular,
                                           Line_R    => 0.8,
                                           Line_G    => 0.8,
                                           Line_B    => 0.8,
                                           Glow_R    => 0.1,
                                           Glow_G    => 0.1,
                                           Glow_B    => 0.1);
  
   -- Font for the active waypoint
   Font_2 : Gl.Fonts.Font_Style_Record := Font_1;
      
   -- Font for the home waypoint
   Font_3 : Gl.Fonts.Font_Style_Record := Font_1;
      
   -- Font for the leg info
   Font_4 : Gl.Fonts.Font_Style_Record := Font_1;
      
   -- Font for the units
   Font_5 : Gl.Fonts.Font_Style_Record := Font_1;
      
   -- Font for status
   Font_6 : Gl.Fonts.Font_Style_Record := Font_1;
      
   -- Font for status
   Font_7 : Gl.Fonts.Font_Style_Record := Font_1;
      
   -- Font for the elevation of the waypoint
   Font_8 : Gl.Fonts.Font_Style_Record := Font_1; 
    
   -- Font for above terrain
   Font_A : Gl.Fonts.Font_Style_Record := Font_1;
      
   -- Font for below terrain
   Font_B : Gl.Fonts.Font_Style_Record := Font_1;
   
   --///////////////////////////////////////////////////////////////////////////
   -- Actions
   --///////////////////////////////////////////////////////////////////////////
   
   Btn_Name   : Button_Record;
   
   Btn_Up     : Button_Record;
   
   Btn_Down   : Button_Record;
  
   Btn_Back   : Button_Record;
   
   --///////////////////////////////////////////////////////////////////////////
   -- Task strips
   --///////////////////////////////////////////////////////////////////////////
   
   Frm_Waypoint : array (Waypoint_Range) of Widget_Record;
   
   Frm_Task     : Widget_Record;
      
   Frm_Progress : Widget_Record;
      
   Frm_Bar      : Widget_Record;
   
   First_Index  : Waypoint_Range := Waypoint_Range'First;
   
   --===========================================================================
   --
   --===========================================================================
   procedure Init is
      
      use Utility.Strings;
      
      M : constant Dimension_Float := 0.01;
      H : constant Dimension_Float := 0.06;
      W : constant Dimension_Float := 0.22;
      
      Allocation : Allocation_Record;
      
   begin

      -- Fonts
      -------------------------------------------------
                        
      -- Active waypoint font
      -----------------------
      Font_2.Line_R := 1.0;
      Font_2.Line_G := 0.4;
      Font_2.Line_B := 1.0;      
      Font_2.Glow_R := 0.4;
      Font_2.Glow_G := 0.0;
      Font_2.Glow_B := 0.4;
      
      -- Home waypoint font
      -----------------------
      Font_3.Line_R := 0.0;
      Font_3.Line_G := 0.8;
      Font_3.Line_B := 0.0;      
      Font_3.Glow_R := 0.0;
      Font_3.Glow_G := 0.3;
      Font_3.Glow_B := 0.0;
      
      -- Leg font
      -----------------------
      Font_4.Height := 0.8 * Font_1.Height;      
      Font_4.Width  := 0.8 * Font_1.Width;
      Font_4.Line_R := 0.6;
      Font_4.Line_G := 0.6;
      Font_4.Line_B := 1.0;      
      Font_4.Glow_R := 0.1;
      Font_4.Glow_G := 0.1;
      Font_4.Glow_B := 0.1;
            
      -- Units font
      -----------------------
      Font_5.Height := 0.6 * Font_1.Height;      
      Font_5.Width  := 0.6 * Font_1.Width;
      Font_5.Line_R := 0.5;
      Font_5.Line_G := 0.5;
      Font_5.Line_B := 0.5;      
      Font_5.Glow_R := 0.1;
      Font_5.Glow_G := 0.1;
      Font_5.Glow_B := 0.1;
       
      -- Units font
      -----------------------
      Font_6.Line_R := 0.5;
      Font_6.Line_G := 0.5;
      Font_6.Line_B := 0.5;      
      Font_6.Glow_R := 0.1;
      Font_6.Glow_G := 0.1;
      Font_6.Glow_B := 0.1;
      
      -- Home waypoint font
      -----------------------
      Font_7.Line_R := 0.4;
      Font_7.Line_G := 0.4;
      Font_7.Line_B := 0.4;      
      Font_7.Glow_R := 0.1;
      Font_7.Glow_G := 0.1;
      Font_7.Glow_B := 0.1;
      
      -- Ground elevation
      -----------------------
      Font_8.Line_R := 0.7;
      Font_8.Line_G := 0.5;
      Font_8.Line_B := 0.0;      
      Font_8.Glow_R := 0.1;
      Font_8.Glow_G := 0.1;
      Font_8.Glow_B := 0.1;
      
      -- Above terrain
      -----------------------
      Font_A.Line_R := 0.1;
      Font_A.Line_G := 0.8;
      Font_A.Line_B := 0.1;      
      Font_A.Glow_R := 0.1;
      Font_A.Glow_G := 0.1;
      Font_A.Glow_B := 0.1;
      
      -- Below terrain
      -----------------------
      Font_B.Line_R := 0.8;
      Font_B.Line_G := 0.1;
      Font_B.Line_B := 0.1;      
      Font_B.Glow_R := 0.1;
      Font_B.Glow_G := 0.1;
      Font_B.Glow_B := 0.1;
      
      -- Flight plan name
      -------------------------------------------------
      
      Allocation.X := 0.17;
      
      Allocation.Y := 0.89;
      
      Allocation.W := 0.27;
      
      Allocation.H := 0.10;
      
      Btn_Name.Set_Allocation (Allocation);
      
      Btn_Name.Set_Label (Trim (Flight_Plan.Name));
      
      Btn_Name.Set_Background_Color (Color_Gray_4);
      
      Btn_Name.Set_Border_Color (Color_Black);      
      
      Btn_Name.Set_Label_Color (Color_White, Color_Gray_3);      
      
      Btn_Name.Set_Font_Size (0.4, 0.3);
        
      -- Change active waypoint one position upwards
      -------------------------------------------------
      
      Allocation.X := Allocation.X + Allocation.W + M;
      
      Allocation.W := 0.10;
      
      Btn_Up.Set_Allocation (Allocation);
      
      Btn_Up.Set_Label ("}");
      
      Btn_Up.Set_Background_Color (Color_Gray_4);
      
      Btn_Up.Set_Border_Color (Color_Black);      
      
      Btn_Up.Set_Label_Color (Color_White, Color_Gray_3);      
      
      Btn_Up.Set_Font_Size (0.6, 0.5);
      
      -- Change active waypoint one position downwards
      -------------------------------------------------
      
      Allocation.X := Allocation.X + Allocation.W + M;
      
      Btn_Down.Set_Allocation (Allocation);
      
      Btn_Down.Set_Label ("{");
      
      Btn_Down.Set_Background_Color (Color_Gray_4);
      
      Btn_Down.Set_Border_Color (Color_Black);      
      
      Btn_Down.Set_Label_Color (Color_White, Color_Gray_3);      
      
      Btn_Down.Set_Font_Size (0.6, 0.5);
    
      -- Back route button
      -------------------------------------------------
      
      Allocation.X := Allocation.X + Allocation.W + M;
      
      Btn_Back.Set_Allocation (Allocation);
      
      Btn_Back.Set_Label ("BACK");
      
      Btn_Back.Set_Background_Color (Color_Gray_1);
      
      Btn_Back.Set_Border_Color (Color_Black);      
      
      Btn_Back.Set_Label_Color (Color_White, Color_Gray_3);      
      
      Btn_Back.Set_Font_Size (0.4, 0.3);
      
      -- Frames for each waypoint/task pair
      -------------------------------------------------
      
      Allocation.X := 0.06;
      
      Allocation.W := 0.70;
      
      Allocation.H := 2.0 * Font_1.Height;
      
      for I in Waypoint_Range loop
         
         Frm_Waypoint (I).Set_Allocation (Allocation);
      
         Frm_Waypoint (I).Set_Border_Color (Color_Magenta);
      
         Frm_Waypoint (I).Set_Background_Color (Color_Gray_5);
         
      end loop;
      
      Allocation.W := 0.25;
      
      Allocation.H := 2.5 * Font_4.Height;
      
      Frm_Task.Set_Allocation (Allocation);
      
      Frm_Task.Set_Background_Color (Color_Sky);
      
      Frm_Task.Set_Border_Color (Color_Magenta);
      
      Frm_Progress.Set_Background_Color (Color_Red);
      
      Frm_Progress.Set_Border_Color (Color_Magenta);
             
      -- Flight data panel
      ------------------------------------------------------  
      
      Display.Flight_Panel.Init;
     
   end Init;
   -----------------------------------------------------------------------------
   
        
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is 
      
      use Utility.Strings;
      use Gl.Fonts;

      -- Allocation variables
      M : Float := 0.040;
      Y : Float := 0.820;
      X : Float := 0.000;
      A : Allocation_Record;
      K : Natural := 0;
      Is_Active   : Boolean := False;
      Is_Previous : Boolean := False;
      Is_Home     : Boolean := False;
         
      -- Font for the name of a waypoiny
      Font_N : Gl.Fonts.Font_Style_Record;
            
      -- Font for the elevation of a waypoint
      Font_E : Gl.Fonts.Font_Style_Record;
          
      -- Font for distance and bearing
      Font_D : Gl.Fonts.Font_Style_Record;
    
      -- Font for the altitude margin
      Font_M : Gl.Fonts.Font_Style_Record;
      
      use Maps;
      
   begin
       
      for I in Waypoint_Range loop
            	   
         exit when not Flight_Plan.Waypoints(I).Is_Loaded;
         
         if Flight_Plan.Waypoints(I).Is_Active then
         
            if Flight_Plan.Go_Back then
               
               if I < First_Index then
              
                  First_Index := I;
               
               elsif I > First_Index + 5 then
            
                  First_Index := I - 5;
               
               end if;
               
            else
               
               if I < First_Index then
              
                  First_Index := I;
               
               elsif I > First_Index + 5 then
            
                  First_Index := I - 5;
               
               end if;
               
            end if;
            
            exit;
                      
         end if;
               
      end loop;
                 
      -- Flight panel
      -----------------------
      Display.Flight_Panel.Draw (Width, Height);
      
      -- Buttons
      -----------------------

      Btn_Name.Set_Label (Trim (Flight_Plan.Name));
      
      Btn_Name.Draw;

      Btn_Down.Draw;
      
      Btn_Up.Draw;
      
      Btn_Back.Draw;
      
      --Table header
      -----------------------
      Y := Btn_Name.Get_Allocation.Y - 0.025;
     
      for I in Waypoint_Range loop
            	
         exit when not Flight_Plan.Waypoints (I).Is_Loaded;
         
         if I >= First_Index then
         
            K := K + 1;
            
            Y := Y - (Font_1.Height + M);
            
            Is_Active   := Flight_Plan.Waypoints (I).Is_Active;
            
            Is_Previous := (not Flight_Plan.Go_Back and then I < Waypoint_Range'Last  and then Flight_Plan.Waypoints (I + 1).Is_Active) or
                           (    Flight_Plan.Go_Back and then I > Waypoint_Range'First and then Flight_Plan.Waypoints (I - 1).Is_Active);
                          
            Is_Home     := Is_Active and I = Waypoint_Range'First;
            
            -- Select font according to status
            ----------------------------------------------
            if Is_Active then
               
               Font_N := Font_2;
            
               Font_D := Font_2;
               
               Font_E := Font_8;
            
            elsif Is_Previous then
            
               Font_N := Font_1;
            
               Font_D := Font_1;
               
               Font_E := Font_8;
            
            else
            
               Font_N := Font_7;
            
               Font_D := Font_7;
               
               Font_E := Font_7;
               
            end if;
            
            A := Frm_Waypoint (I).Get_Allocation;
            
            A.Y := Y - 0.50 * Font_1.Height;
            
            Frm_Waypoint (I).Set_Allocation (A);
            
            if Is_Active then
            
               Frm_Waypoint (I).Set_Border_Color (Color_Magenta);
      
               Frm_Waypoint (I).Set_Background_Color (Color_White);
               
            elsif Is_Previous then
                              
               Frm_Waypoint (I).Set_Border_Color (Color_Gray_5);
      
               Frm_Waypoint (I).Set_Background_Color (Color_White);
               
            else
                                
               Frm_Waypoint (I).Set_Border_Color (Color_Gray_2);
      
               Frm_Waypoint (I).Set_Background_Color (Color_Gray_1);
               
            end if;
                    
            Frm_Waypoint (I).Draw;
            
            X := Frm_Waypoint (I).Get_Allocation.X;
            
            Gl.Fonts.Draw (Trim (Waypoint_Range'Image (I)),
                           0.03, 
                           Y,
                           Font_N,
                           Alignment_LC);
             
            -- Blinking name when in proximity
            ----------------------------------------------
            if not Flight_Plan.Waypoints (I).In_Proximity or else Blink then
           
               -- Waypoint name
               ----------------------------------------------
               Gl.Fonts.Draw (Trim (Flight_Plan.Waypoints (I).Name),
                              X + 0.02, 
                              Y,
                              Font_N,
                              Alignment_LL);
                      
            end if;
         
            -- Waypoint distance
            ----------------------------------------------	 	
            Gl.Fonts.Draw (Float_Image (Flight_Plan.Waypoints (I).Distance, 1),
                           X + 0.25, 
                           Y,
                           Font_D,
                           Alignment_LR);
         
            Gl.Fonts.Draw ("KM",
                           X + 0.26, 
                           Y,
                           Font_5,
                           Alignment_LL);
             
            -- Waypoint bearing
            ----------------------------------------------	 	
            Gl.Fonts.Draw (Float_Image (Flight_Plan.Waypoints (I).Bearing, 0),
                           X + 0.37, 
                           Y,
                           Font_D,
                           Alignment_LR);
         
            Gl.Fonts.Draw ("*",
                           X + 0.38, 
                           Y + 0.01,
                           Font_5,
                           Alignment_LL);
         
            -- Waypoint elevation
            ----------------------------------------------
            Gl.Fonts.Draw (Float_Image (Flight_Plan.Waypoints (I).Elevation, 0),
                           X + 0.51, 
                           Y,
                           Font_E,
                           Alignment_LR);
     
            Gl.Fonts.Draw ("M",
                           X + 0.52,
                           Y,
                           Font_5,
                           Alignment_LL);
             
            -- Waypoint altitude margin
            ----------------------------------------------
            
            if Is_Active or Is_Previous then
              
               if Flight_Plan.Waypoints (I).Margin < 0.0 or Flight_Plan.Waypoints (I).Margin = No_Altitude then
               
                  -- Below ground or not accessible (N.A.);
                  Font_M := Font_B;
                  
               else
                  
                  -- Above ground
                  Font_M := Font_A;
                  
               end if;
                              
            else
                   
               -- Inactive
               Font_M := Font_7;
                  
            end if;
            
            Gl.Fonts.Draw (Flight_Plan.Waypoints (I).Get_Margin_Image,
                           X + 0.65, 
                           Y,
                           Font_M,
                           Alignment_LR);
            
            Gl.Fonts.Draw ("M",
                           X + 0.66,
                           Y,
                           Font_5,
                           Alignment_LL);
            
            -- Exit when reaching the bottom
            ----------------------------------------------
            exit when K = 6;
              
            -- Leg info to the next waypoint
            ----------------------------------------------
            if Flight_Plan.Tasks (I).Is_Loaded then
            
               Y := Y - (Font_4.Height + M);
              
               if Flight_Plan.Tasks (I).Is_Active then
                  
                  -- Task highlight
                  --------------------------
                     
                  A := Frm_Task.Get_Allocation;
                  
                  A.Y := Y - 0.75 * Font_4.Height;
                  
                  Frm_Task.Set_Allocation (A);
                  
                  --Frm_Task.Draw;
                  
                  -- Progress bar
                  --------------------------
                     
                  A.H := 0.5 * A.H;
                     
                  A.X := A.X + A.W;
                     
                  A.W := Float'Max (0.0, Frm_Waypoint (I).Get_Allocation.X + Frm_Waypoint (I).Get_Allocation.W - A.X);
                     
                  A.W := Flight_Plan.Tasks (I).Progress * A.W;
                     
                  Frm_Progress.Set_Allocation (A);
                     
                  --Frm_Progress.Draw;
                  
               end if;
                          
               if Flight_Plan.Tasks (I).Is_Active then
                  
                  if Flight_Plan.Go_Back then
                     
                     Gl.Fonts.Draw ("}",
                                    0.12,
                                    Y,
                                    Font_1,
                                    Alignment_LR);
                     
                  else
                     
                     Gl.Fonts.Draw ("{",
                                    0.12,
                                    Y,
                                    Font_1,
                                    Alignment_LR);
                     
                  end if;
                  
               end if;
                                
               Gl.Fonts.Draw (Float_Image (Flight_Plan.Tasks (I).Length, 0),
                              0.17,
                              Y,
                              Font_4,
                              Alignment_LR);
         
               Gl.Fonts.Draw ("KM",
                              0.18, 
                              Y,
                              Font_5,
                              Alignment_LL);
               
               Gl.Fonts.Draw (Float_Image (Flight_Plan.Tasks (I).Course, 0),
                              0.27,
                              Y,
                              Font_4,
                              Alignment_LR);
         
               Gl.Fonts.Draw ("*",
                              0.28, 
                              Y,
                              Font_5,
                              Alignment_LL);
               
            end if;
            
         end if;
                     
      end loop;
            
   end Draw;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin
   
      -- Normal mode
      --------------------------------------------------------------------------             
      Display.Flight_Panel.Screen_Pressed (X, Y);
      
      if Btn_Up.Contains (X, Y) then
         
         Flight.Plan.Goto_Previous_Waypoint;
         
         Flight.Plan.Update_Flight_Plan;
         
         Refresh := True;
         
      elsif Btn_Down.Contains (X, Y) then
         
         Flight.Plan.Goto_Next_Waypoint;
         
         Flight.Plan.Update_Flight_Plan;
         
         Refresh := True;
         
      elsif Btn_Back.Contains (X, Y) then
         
         Flight.Plan.Toggle_Go_Back;
         
         if Flight.Plan.Flight_Plan.Go_Back then
            
            Btn_Back.Set_Background_Color (Color_Orange);
            
         else
            
            Btn_Back.Set_Background_Color (Color_Gray_1);
            
         end if;
              
         Refresh := True;
             
      elsif Btn_Name.Contains (X, Y) then
             
         null;
         
      end if;
            
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Handles a panel key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin
      
      -- Normal mode
      --------------------------------------------------------------------------
      case Key is
         
         when Panel_Wheel_Left =>
            
            Flight.Plan.Goto_Previous_Waypoint;
         
            Flight.Plan.Update_Flight_Plan;
         
            Refresh := True;
            
         when Panel_Wheel_Right =>
            
            Flight.Plan.Goto_Next_Waypoint;
         
            Flight.Plan.Update_Flight_Plan;
         
            Refresh := True;
            
         when Panel_Wheel_Button =>
            
            null;
            
         when Panel_Button_Right =>
            
            null;
                        
         when Panel_Button_Left =>
              
            null;
            
         when others =>
            
            null;
            
      end case;
                   
   end Key_Changed;
   -----------------------------------------------------------------------------
   
end Display.Pages.Strips;
--------------------------------------------------------------------------------
