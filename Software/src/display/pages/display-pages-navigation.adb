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
with Ada.Calendar;
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;
-- Gnav
with Display.Compass;
with Display.Panels.Gauges;
with Flight;
with Flight.Aircraft;
with Flight.Plan;
with Flight.Representation;
with Flight.Signal;
with Flight.Stream;
with Gl;
use  Gl;
with Gl.Colors;
use  Gl.Colors;
with Gl.Fonts;
with Gl.Resources;
with Gl.Shaders;
with Math.Vector2;
with Maps;
use  Maps;
with Maps.Terrain;
with Maps.Layers;
with Maps.Layers.Airspaces;
with Maps.Loader;
with Maps.Reference;
with Timing.Events;
with Utility.Strings;
with Utility.Units;
use  Utility.Units;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Navigation is
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The map view
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   View : Maps.Map_View_Record;

   -- Main menu buttons
   ---------------------------------
   
   Btn_Zoom_In   : Button_Record;
     
   Btn_Zoom_Out  : Button_Record;
   
   Btn_Left      : Button_Record;
   
   Btn_Right     : Button_Record;
   
   Btn_Down      : Button_Record;
   
   Btn_Up        : Button_Record;
       
   Btn_Move       : Button_Record;
    
   -- Range function buttons
   ---------------------------------
   
   Btn_Range      : Button_Record;
      
   Btn_Range_Mode : Button_Record;
      
   Btn_Cone_Mode  : Button_Record;
        
   -- Mac Cready adjustment buttons
   ---------------------------------
   
   Btn_Ascent_Plus  : Button_Record; -- Inter thermal sink/lift (+)
     
   Btn_Ascent_Min   : Button_Record; -- Inter thermal sink/lift (-)
    
   -- Status variables
   ---------------------------------
   
   Auto_Center   : Boolean := True;
   
   Show_Move     : Boolean := False;
   
   Frame_SW      : Position_Record;
      
   Frame_NE      : Position_Record;
   
   Altitude_Unit : Altitude_Units := Unit_Meter;
   
   Distance_Unit : Distance_Units := Unit_Kilometer;
   
   Velocity_Unit : Velocity_Units := Unit_Kilometer_Hour;
   
   Gnss_Blink_Count : Natural := 0;
   
   -- Embedded water edition tools
   ---------------------------------
   
   type Edit_Types is (Off, Waterize, Dewaterize, Rewaterize);
      
   Edit_Mode : Boolean := False;
     
   Edit_Type : Edit_Types := Dewaterize;
    
   Edit_Size : Natural := 10;
   
   Btn_Water : Button_Record;
       
   Btn_Size  : Button_Record;
      
   Btn_Save  : Button_Record;
  
   -- Fonts
   ---------------------------------
   Font_1 : Gl.Fonts.Font_Style_Record := (Width     => 0.010, 
                                           Height    => 0.033, 
                                           Space     => 0.004,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular);
      
   -- Fonts
   ---------------------------------
   Font_2 : Gl.Fonts.Font_Style_Record := (Width     => 0.010, 
                                           Height    => 0.033, 
                                           Space     => 0.004,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular);
         
   -- Fonts
   ---------------------------------
   Font_3 : Gl.Fonts.Font_Style_Record := (Width     => 0.012, 
                                           Height    => 0.035, 
                                           Space     => 0.008,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular);
            
   -- Fonts
   ---------------------------------
   Font_4 : Gl.Fonts.Font_Style_Record := (Width     => 0.030, 
                                           Height    => 0.035, 
                                           Space     => 0.008,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular);
                
   -- Fonts
   ---------------------------------
   Font_5 : Gl.Fonts.Font_Style_Record := (Width     => 0.012, 
                                           Height    => 0.035, 
                                           Space     => 0.008,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular);
            
   -- Wind rosetta
   ---------------------------------
   Wind_Arrow_Id : Gl_Uint;
     
   --===========================================================================
   -- Loads the wind rosetta buffer
   --===========================================================================
   procedure Load_Wind_Arrow is
      
      Arrow_Buffer  : Gl.Gl_Float_Vec (1 .. 8);

   begin

      Arrow_Buffer (1) := 0.25;
      Arrow_Buffer (2) := 0.00;

      Arrow_Buffer (3) :=-0.08;
      Arrow_Buffer (4) := 0.12;

      Arrow_Buffer (5) := 0.00;
      Arrow_Buffer (6) := 0.00;

      Arrow_Buffer (7) :=-0.08;
      Arrow_Buffer (8) :=-0.12;

      Gl.Resources.Update_Resource (Wind_Arrow_Id, Arrow_Buffer'Unrestricted_Access);

   end Load_Wind_Arrow;
   -----------------------------------------------------------------------------
        



   --===========================================================================
   -- Draw wind
   --===========================================================================
   procedure Draw_Wind is
    
      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;
      use Gl.Fonts;
         
      X      : Float := 0.25;
      
      Y      : Float := 0.88;
      
      Size   : Float := 0.18;
      
      Aspect : Float := Width / Height;
      
      M1     : Gl.Gl_Mat_4 := Gl.Shaders.Get_Active_Matrix;

      M2     : Gl.Gl_Mat_4 := Gl.Gl_Mat_4_Identity;

      Angle  : Float;

      Wind   : Float := Convert (Float (Flight.Data.Wind.Norm2), 
                                 Unit_Meter_Second,
                                 Unit_Kilometer_Hour);
      
   begin

      if Flight.Data.Is_Valid (Flight.Field_Wind) and then Wind > 0.0 then
         
         Angle := Float (Flight.Data.Wind.Orientation * 180.0 / Pi);

         if Angle < 0.0 then

            Angle := 360.0 + Angle;

         end if;

         Angle := Angle * Pi / 180.0;

         M2 := Gl_Mat_4_Identity;

         Translate (M2, X, Y, 0.0);

         Scale     (M2, Size, Size * Aspect, 1.0);

         Rotate    (M2, Angle);

         M2 := Multiply  (M1, M2);

         Gl.Shaders.Load_Matrix (M2);

         Gl.Bind_Buffer (GL_ARRAY_BUFFER, Wind_Arrow_Id);

         Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

         Gl.Shaders.Bind_Shader (Gl.Shaders.Monochrome_2D);

         Gl.Shaders.Load_Color (0.1, 0.1, 0.4, 1.0);

         Gl.Draw_Arrays (GL_TRIANGLE_FAN, 0, 4);

         Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

         Gl.Shaders.Load_Color (0.2, 0.2, 0.9, 1.0);

         Gl.Shaders.Load_Width (1.0);

         Gl.Draw_Arrays (GL_LINE_LOOP, 0, 4);
      
         Gl.Shaders.Load_Matrix (M1);

         Gl.Fonts.Draw (Text      => Utility.Strings.Float_Image (Wind, 0),
                        X         => X - 0.04 * Cos (Angle),
                        Y         => Y - 0.04 * Sin (Angle) * Aspect,
                        Style     => Font_5,
                        Color     => Line_Cyan,
                        Alignment => Gl.Fonts.Alignment_CC);
         
      else
         
         Y := 0.95;
         
         Gl.Fonts.Draw (Text      => "NO WIND",
                        X         => X,
                        Y         => Y,
                        Style     => Font_5,
                        Color     => Line_Cyan,
                        Alignment => Gl.Fonts.Alignment_TC);
                  
      end if;  
           
   end Draw_Wind;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Center_Frame is
   begin
      
      Frame_SW.Lat := View.Center.Lat - Long_Float (0.1 * View.Height * View.Zoom);
      
      Frame_SW.Lon := View.Center.Lon - Long_Float (0.1 * View.Width  * View.Zoom);
      
      Frame_NE.Lat := View.Center.Lat + Long_Float (0.1 * View.Height * View.Zoom);
      
      Frame_NE.Lon := View.Center.Lon + Long_Float (0.1 * View.Width  * View.Zoom);
      
   end Center_Frame;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Update_View is
      
      use Math.Vector2;
      
   begin
      
      -- Update center of screen
      -------------------------------------------------
      
      if Auto_Center then
         
         if 
           Flight.Data.Position.Lat < Frame_SW.Lat or else
           Flight.Data.Position.Lon < Frame_SW.Lon or else
           Flight.Data.Position.Lat > Frame_NE.Lat or else
           Flight.Data.Position.Lon > Frame_NE.Lon
         then
            
            View.Center := Flight.Data.Position;
            
            Center_Frame;
            
         end if;
                  
      end if;
      
      -- Update cone
      -------------------------------------------------
      
      --if View.Cone_Active and Flight.Aircraft.Cone_Changed then
         
      --   Maps.Terrain.Force_Reload;
         
      --end if;
      
   end Update_View;
   -----------------------------------------------------------------------------	
      
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Init is
        
      use Utility.Strings;
      use Widgets.Panel;
      
      M : Dimension_Float := 0.01;
      H : Dimension_Float := 0.08;
      W : Dimension_Float := 0.05;
      V : Dimension_Float := 0.04;
      
      Allocation : Allocation_Record;
      
   begin

      for I in 1..Ada.Command_Line.Argument_Count loop
         
         if Ada.Command_Line.Argument (I) = "EDIT_WATER" then
      
            Edit_Mode := True;
            
         end if;
         
      end loop;
            
      M := 0.01;
      H := 0.12;
      W := 0.08;
      
      -- Down button
      ------------------------------------------------------
      
      Allocation.H := H;      
      Allocation.W := W;             
      Allocation.X := 0.4 + 0.5 * W;      
      Allocation.Y := M;
      
      Btn_Down.Set_Label ("S");
      
      Btn_Down.Set_Allocation (Allocation);
      
      Btn_Down.Set_Label_Color (Color_White);
      
      Btn_Down.Set_Transparency (0.3);
      
      Btn_Down.Set_Font_Size (0.5, 0.4);
                   
      -- Up button
      ------------------------------------------------------
      
      Allocation.Y := 1.0 - H - M;
      
      Btn_Up.Set_Label ("N");
      
      Btn_Up.Set_Allocation (Allocation);
      
      Btn_Up.Set_Label_Color (Color_White);
      
      Btn_Up.Set_Transparency (0.3);
      
      Btn_Up.Set_Font_Size (0.5, 0.4);
                             
      -- Left button
      ------------------------------------------------------
      
      Allocation.X := Allocation.X - 0.22 - 0.5 * W;      
      Allocation.Y := 0.5 - 0.5 * H;
               
      Btn_Left.Set_Label ("W");
      
      Btn_Left.Set_Allocation (Allocation);
      
      Btn_Left.Set_Label_Color (Color_White);
      
      Btn_Left.Set_Transparency (0.3);
      
      Btn_Left.Set_Font_Size (0.5, 0.4);
                        
      -- Right button
      ------------------------------------------------------
      
      Allocation.X := Allocation.X + 0.42 + W;      
      Allocation.Y := 0.5 - 0.5 * H;
      
      Btn_Right.Set_Label ("E");
      
      Btn_Right.Set_Allocation (Allocation);
      
      Btn_Right.Set_Label_Color (Color_White);
      
      Btn_Right.Set_Transparency (0.3);
      
      Btn_Right.Set_Font_Size (0.5, 0.4);
             
      -- Zoom out button
      ------------------------------------------------------   
      
      Allocation.X := M;      
      Allocation.H := H;      
      Allocation.Y := M;      
      Allocation.W := W;
           
      Btn_Zoom_Out.Set_Label ("-");
      
      Btn_Zoom_Out.Set_Allocation (Allocation);
      
      Btn_Zoom_Out.Set_Label_Color (Color_White);
      
      Btn_Zoom_Out.Set_Font_Size (0.6);
                       
      -- Zoom in button
      ------------------------------------------------------
      
      Btn_Zoom_In.Set_Label ("+");
      
      Allocation.Y := H + 2.0 * M;
      
      Btn_Zoom_In.Set_Allocation (Allocation);
      
      Btn_Zoom_In.Set_Label_Color (Color_White);
      
      Btn_Zoom_In.Set_Font_Size (0.5);
                       
      -- Flight data panel
      ------------------------------------------------------  
      
      Display.Panels.Gauges.Init;
      
      -- Range button
      ------------------------------------------------------  
      
      M := 0.01;
      H := (1.0 - 9.0 * M) / 8.0;
      W := 0.15;
                      
      -- Range mode button
      ------------------------------------------------------  
      
      Allocation.X := 2.0 * M + W;      
      Allocation.H := 0.7 * H;      
      Allocation.Y := M;      
      Allocation.W := 0.8 * W;
           
      Btn_Range_Mode.Set_Label ("---");
      
      Btn_Range_Mode.Set_Allocation (Allocation);
      
      Btn_Range_Mode.Set_Label_Color (Color_Red);
      
      Btn_Range_Mode.Set_Font_Size (0.4, 0.3, 0.5);
                             
      Btn_Range_Mode.Set_Background_Color (Color_Black.With_Alpha (0.5));
               
      Allocation.X := 3.0 * M + 1.8 * W;      
      Allocation.H := 0.7 * H;      
      Allocation.Y := M;      
      Allocation.W := 0.8 * W;
           
      Btn_Cone_Mode.Set_Label ("---");
      
      Btn_Cone_Mode.Set_Allocation (Allocation);
      
      Btn_Cone_Mode.Set_Label_Color (Color_Red);
      
      Btn_Cone_Mode.Set_Font_Size (0.4, 0.3, 0.5);
                             
      Btn_Cone_Mode.Set_Background_Color (Color_Black.With_Alpha (0.5));
             
      -- Mac Cready setup buttons
      ------------------------------------------------------  
          
      Allocation.X := 0.7;      
      Allocation.H := 0.7 * H;      
      Allocation.Y := M;      
      Allocation.W := 0.4 * W;
           
      Btn_Ascent_Min.Set_Label ("-");
      
      Btn_Ascent_Min.Set_Allocation (Allocation);
      
      Btn_Ascent_Min.Set_Label_Color (Color_White);
      
      Btn_Ascent_Min.Set_Font_Size (0.6, 0.6, 0.5);
                             
      Btn_Ascent_Min.Set_Background_Color (Color_Black.With_Alpha (0.5));
     
      Allocation.Y := 3.0 * M + 1.4 * H;      
           
      Btn_Ascent_Plus.Set_Label ("+");
      
      Btn_Ascent_Plus.Set_Allocation (Allocation);
      
      Btn_Ascent_Plus.Set_Label_Color (Color_White);
      
      Btn_Ascent_Plus.Set_Font_Size (0.6, 0.6, 0.5);
                             
      Btn_Ascent_Plus.Set_Background_Color (Color_Black.With_Alpha (0.5));

      -- Range button
      ------------------------------------------------------  
      
      Allocation.X := M;      
      Allocation.H := H;      
      Allocation.Y := M;      
      Allocation.W := W;
           
      Btn_Range.Set_Label ("RANGE");
      
      Btn_Range.Set_Allocation (Allocation);
      
      Btn_Range.Set_Label_Color (Color_White);
      
      Btn_Range.Set_Font_Size (0.3, 0.3, 0.5);
                             
      Btn_Range.Set_Background_Color (Color_Black.With_Alpha (0.5));

      -- Move button
      ------------------------------------------------------
      
      Allocation.Y := Allocation.Y + H + M;
      
      Btn_Move.Set_Label ("MOVE");
      
      Btn_Move.Set_Allocation (Allocation);
      
      Btn_Move.Set_Label_Color (Color_White);
      
      Btn_Move.Set_Font_Size (0.3, 0.3, 0.5);
                      
      Btn_Move.Set_Background_Color (Color_Black.With_Alpha (0.5));
      
      -- Terrain data
      ------------------------------------------------------
     
      if Edit_Mode then
               
         Allocation.X := M;      
         Allocation.H := H;      
         Allocation.Y := 0.80;      
         Allocation.W := 0.1;
         
         Btn_Water.Set_Label (Edit_Types'Image (Edit_Type));
           
         Btn_Water.Set_Allocation (Allocation);
           
         Btn_Water.Set_Label_Color (Color_White);
      
         Btn_Water.Set_Background_Color ((0.1, 0.1, 0.8, 0.5));
      
         Btn_Water.Set_Font_Size (0.2, 0.4, 0.02);
                   
         Allocation.Y := 0.69;      
         
         Btn_Size.Set_Label (Integer_Image (Edit_Size));
           
         Btn_Size.Set_Allocation (Allocation);
           
         Btn_Size.Set_Label_Color (Color_White);
      
         Btn_Size.Set_Background_Color ((0.1, 0.1, 0.8, 0.5));
      
         Btn_Size.Set_Font_Size (0.4, 0.5, 0.01);
                     
         Allocation.Y := 0.58;      
         
         Btn_Save.Set_Label ("SAVE");
           
         Btn_Save.Set_Allocation (Allocation);
           
         Btn_Save.Set_Label_Color (Color_White);
      
         Btn_Save.Set_Background_Color ((0.1, 0.1, 0.8, 0.5));
      
         Btn_Save.Set_Font_Size (0.4, 0.5, 0.01);
            
      end if;
            
      -- Setup the view based on the loaded terrain
      --------------------------------------------------------------------------
      
      Flight.Data.Position := Maps.Terrain.Get_Middle;
      
      View.Center          := Flight.Data.Position;
      
      View.Zoom            := Lower_Zoom;
      
      View.Shrink          := Float (Maps.Shrinkage (Flight.Data.Position.Lat));
      
      Center_Frame;
      
      -- Route
      --------------------------------------------------------------------------
      
      Flight.Representation.Init;
      
      -- View update
      --------------------------------------------------------------------------
      
      Timing.Events.Register_Timer (Timer    => Timing.Time_Delta,
                                    Callback => Update_View'Access);
                      
      -- Wind arrow
      ------------------------------------------------------
      
      Load_Wind_Arrow;
      
   end Init;
   -----------------------------------------------------------------------------
        
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is
      
      use Utility.Strings;
      use Flight.Aircraft;
      
      Point : Point_Record;
      Left  : constant Float := 0.790;
      X     : Float := 0.0;
      Color : Line_Color_Record;

   begin
      
      View.Height := Height;
      
      View.Width  := Width;
      
      -- Map
      --------------------------------
      
      Maps.Terrain.Draw (View);
      
      Maps.Layers.Draw (View);
      
      Maps.Reference.Draw (View);
      
      Maps.Layers.Airspaces.Draw_Labels (View);
      
      --Flight.Draw_Horizontal_Path (View);
      
      if Edit_Mode then
         
         Btn_Size.Draw;
         
         Btn_Water.Draw;
      
         Btn_Save.Draw;
      
      end if;
      
      -- Flight representation
      --------------------------------
      
      Flight.Representation.Draw (View);
      
      -- Flight data
      --------------------------------
      
      Display.Panels.Gauges.Draw (Width, Height);
      
      Btn_Range.Draw;
      
      if View.Cone_Active then
         
         if Get_Range_Mode = Range_Straight then
            
            Btn_Range_Mode.Set_Label ("AHEAD");
            
         else
            
            Btn_Range_Mode.Set_Label ("LOCAL");
            
            if Get_Cone_Mode = Cone_10_To_1 then
               
               Btn_Cone_Mode.Set_Label ("10:1");
            
            else               
               Btn_Cone_Mode.Set_Label ("OPTIMAL");
            
            end if;
            
            Btn_Cone_Mode.Draw;
         
         end if;
         
         Btn_Range_Mode.Draw;
  
      end if;
         
      -- Map moving buttons
      --------------------------------
      
      Btn_Move.Draw;
      
      if Show_Move then
         
         Btn_Down.Draw;
      
         Btn_Up.Draw;
      
         Btn_Left.Draw;
      
         Btn_Right.Draw;
      
         --Btn_Zoom_In.Draw;
      
         --Btn_Zoom_Out.Draw;
           
      end if;
        
      -- MacCready setup buttons
      --------------------------------
      
      Btn_Ascent_Plus.Draw;
      
      Btn_Ascent_Min.Draw;
      
      if Flight.Aircraft.Get_Ascent > 0.0 then
         
         Color := Line_Grass;

      else
         
         Color := Line_Red;
         
      end if;
             
      Gl.Fonts.Draw (Text      => Utility.Strings.Float_Image (Flight.Aircraft.Get_Ascent, 1),
                     X         => Btn_Ascent_Plus.Get_Allocation.X + 0.5 * Btn_Ascent_Plus.Get_Allocation.W,
                     Y         => 0.5 * (Btn_Ascent_Plus.Get_Allocation.Y + Btn_Ascent_Min.Get_Allocation.Y + Btn_Ascent_Min.Get_Allocation.H),
                     Style     => Font_1,
                     Color     => Color,
                     Alignment => Gl.Fonts.Alignment_CC);
      
      -- Optimal airspeed in current direction
      ----------------------------------------
      
      -- TODO: manage units to synchronize with gauges panel (km/h <-> kts)
       
      if Flight.Data.Is_Recent (Flight.Field_Course) then
         
         declare
            Speed : Float := Flight.Aircraft.Get_Optimal_Speed;
         begin
         
            if Speed > 0.0 then
            
               Gl.Fonts.Draw (Text      => Float_Image (3.6 * Speed, 0),
                              X         => Btn_Ascent_Min.Get_Allocation.X - 0.02,
                              Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.16,
                              Style     => Font_3,
                              Color     => Line_White, 
                              Alignment => Gl.Fonts.Alignment_LR);
         
            end if;
         
         end;
         
      else
         
         Gl.Fonts.Draw (Text      => "---",
                        X         => Btn_Ascent_Min.Get_Allocation.X - 0.03,
                        Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.16,
                        Style     => Font_1,
                        Color     => Line_Red, 
                        Alignment => Gl.Fonts.Alignment_LR);
         
      end if;
         
      -- Necessary altitude to reach waypoint
      ----------------------------------------
      
      -- TODO: manage units to synchronize with gauges panel (m <-> FT)
 
      Gl.Fonts.Draw (Text      => "\",
                     X         => Btn_Ascent_Min.Get_Allocation.X - 0.14,
                     Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.02,
                     Style     => Font_4,
                     Color     => Line_Magenta,
                     Alignment => Gl.Fonts.Alignment_LL);
          
      if Flight.Data.Is_Recent (Flight.Field_Position) then
         
         Gl.Fonts.Draw (Text      => Flight.Plan.Next_Waypoint.Get_Required_Altitude_Image,
                        X         => Btn_Ascent_Min.Get_Allocation.X - 0.02,
                        Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.02,
                        Style     => Font_3,
                        Color     => Line_Magenta,
                        Alignment => Gl.Fonts.Alignment_LR);
                
      else
         
         Gl.Fonts.Draw (Text      => "---",
                        X         => Btn_Ascent_Min.Get_Allocation.X - 0.03,
                        Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.02,
                        Style     => Font_1, 
                        Color     => Line_Red,
                        Alignment => Gl.Fonts.Alignment_LR);         
         
      end if;
      
      -- Optimal airspeed to reach waypoint
      ----------------------------------------
      
      Gl.Fonts.Draw (Text      => ">",
                     X         => Btn_Ascent_Min.Get_Allocation.X - 0.14,
                     Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.09,
                     Style     => Font_4,
                     Color     => Line_Magenta,
                     Alignment => Gl.Fonts.Alignment_LL);
      
      if Flight.Data.Is_Recent (Flight.Field_Position) then
         
         Gl.Fonts.Draw (Text      => Flight.Plan.Next_Waypoint.Get_Optimal_Speed_Image,
                        X         => Btn_Ascent_Min.Get_Allocation.X - 0.02,
                        Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.09,
                        Style     => Font_3,
                        Color     => Line_Magenta, 
                        Alignment => Gl.Fonts.Alignment_LR);
         
      else
         
         Gl.Fonts.Draw (Text      => "---",
                        X         => Btn_Ascent_Min.Get_Allocation.X - 0.03,
                        Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.09,
                        Style     => Font_1, 
                        Color     => Line_Red,
                        Alignment => Gl.Fonts.Alignment_LR);         
         
      end if;
      
      -- Wind speed and direction
      ----------------------------------------

      Draw_Wind;
       
      -- GNSS signal indication
      ----------------------------------------

      X := Btn_Ascent_Min.Get_Allocation.X + Btn_Ascent_Min.Get_Allocation.W;
               
      declare
         use Flight;
         use Flight.Signal;
         S : Natural := 0; -- (number of satellites in view with a decent strength level)
      begin
            
         for K in Satellite_Kinds loop
            
            for Sat of Satellites (K) loop   
               
               if Sat.Valid and Sat.Strength > 20 then
                  S := S + 1;           
               end if;
               
            end loop;            
            
         end loop;
         
         if S = 0 or not Data.Is_Recent (Field_Position) then
               
            if Display.Blink or Gnss_Blink_Count > 10 then
            
               Gl.Fonts.Draw (Text      => "GNSS",
                              X         => X,
                              Y         => 0.94,
                              Style     => Font_1,
                              Color     => Line_Red,
                              Alignment => Gl.Fonts.Alignment_LR);
            
               Gnss_Blink_Count := Gnss_Blink_Count + 1;
            
            end if;
         
         else
               
            Gnss_Blink_Count := 0;
               
            for I in 1..S loop
              
               Gl.Fonts.Draw (Text      => "|",
                              X         => X,
                              Y         => 0.94,
                              Style     => Font_2,
                              Color     => Line_Green,
                              Alignment => Gl.Fonts.Alignment_LR);  
            
               Gnss_Blink_Count := Gnss_Blink_Count + 1;  
               
               X := X - 0.008; 
            
            end loop;
         
         end if;
      
      end;
     
   end Draw;
   -----------------------------------------------------------------------------
      



   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
      
      use Flight.Aircraft;
      use Math.Vector2;
      use Maps.Terrain;
      use Utility.Strings;
      
      Changed : Boolean := True; 
      
   begin
      
      Display.Panels.Gauges.Screen_Pressed (X ,Y);
      
      if 
        Btn_Move.Contains (X, Y) 
      then
                  
         Show_Move := not Show_Move;
         
         if Show_Move then
            
            Btn_Move.Set_Background_Color (Color_Amber.With_Alpha (0.8));	
         
         else
                        
            Btn_Move.Set_Background_Color (Color_Black.With_Alpha (0.5));
            
            --if not Auto_Center then
               
            --   View.Center := Flight.Data.Position;
         
            --   Auto_Center := True;
            
            --end if;
            
         end if;
         
      elsif 
        Btn_Range.Contains (X, Y)
      then
         
         View.Cone_Active := not View.Cone_Active;
         
         if View.Cone_Active then
            
            Btn_Range.Set_Background_Color (Color_Amber.With_Alpha (0.8));	
         
         else
                        
            Btn_Range.Set_Background_Color (Color_Black.With_Alpha (0.5));
            
         end if;
             
      elsif 
        Btn_Range_Mode.Contains (X, Y)
      then
         
         if Get_Range_Mode = Range_Straight then
            Flight.Aircraft.Set_Reference  (Flight.Plan.Home_Waypoint.Position);
            Flight.Aircraft.Set_Range_Mode (Range_Local);
         else
            Flight.Aircraft.Set_Range_Mode (Range_Straight);
         end if;
                
      elsif 
        Btn_Cone_Mode.Contains (X, Y)
      then
         
         if Get_Cone_Mode = Cone_Optimal then
            Flight.Aircraft.Set_Cone_Mode (Cone_10_To_1);
         else
            Flight.Aircraft.Set_Cone_Mode (Cone_Optimal);
         end if;
                 
      elsif
        Btn_Ascent_Plus.Contains (X, Y)
      then
         Flight.Aircraft.Set_Ascent (Flight.Aircraft.Get_Ascent + 0.5);
                  
      elsif
        Btn_Ascent_Min.Contains (X, Y)
      then
         Flight.Aircraft.Set_Ascent (Flight.Aircraft.Get_Ascent - 0.5);
         
      elsif 
        Show_Move and 
        Btn_Zoom_In.Contains (X, Y) 
      then
         
         View.Zoom_In;
         
         Center_Frame;
         
      elsif 
        Show_Move and 
        Btn_Zoom_Out.Contains (X, Y) 
      then
         
         View.Zoom_Out;
         
         Center_Frame;
           
      elsif 
        Show_Move and 
        Btn_Right.Contains (X, Y)
      then
         
         View.Center.Lon := View.Center.Lon + 0.15 * Long_Float (View.Zoom * View.Width);
         
         Auto_Center := False;
         
      elsif 
        Show_Move and
        Btn_Left.Contains (X, Y)
      then
         
         View.Center.Lon := View.Center.Lon - 0.15 * Long_Float (View.Zoom * View.Width);
         
         Auto_Center := False;
         
      elsif 
        Show_Move and
        Btn_Up.Contains (X, Y)
      then
         
         View.Center.Lat := View.Center.Lat + 0.15 * Long_Float (View.Zoom * View.Height);
         
         Auto_Center := False;
         
      elsif 
        Show_Move and 
        Btn_Down.Contains (X, Y) 
      then
         
         View.Center.Lat := View.Center.Lat - 0.15 * Long_Float (View.Zoom * View.Height);
         
         Auto_Center := False;
       
      elsif Edit_Mode then
            
         if Btn_Size.Contains (X, Y) then
            
            Edit_Size := Edit_Size + 2;
            
            if Edit_Size > 20 then
               
               Edit_Size := 0;
                 
            end if;
            
            Btn_Size.Set_Label (Integer_Image (Edit_Size));
            
         elsif Btn_Water.Contains (X, Y) then
            
            if Edit_Type = Waterize then
               
               Edit_Type := Dewaterize;
               
            elsif Edit_Type = Dewaterize then
               
               Edit_Type := Rewaterize;
               
            elsif Edit_Type = Rewaterize then
               
               Edit_Type := Off;
               
            elsif Edit_Type = Off then
               
               Edit_Type := Waterize;
               
            end if;
            
            Btn_Water.Set_Label (Edit_Types'Image (Edit_Type));
                 
         elsif Btn_Save.Contains (X, Y) then
                   
            Terrain.Save_Binary;
            
         else
                     
            declare
               V : Vector2_Record := New_Vector2_Record (Long_Float (X), Long_Float (Y));
            begin
                           
               case Edit_Type is
                            
                  when Waterize =>
               
                     Maps.Terrain.Modify_Altitude (Position    => View.To_Map_Coordinates (V),
                                                   Elevation   => 0.0,
                                                   Extent      => Edit_Size,
                                                   Lower_Level => Short_Float'First,
                                                   Upper_Level => Short_Float'Last);
               
                  when Dewaterize =>
               
                     Maps.Terrain.Modify_Altitude (Position    => View.To_Map_Coordinates (V),
                                                   Elevation   => 0.1,
                                                   Extent      => Edit_Size,
                                                   Lower_Level => 0.0,
                                                   Upper_Level => 0.0);
                
                  when Rewaterize =>
               
                     Maps.Terrain.Modify_Altitude (Position    => View.To_Map_Coordinates (V),
                                                   Elevation   => 0.0,
                                                   Extent      => Edit_Size,
                                                   Lower_Level => 0.1,
                                                   Upper_Level => 0.1);
               
                  when others =>
                  
                     null;
                     
               end case;
            
            end;
            
         end if;
                  
      else
         
         Changed := False;
         
      end if;
      
      if Changed then 
         
         Display.Refresh := True;
      
      end if;
               
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin 
      
      case Key is
         
         when Panel_Wheel_Left =>
            
            -- Zoom out
            ----------------------------
            
            View.Zoom_Out;
            
            Display.Refresh := True;
      
         when Panel_Wheel_Right =>
            
            -- Zoom in
            ----------------------------
            
            View.Zoom_In;
            
            Display.Refresh := True;
      
         when Panel_Wheel_Button =>
            
            -- Reset the view
            ----------------------------
                 
            Show_Move := False;
                     
            Auto_Center := True;
            
            View.Zoom := Lower_Zoom;
                      
            Btn_Move.Set_Background_Color (Color_Black.With_Alpha (0.5));
            
            Center_Frame;
            
            Update_View;
            
            Display.Refresh := True;
            
         when Panel_Button_Right =>
            
            -- Toggle cone
            ----------------------------
            
            View.Cone_Active := not View.Cone_Active;
            
         when Panel_Button_Left =>
              
            -- Not assigned
            ----------------------------
            
            null;
            
         when others =>
            
            null;
            
      end case;
                        
   end Key_Changed;
   -----------------------------------------------------------------------------
   
   
   

   --===========================================================================
   --
   --===========================================================================
   procedure Drag (X, Y : Float) is
   begin

      null;
      
   end Drag;
   -----------------------------------------------------------------------------
   
end Display.Pages.Navigation;
--------------------------------------------------------------------------------
