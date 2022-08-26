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
with Ada.Text_IO;
-- Gnav
with Display.Compass;
with Flight;
with Flight.Aircraft;
with Flight.Plan;
with Flight.Representation;
with Flight.Stream;
with Gl.Fonts;
with Math.Vector2;
with Maps;
use  Maps;
with Maps.Terrain;
with Maps.Layers;
with Maps.Layers.Airspaces;
with Maps.Loader;
with Timing.Events;
with Utility.Colors;
use  Utility.Colors;
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
package body Display.Flight_Panel is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The map view
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   View : Maps.Map_View_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The timer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Timer : Ada.Calendar.Time;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Flag that indicates if the timer must be reset on the next click
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Reset_Timer : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the UTC time must be displayed
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Utc_Time : Boolean := False;

   -- Flight data indicators
   ---------------------------------

   Frm_Data      : Widget_Record;

   Pnl_Clock     : Panel_Record;

   Pnl_Timer     : Panel_Record;

   Pnl_Speed     : Panel_Record;

   Pnl_Altitude  : Panel_Record;

   Pnl_Elevation : Panel_Record;

   Pnl_Waypoint  : Panel_Record;

   Pnl_Home      : Panel_Record;

   -- Status variables
   ---------------------------------

   Initialized   : Boolean := False;

   Altitude_Unit : Altitude_Units := Unit_Meter;

   Distance_Unit : Distance_Units := Unit_Kilometer;

   Velocity_Unit : Velocity_Units := Unit_Kilometer_Hour;

   -- Fonts
   ---------------------------------
   Font_1 : Gl.Fonts.Font_Style_Record := (Width     => 0.010,
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

   -- Fonts
   ---------------------------------
   Font_2 : Gl.Fonts.Font_Style_Record := (Width     => 0.020,
                                           Height    => 0.060,
                                           Space     => 0.008,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular,
                                           Line_R    => 0.0,
                                           Line_G    => 1.0,
                                           Line_B    => 1.0,
                                           Glow_R    => 0.1,
                                           Glow_G    => 0.1,
                                           Glow_B    => 0.8);

   -- Fonts
   ---------------------------------
   Font_5 : Gl.Fonts.Font_Style_Record := (Width     => 0.020,
                                           Height    => 0.060,
                                           Space     => 0.008,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular,
                                           Line_R    => 0.6,
                                           Line_G    => 0.6,
                                           Line_B    => 0.6,
                                           Glow_R    => 0.1,
                                           Glow_G    => 0.1,
                                           Glow_B    => 0.1);

   -- Fonts
   ---------------------------------
   Font_3 : Gl.Fonts.Font_Style_Record := (Width     => 0.015,
                                           Height    => 0.035,
                                           Space     => 0.008,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular,
                                           Line_R    => 1.0,
                                           Line_G    => 0.3,
                                           Line_B    => 1.0,
                                           Glow_R    => 0.4,
                                           Glow_G    => 0.1,
                                           Glow_B    => 0.4);

   -- Fonts
   ---------------------------------
   Font_4 : Gl.Fonts.Font_Style_Record := (Width     => 0.015,
                                           Height    => 0.035,
                                           Space     => 0.008,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular,
                                           Line_R    => 0.3,
                                           Line_G    => 1.0,
                                           Line_B    => 0.3,
                                           Glow_R    => 0.1,
                                           Glow_G    => 0.4,
                                           Glow_B    => 0.1);

   -- Fonts
   ---------------------------------
   Font_6 : Gl.Fonts.Font_Style_Record := (Width     => 0.008,
                                           Height    => 0.030,
                                           Space     => 0.003,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular,
                                           Line_R    => 0.2,
                                           Line_G    => 0.8,
                                           Line_B    => 0.2,
                                           Glow_R    => 0.1,
                                           Glow_G    => 0.3,
                                           Glow_B    => 0.1);

   -- Fonts
   ---------------------------------
   Font_Hours : Gl.Fonts.Font_Style_Record := (Width     => 0.018,
                                               Height    => 0.070,
                                               Space     => 0.008,
                                               Rendering => Gl.Fonts.Font_Glow,
                                               Thickness => Gl.Fonts.Font_Regular,
                                               Line_R    => 0.8,
                                               Line_G    => 0.8,
                                               Line_B    => 0.8,
                                               Glow_R    => 0.1,
                                               Glow_G    => 0.1,
                                               Glow_B    => 0.1);

   -- Fonts
   ---------------------------------
   Font_Minutes : Gl.Fonts.Font_Style_Record := (Width     => 0.010,
                                                 Height    => 0.040,
                                                 Space     => 0.008,
                                                 Rendering => Gl.Fonts.Font_Glow,
                                                 Thickness => Gl.Fonts.Font_Regular,
                                                 Line_R    => 0.8,
                                                 Line_G    => 0.8,
                                                 Line_B    => 0.8,
                                                 Glow_R    => 0.1,
                                                 Glow_G    => 0.1,
                                                 Glow_B    => 0.1);

   -- Fonts
   ---------------------------------
   Font_Error : Gl.Fonts.Font_Style_Record := (Width     => 0.010,
                                               Height    => 0.040,
                                               Space     => 0.008,
                                               Rendering => Gl.Fonts.Font_Glow,
                                               Thickness => Gl.Fonts.Font_Regular,
                                               Line_R    => 1.0,
                                               Line_G    => 0.0,
                                               Line_B    => 0.0,
                                               Glow_R    => 0.1,
                                               Glow_G    => 0.1,
                                               Glow_B    => 0.1);

   Cancel_Timer : access Timing.Events.Timer_Record := null;

   --===========================================================================
   -- Cancels the reset of the timer
   --===========================================================================
   procedure Cancel_Timer_Reset is
   begin

      Reset_Timer := False;

      Refresh     := True;

      Cancel_Timer.Pause;

   end Cancel_Timer_Reset;
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

      if Initialized then
         return;
      end if;

      -- Data frame background
      ------------------------------------------------------

      Allocation.X := 0.78;
      Allocation.H := 1.0;
      Allocation.Y := 0.0;
      Allocation.W := 0.22;

      Frm_Data.Set_Allocation (Allocation);

      Frm_Data.Set_Background_Color (Color_Gray_3);

      Frm_Data.Set_Transparency (0.50);

      Frm_Data.Set_Show_Border (False);

      -- Timer panel
      ------------------------------------------------------

      Allocation.X := 0.785;
      Allocation.Y := 0.870;
      Allocation.W := 0.100;
      Allocation.H := 0.100;

      Pnl_Timer.Set_Allocation (Allocation);

      Pnl_Timer.Set_Background_Color (Color_Black);

      Pnl_Timer.Set_Transparency (0.60);

      Pnl_Timer.Set_Show_Border (True);

      Pnl_Timer.Set_Label ("TMR", Label_Right);

      Pnl_Timer.Set_Font_Size (0.03, 0.25);

      Pnl_Timer.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      -- Clock panel
      ------------------------------------------------------

      Allocation.X := 0.895;
      Allocation.Y := 0.870;
      Allocation.W := 0.100;
      Allocation.H := 0.100;

      Pnl_Clock := Pnl_Timer;

      Pnl_Clock.Set_Allocation (Allocation);

      Pnl_Clock.Set_Label ("L/T", Label_Right);

      -- Speed panel
      ------------------------------------------------------

      Pnl_Speed := Pnl_Timer;

      Allocation.X := 0.785;
      Allocation.Y := 0.740;
      Allocation.W := 0.210;
      Allocation.H := 0.100;

      Pnl_Speed.Set_Allocation (Allocation);

      Pnl_Speed.Set_Label ("G/S", Label_Right);

      -- Altitude panel
      ------------------------------------------------------

      Pnl_Altitude := Pnl_Speed;

      Allocation.Y := 0.61;

      Pnl_Altitude.Set_Allocation (Allocation);

      Pnl_Altitude.Set_Label ("ASL", Label_Right);

      -- Elevation panel
      ------------------------------------------------------

      Pnl_Elevation := Pnl_Altitude;

      Allocation.Y := 0.48;

      Pnl_Elevation.Set_Allocation (Allocation);

      Pnl_Elevation.Set_Label ("AGL", Label_Right);

      -- Compass
      ------------------------------------------------------

      Display.Compass.Init;

      -- Waypoint vector panel
      ------------------------------------------------------

      Pnl_Waypoint := Pnl_Altitude;

      Allocation.Y := 0.010;
      Allocation.W := 0.100;
      Allocation.H := 0.140;

      Pnl_Waypoint.Set_Allocation (Allocation);

      Pnl_Waypoint.Set_Label_Color ((Font_3.Line_R, Font_3.Line_G, Font_3.Line_B, 1.0),
                                    (Font_3.Glow_R, Font_3.Glow_G, Font_3.Glow_B, 1.0));

      Pnl_Waypoint.Set_Label ("WPT");

      -- Home vector panel
      ------------------------------------------------------

      Pnl_Home := Pnl_Waypoint;

      Allocation.X := 0.895;

      Pnl_Home.Set_Allocation (Allocation);

      Pnl_Home.Set_Label_Color ((Font_4.Line_R, Font_4.Line_G, Font_4.Line_B, 1.0),
                                (Font_4.Glow_R, Font_4.Glow_G, Font_4.Glow_B, 1.0));

      Pnl_Home.Set_Label ("B/H");

      -- Setup done
      ------------------------------------------------------

      Timer := Ada.Calendar.Clock;

      Cancel_Timer := Timing.Events.Register_Timer (3.0, Cancel_Timer_Reset'Access);

      Cancel_Timer.Pause;

      Initialized := True;

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw (Width, Height : Float) is

      use Ada.Calendar;
      use Utility.Strings;

      Point : Point_Record;

      Left  : constant Float := 0.790;

      Clock : Time := Ada.Calendar.Clock;

      Timer_Span : Duration := Ada.Calendar.Clock - Timer;

   begin

      if not Initialized then

         return;

      end if;

      -- Transparent background frame
      ------------------------------------------------------
      Frm_Data.Draw;

      -- Time
      ------------------------------------------------------

      if Reset_Timer then

         Pnl_Timer.Set_Label_Color (Color_Red);

      else

         Pnl_Timer.Set_Label_Color (Color_Gray_8);

      end if;

      Pnl_Timer.Draw;

      if Timer_Span < 3600.0 then

         Gl.Fonts.Draw (Utility.Strings.Minute_Image (Timer_Span),
                        0.835,
                        0.885,
                        Font_Hours,
                        Gl.Fonts.Alignment_LR);

         Gl.Fonts.Draw (Utility.Strings.Second_Image (Timer_Span),
                        0.880,
                        0.885,
                        Font_Minutes,
                        Gl.Fonts.Alignment_LR);

      else

         Gl.Fonts.Draw (Utility.Strings.Hour_Image (Timer_Span),
                        0.835,
                        0.885,
                        Font_Hours,
                        Gl.Fonts.Alignment_LR);

         Gl.Fonts.Draw (Utility.Strings.Minute_Image (Timer_Span),
                        0.880,
                        0.885,
                        Font_Minutes,
                        Gl.Fonts.Alignment_LR);

      end if;

      -- Clock
      ------------------------------------------------------

      if Utc_Time then

         Clock := Clock + Utc_Shift;

      end if;

      Pnl_Clock.Draw;

      Gl.Fonts.Draw (Utility.Strings.Hour_Image (Clock),
                     0.945,
                     0.885,
                     Font_Hours,
                     Gl.Fonts.Alignment_LR);

      Gl.Fonts.Draw (Utility.Strings.Minute_Image (Clock),
                     0.990,
                     0.885,
                     Font_Minutes,
                     Gl.Fonts.Alignment_LR);

      -- Ground speed
      ------------------------------------------------------

      Pnl_Speed.Draw;

      if Flight.Data.Is_Recent (Flight.Field_Speed) then

         Gl.Fonts.Draw (Utility.Strings.Float_Image (Convert (Flight.Data.Speed, Unit_Kilometer_Hour, Velocity_Unit), 0),
                        0.935,
                        0.760,
                        Font_2,
                        Gl.Fonts.Alignment_LR);

      else

         Gl.Fonts.Draw ("---",
                        0.935,
                        0.760,
                        Font_Error,
                        Gl.Fonts.Alignment_LR);

      end if;

      Gl.Fonts.Draw (Image (Velocity_Unit),
                     0.968,
                     0.760,
                     Font_6,
                     Gl.Fonts.Alignment_LC);

      -- Altitude above mean sea level (not barometric)
      ------------------------------------------------------

      Pnl_Altitude.Draw;

      if Flight.Data.Is_Recent (Flight.Field_Altitude) then

         Gl.Fonts.Draw (Utility.Strings.Float_Image (Convert (Flight.Data.Altitude, Unit_Meter, Altitude_Unit), 0),
                        0.935,
                        0.630,
                        Font_2,
                        Gl.Fonts.Alignment_LR);

      else

         Gl.Fonts.Draw ("---",
                        0.935,
                        0.630,
                        Font_Error,
                        Gl.Fonts.Alignment_LR);

      end if;

      Gl.Fonts.Draw (Image (Altitude_Unit),
                     0.968,
                     0.630,
                     Font_6,
                     Gl.Fonts.Alignment_LC);

      -- Elevation above ground level (not barometric)
      ------------------------------------------------------

      Pnl_Elevation.Draw;

      if Flight.Data.Is_Recent (Flight.Field_Elevation) then

         Gl.Fonts.Draw (Utility.Strings.Float_Image (Convert (Flight.Data.Elevation, Unit_Meter, Altitude_Unit), 0),
                        0.935,
                        0.500,
                        Font_5,
                        Gl.Fonts.Alignment_LR);

      else

         Gl.Fonts.Draw ("---",
                        0.935,
                        0.500,
                        Font_Error,
                        Gl.Fonts.Alignment_LR);

      end if;

      Gl.Fonts.Draw (Image (Altitude_Unit),
                     0.968,
                     0.500,
                     Font_6,
                     Gl.Fonts.Alignment_LC);

      -- Compass
      ------------------------------------------------------

      Display.Compass.Draw (0.89, 0.320, 0.15, Width / Height, Font_2);

      -- Waypoint and home panels
      ------------------------------------------------------

      Pnl_Waypoint.Set_Label (Trim (Flight.Plan.Next_Waypoint.Name));

      Pnl_Waypoint.Draw;

      Pnl_Home.Set_Label (Trim (Flight.Plan.Home_Waypoint.Name));

      Pnl_Home.Draw;

      Gl.Fonts.Draw (Flight.Plan.Next_Waypoint.Get_Distance_Image,
                     0.88,
                     0.07,
                     Font_3,
                     Gl.Fonts.Alignment_LR);

      Gl.Fonts.Draw (Flight.Plan.Next_Waypoint.Get_Bearing_Image,
                     0.88,
                     0.02,
                     Font_3,
                     Gl.Fonts.Alignment_LR);

      Gl.Fonts.Draw (Flight.Plan.Home_Waypoint.Get_Distance_Image,
                     0.99,
                     0.07,
                     Font_4,
                     Gl.Fonts.Alignment_LR);

      Gl.Fonts.Draw (Flight.Plan.Home_Waypoint.Get_Bearing_Image,
                     0.99,
                     0.02,
                     Font_4,
                     Gl.Fonts.Alignment_LR);

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is

      use Math.Vector2;
      use Maps.Terrain;
      use Utility.Strings;

      Changed : Boolean := True;

   begin

      if not Initialized then
         return;
      end if;

      if
        Pnl_Altitude.Contains  (X, Y) or
        Pnl_Elevation.Contains (X, Y)
      then

         if Altitude_Unit = Unit_Meter then

            Altitude_Unit := Unit_Feet;

         else

            Altitude_Unit := Unit_Meter;

         end if;

      elsif
        Pnl_Speed.Contains (X, Y)
      then

         if Velocity_Unit = Unit_Kilometer_Hour then

            Velocity_Unit := Unit_Knot;

         else

            Velocity_Unit := Unit_Kilometer_Hour;

         end if;

      elsif
        Pnl_Timer.Contains (X, Y)
      then

         if Reset_Timer then

            Timer := Ada.Calendar.Clock;

         end if;

         Reset_Timer := not Reset_Timer;

         if Reset_Timer then

            Cancel_Timer.Restart;

         end if;

      elsif
        Pnl_Clock.Contains (X, Y)
      then

         Utc_Time := not Utc_Time;

         if Utc_Time then

            Pnl_Clock.Set_Label ("UTC", Label_Right);

         else

            Pnl_Clock.Set_Label ("L/T", Label_Right);

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

            null;

         when Panel_Wheel_Right =>

            null;

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




   --===========================================================================
   --
   --===========================================================================
   procedure Drag (X, Y : Float) is
   begin

      null;

   end Drag;
   -----------------------------------------------------------------------------

end Display.Flight_Panel;
--------------------------------------------------------------------------------
