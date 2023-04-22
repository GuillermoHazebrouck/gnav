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

-- Stadard
with Ada.Command_Line;
-- Gnav
with Flight.Aircraft;
use  Flight.Aircraft;
with Gl.Colors;
use  Gl.Colors;
with Gl.Fonts;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Strings;
with Utility.Units;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Glider is

   -- Fonts
   ---------------------------------
   Font_1 : Gl.Fonts.Font_Style_Record := (Width     => 0.010,
                                           Height    => 0.030,
                                           Space     => 0.006,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular);

   Font_2 : Gl.Fonts.Font_Style_Record := (Width     => 0.016,
                                           Height    => 0.050,
                                           Space     => 0.006,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular);

   Font_3 : Gl.Fonts.Font_Style_Record := (Width     => 0.008,
                                           Height    => 0.025,
                                           Space     => 0.006,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular);

   M : constant Dimension_Float := 0.01;
   H : constant Dimension_Float := 0.10;
   W : constant Dimension_Float := 0.15;

   -- Page widgets
   ---------------------------------

   Btn_Model : Button_Record;

   Btn_Mass  : Button_Record;

   Btn_Next  : Button_Record;

   Pnl_Mass  : Panel_Record;

   Pnl_Info  : Panel_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Mass_Control is record

      Btn_Mass : Button_Record;

      Btn_Plus : Button_Record;

      Btn_Less : Button_Record;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Mass_Controls : array (Mass_Point_Range) of Mass_Control;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Mass_Focus    : Mass_Point_Range := Mass_Point_Range'First;

   --===========================================================================
   --
   --===========================================================================
   procedure Init is

      use Utility.Strings;

      Allocation : Allocation_Record;

   begin

      -- Info panel
      ------------------------------------------------------

      Allocation.X := 0.58;

      Allocation.Y := 0.01;

      Allocation.H := 0.85;

      Allocation.W := 0.41;

      Pnl_Info.Set_Allocation (Allocation);

      Pnl_Info.Set_Background_Color (Color_Black);

      Pnl_Info.Set_Transparency (0.60);

      Pnl_Info.Set_Font_Size (0.03, 0.25);

      Pnl_Info.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Info.Set_Label ("OPERATION");

      -- Mass panel
      ------------------------------------------------------

      Allocation.X := 0.01;

      Allocation.Y := 0.01;

      Allocation.H := 0.85;

      Allocation.W := 0.56;

      Pnl_Mass.Set_Allocation (Allocation);

      Pnl_Mass.Set_Background_Color (Color_Black);

      Pnl_Mass.Set_Transparency (0.60);

      Pnl_Mass.Set_Font_Size (0.03, 0.25);

      Pnl_Mass.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Mass.Set_Label ("MASS");

      --

      Allocation.X := 1.0 - M - 0.12;

      Allocation.Y := 1.0 - (H + M);

      Allocation.W := 0.12;

      Allocation.H := H;

      Btn_Next.Set_Allocation (Allocation);

      Btn_Next.Set_Label (">");

      Btn_Next.Set_Background_Color (Color_Black);

      Btn_Next.Set_Border_Color (Color_Gray_3);

      Btn_Next.Set_Label_Color (Line_Gray);

      Btn_Next.Set_Font_Size (0.6, 0.4);

      --

      Allocation.X := 0.16 + M;

      Allocation.Y := 1.0 - (H + M);

      Allocation.W := Btn_Next.Get_Allocation.X - M - Allocation.X;

      Allocation.H := H;

      Btn_Model.Set_Allocation (Allocation);

      Btn_Model.Set_Label (Trim (This_Aircraft.Model) & "/" & Trim (This_Aircraft.Registration));

      Btn_Model.Set_Background_Color (Color_Black);

      Btn_Model.Set_Border_Color (Color_Gray_3);

      Btn_Model.Set_Label_Color (Line_Cyan);

      Btn_Model.Set_Font_Size (0.5, 0.35);

      --

      Btn_Mass.Set_Background_Color (Color_Black);

      Btn_Mass.Set_Border_Color (Color_Gray_3);

      Btn_Mass.Set_Label_Color (Line_Magenta);

      --

      Allocation.Y := Pnl_Mass.Get_Allocation.Y + Pnl_Mass.Get_Allocation.H - 0.02;

      for I in Flight.Aircraft.Mass_Point_Range loop

         Mass_Controls (I).Btn_Mass.Set_Background_Color (Color_Gray_3);

         Mass_Controls (I).Btn_Mass.Set_Border_Color (Color_Gray_4);

         Mass_Controls (I).Btn_Mass.Set_Label_Color (Line_Cyan);

         Mass_Controls (I).Btn_Mass.Set_Font_Size (0.6, 0.3);

         Allocation.X := (Font_1.Width + Font_1.Space) * Float (String_12'Length) + M;

         Allocation.Y := Allocation.Y - (H + 2.0 * M);

         Allocation.W := 0.12;

         Allocation.H := H;

         Mass_Controls (I).Btn_Mass.Set_Allocation (Allocation);

         --

         Mass_Controls (I).Btn_Plus.Set_Label ("+");

         Mass_Controls (I).Btn_Plus.Set_Background_Color (Color_Sky);

         Mass_Controls (I).Btn_Plus.Set_Border_Color (Color_Blue);

         Mass_Controls (I).Btn_Plus.Set_Label_Color (Color_White);

         Mass_Controls (I).Btn_Plus.Set_Font_Size (0.7, 0.5);

         Allocation.X := Allocation.X + Allocation.W + 2.0 * (M + Font_1.Width + Font_1.Space) + M;

         Allocation.W := 0.08;

         Mass_Controls (I).Btn_Plus.Set_Allocation (Allocation);

         --

         Mass_Controls (I).Btn_Less.Set_Label ("-");

         Mass_Controls (I).Btn_Less.Set_Background_Color (Color_Sky);

         Mass_Controls (I).Btn_Less.Set_Border_Color (Color_Blue);

         Mass_Controls (I).Btn_Less.Set_Label_Color (Color_White);

         Mass_Controls (I).Btn_Less.Set_Font_Size (0.7, 0.5);

         Allocation.X := Allocation.X + Allocation.W + M;

         Mass_Controls (I).Btn_Less.Set_Allocation (Allocation);

      end loop;

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      use Flight.Aircraft;
      use Utility.Strings;
      use Utility.Units;

      X : Float;
      Y : Float;
      C : Line_Color_Record := Line_Cyan;

   begin

      Btn_Model.Draw;

      Btn_Next.Draw;

      -- Mass panel
      --------------------------------------------------------------------------
      Pnl_Mass.Draw;

      for I in Mass_Point_Range loop

         if This_Aircraft.Mass_Points (I).Active then

            if Mass_Focus = I then

               Mass_Controls (I).Btn_Mass.Set_Background_Color (Color_Pink);

            else

               Mass_Controls (I).Btn_Mass.Set_Background_Color (Color_Gray_3);

            end if;

            Mass_Controls (I).Btn_Mass.Set_Label (Trim (Integer'Image (Integer (This_Aircraft.Mass_Points (I).Mass))));

            Mass_Controls (I).Btn_Mass.Set_Label_Color (Line_Cyan);

            if
              (This_Aircraft.Mass_Points (I).Mass > This_Aircraft.Mass_Points (I).Mass_Max or else
               This_Aircraft.Mass_Points (I).Mass < This_Aircraft.Mass_Points (I).Mass_Min) and then Blink
            then

               Mass_Controls (I).Btn_Mass.Set_Label_Color (Line_Red);

            end if;

            Y := Mass_Controls (I).Btn_Mass.Get_Allocation.Y + 0.5 * (Mass_Controls (I).Btn_Mass.Get_Allocation.H - Font_1.Height);

            X := 2.0 * M;

            Gl.Fonts.Draw (Trim (This_Aircraft.Mass_Points (I).Label),
                           X,
                           Y,
                           Font_1,
                           Line_White);

            X := Mass_Controls (I).Btn_Mass.Get_Allocation.X + Mass_Controls (I).Btn_Mass.Get_Allocation.W + M;

            Gl.Fonts.Draw ("KG",
                           X,
                           Y,
                           Font_3,
                           Line_Grass);

            Mass_Controls (I).Btn_Mass.Draw;

            Mass_Controls (I).Btn_Plus.Draw;

            Mass_Controls (I).Btn_Less.Draw;

         end if;

      end loop;

      Y := Pnl_Mass.Get_Allocation.Y + 2.0 * M;

      if This_Aircraft.Total_Mass > This_Aircraft.Maximum_Mass and Blink then

         C := Line_Red;

      end if;

      Gl.Fonts.Draw ("TOTAL = " & Float_Image (This_Aircraft.Total_Mass, 0),
                     Pnl_Mass.Get_Allocation.X + M, Y,
                     Font_2,
                     C);

      Gl.Fonts.Draw ("KG",
                     Pnl_Mass.Get_Allocation.X + 0.55 * Pnl_Mass.Get_Allocation.W, Y,
                     Font_3,
                     Line_Grass);

      -- Info panel
      --------------------------------------------------------------------------
      Pnl_Info.Draw;

      X := Pnl_Info.Get_Allocation.X + 0.05;
      Y := Pnl_Info.Get_Allocation.Y + Pnl_Info.Get_Allocation.H - 2.3 * Font_2.Height;

      Gl.Fonts.Draw ("STALL > ",
                     X, Y,
                     Font_2,
                     Line_White);

      Gl.Fonts.Draw (Float_Image (Convert (This_Aircraft.V_S0, Unit_Meter_Second, Unit_Kilometer_Hour), 0),
                     X + 0.55 * Pnl_Info.Get_Allocation.W, Y,
                     Font_2,
                     Line_White,
                     Gl.Fonts.Alignment_LR);

      Gl.Fonts.Draw (Image (Unit_Kilometer_Hour),
                     X + 0.55 * Pnl_Info.Get_Allocation.W + 0.02, Y + 0.01,
                     Font_3,
                     Line_Grass);

      Y := Y - 1.8 * Font_2.Height;

      Gl.Fonts.Draw ("LAND  > ",
                     X, Y,
                     Font_2,
                     Line_White);

      Gl.Fonts.Draw (Float_Image (Convert (This_Aircraft.V_LND, Unit_Meter_Second, Unit_Kilometer_Hour), 0),
                     X + 0.55 * Pnl_Info.Get_Allocation.W, Y,
                     Font_2,
                     Line_White,
                     Gl.Fonts.Alignment_LR);

      Gl.Fonts.Draw (Image (Unit_Kilometer_Hour),
                     X + 0.55 * Pnl_Info.Get_Allocation.W + 0.02, Y + 0.01,
                     Font_3,
                     Line_Grass);

      Y := Y - 1.8 * Font_2.Height;

      Gl.Fonts.Draw ("VNO   >",
                     X, Y,
                     Font_2,
                     Line_Yellow);

      Gl.Fonts.Draw (Float_Image (Convert (This_Aircraft.V_NO, Unit_Meter_Second, Unit_Kilometer_Hour), 0),
                     X + 0.55 * Pnl_Info.Get_Allocation.W, Y,
                     Font_2,
                     Line_Yellow,
                     Gl.Fonts.Alignment_LR);

      Gl.Fonts.Draw (Image (Unit_Kilometer_Hour),
                     X + 0.55 * Pnl_Info.Get_Allocation.W + 0.02, Y + 0.01,
                     Font_3,
                     Line_Grass);

      Y := Y - 1.8 * Font_2.Height;

      Gl.Fonts.Draw ("VNE   >",
                     X, Y,
                     Font_2,
                     Line_Red);

      Gl.Fonts.Draw (Float_Image (Convert (This_Aircraft.V_NE, Unit_Meter_Second, Unit_Kilometer_Hour), 0),
                     X + 0.55 * Pnl_Info.Get_Allocation.W, Y,
                     Font_2,
                     Line_Red,
                     Gl.Fonts.Alignment_LR);

      Gl.Fonts.Draw (Image (Unit_Kilometer_Hour),
                     X + 0.55 * Pnl_Info.Get_Allocation.W + 0.02, Y + 0.01,
                     Font_3,
                     Line_Grass);

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is

      use Flight.Aircraft;
      use Utility.Strings;

   begin

      if Btn_Next.Contains (X, Y) then

         Next_Aircraft;

         Btn_Model.Set_Label (Trim (This_Aircraft.Model) & "/" & Trim (This_Aircraft.Registration));

         Display.Refresh := True;

      else

         for I in Mass_Point_Range loop

            if This_Aircraft.Mass_Points (I).Active then

               if Mass_Controls (I).Btn_Mass.Contains (X, Y) then

                  Mass_Focus := I;

                  Display.Refresh := True;

                  return;

               elsif Mass_Controls (I).Btn_Plus.Contains (X, Y) then

                  This_Aircraft.Mass_Points (I).Mass := Float'Min (150.0, This_Aircraft.Mass_Points (I).Mass + 1.0);

                  Flight.Aircraft.Recalculate_Mass;

                  Display.Refresh := True;

                  return;

               elsif Mass_Controls (I).Btn_Less.Contains (X, Y) then

                  This_Aircraft.Mass_Points (I).Mass := Float'Max (0.0, This_Aircraft.Mass_Points (I).Mass - 1.0);

                  Flight.Aircraft.Recalculate_Mass;

                  Display.Refresh := True;

                  return;

               end if;

            end if;

         end loop;

      end if;

   end Screen_Pressed;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Handles a key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin

      case Key is

         when Panel_Wheel_Button =>

            for I in Mass_Point_Range loop

               if I > Mass_Focus and then This_Aircraft.Mass_Points (I).Active then

                  Mass_Focus := I;

                  Display.Refresh := True;

                  return;

               end if;

            end loop;

            Mass_Focus := Mass_Point_Range'First;

            Display.Refresh := True;

         when Panel_Wheel_Right =>

            This_Aircraft.Mass_Points (Mass_Focus).Mass := Float'Min (150.0, This_Aircraft.Mass_Points (Mass_Focus).Mass + 1.0);

            Flight.Aircraft.Recalculate_Mass;

            Display.Refresh := True;

         when Panel_Wheel_Left =>

            This_Aircraft.Mass_Points (Mass_Focus).Mass := Float'Max (0.0, This_Aircraft.Mass_Points (Mass_Focus).Mass - 1.0);

            Flight.Aircraft.Recalculate_Mass;

            Display.Refresh := True;

         when others =>

            null;

      end case;

   end Key_Changed;
   -----------------------------------------------------------------------------




end Display.Pages.Glider;
--------------------------------------------------------------------------------
