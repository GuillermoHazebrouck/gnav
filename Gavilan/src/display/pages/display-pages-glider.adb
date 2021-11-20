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
with Gl.Fonts;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Colors;
use  Utility.Colors;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Glider is

   -- Fonts
   ---------------------------------
   Font_1 : Gl.Fonts.Font_Style_Record := (Width     => 0.015,
                                           Height    => 0.030,
                                           Space     => 0.008,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular,
                                           Line_R    => 1.0,
                                           Line_G    => 1.0,
                                           Line_B    => 1.0,
                                           Glow_R    => 0.1,
                                           Glow_G    => 0.1,
                                           Glow_B    => 0.1);

   -- Fonts
   ---------------------------------
   Font_2 : Gl.Fonts.Font_Style_Record := (Width     => 0.012,
                                           Height    => 0.024,
                                           Space     => 0.008,
                                           Rendering => Gl.Fonts.Font_Glow,
                                           Thickness => Gl.Fonts.Font_Regular,
                                           Line_R    => 0.0,
                                           Line_G    => 1.0,
                                           Line_B    => 1.0,
                                           Glow_R    => 0.1,
                                           Glow_G    => 0.1,
                                           Glow_B    => 0.8);

   M : constant Dimension_Float := 0.01;
   H : constant Dimension_Float := 0.10;
   W : constant Dimension_Float := 0.15;

   -- Page widgets
   ---------------------------------

   Btn_Model    : Button_Record;

   type Mass_Control is record

      Btn_Mass : Button_Record;

      Btn_Plus : Button_Record;

      Btn_Less : Button_Record;

   end record;

   Mass_Controls : array (Flight.Aircraft.Mass_Point_Range) of Mass_Control;

   --===========================================================================
   --
   --===========================================================================
   procedure Init is

      use Utility.Strings;
      use Flight.Aircraft;

      Allocation : Allocation_Record;

   begin

      Allocation.X := 0.15 + 2.0 * M;

      Allocation.H := H;

      Allocation.Y := 1.0 - (H + M);

      Allocation.W := W;

      -- Page setup
      ------------------------------------------------------

      Allocation.X := M;

      Allocation.Y := Allocation.Y - (H + M);

      Allocation.W := 1.0 - 2.0 * M;

      Allocation.H := H;

      Btn_Model.Set_Allocation (Allocation);

      Btn_Model.Set_Label (Trim (Flight.Aircraft.Model) & "/" & Trim (Flight.Aircraft.Registration));

      Btn_Model.Set_Background_Color (Color_Black);

      Btn_Model.Set_Border_Color (Color_Gray_3);

      Btn_Model.Set_Label_Color (Color_Sky, Color_Blue);

      Btn_Model.Set_Font_Size (0.6, 0.4);

      for I in Flight.Aircraft.Mass_Point_Range loop

         Mass_Controls (I).Btn_Mass.Set_Background_Color (Color_Black);

         Mass_Controls (I).Btn_Mass.Set_Border_Color (Color_Gray_3);

         Mass_Controls (I).Btn_Mass.Set_Label_Color (Color_Sky, Color_Blue);

         Mass_Controls (I).Btn_Mass.Set_Font_Size (0.6, 0.4);

         Allocation.X := (Font_1.Width + Font_1.Space) * Float (String_12'Length) + M;

         Allocation.Y := Allocation.Y - (H + M);

         Allocation.W := 0.12;

         Allocation.H := H;

         Mass_Controls (I).Btn_Mass.Set_Allocation (Allocation);

         --

         Mass_Controls (I).Btn_Plus.Set_Label ("+");

         Mass_Controls (I).Btn_Plus.Set_Background_Color (Color_Sky);

         Mass_Controls (I).Btn_Plus.Set_Border_Color (Color_Blue);

         Mass_Controls (I).Btn_Plus.Set_Label_Color (Color_White);

         Mass_Controls (I).Btn_Plus.Set_Font_Size (0.7, 0.5);

         Allocation.X := Allocation.X + Allocation.W + 2.0 * (M + Font_1.Width + Font_1.Space);

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
   procedure Draw (Width, Height : Float) is

      use Flight.Aircraft;
      use Utility.Strings;

      X : Float;
      Y : Float;

   begin

      Btn_Model.Draw;

      for I in Mass_Point_Range loop

         if Mass_Points (I).Active then

            Mass_Controls (I).Btn_Mass.Set_Label (Trim (Integer'Image (Integer (Mass_Points (I).Mass))));

            Y := Mass_Controls (I).Btn_Mass.Get_Allocation.Y + 0.5 * (Mass_Controls (I).Btn_Mass.Get_Allocation.H - Font_1.Height);

            X := 2.0 * M;

            Gl.Fonts.Draw (Trim (Mass_Points (I).Label),
                           X,
                           Y,
                           Font_1);

            X := Mass_Controls (I).Btn_Mass.Get_Allocation.X + Mass_Controls (I).Btn_Mass.Get_Allocation.W + M;

            Gl.Fonts.Draw ("KG",
                           X,
                           Y,
                           Font_1);

            Mass_Controls (I).Btn_Mass.Draw;

            Mass_Controls (I).Btn_Plus.Draw;

            Mass_Controls (I).Btn_Less.Draw;

         end if;

      end loop;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is

      use Flight.Aircraft;

   begin

      for I in Mass_Point_Range loop

         if Mass_Points (I).Active then

            if Mass_Controls (I).Btn_Plus.Contains (X, Y) then

               Mass_Points (I).Mass := Float'Min (150.0, Mass_Points (I).Mass + 1.0);

               Flight.Aircraft.Recalculate_Mass;

               Display.Refresh := True;

               return;

            elsif Mass_Controls (I).Btn_Less.Contains (X, Y) then

               Mass_Points (I).Mass := Float'Max (0.0, Mass_Points (I).Mass - 1.0);

               Flight.Aircraft.Recalculate_Mass;

               Display.Refresh := True;

               return;

            end if;

         end if;

      end loop;

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




end Display.Pages.Glider;
--------------------------------------------------------------------------------
