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
with Gl.Colors;
use  Gl.Colors;
with Gl.Fonts;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Atmosphere;
use  Utility.Atmosphere;
with Utility.Strings;
use  Utility.Strings;
with Utility.Units;
use  Utility.Units;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Panels.Baro is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Widgets
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Pnl_Qnh      : Panel_Record;

   Btn_Qnh      : Button_Record;

   Btn_Qnh_Plus : Button_Record;

   Btn_Qnh_Min  : Button_Record;

   Btn_Up       : Button_Record;

   Btn_Down     : Button_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- FL/Altitude table for the given QNH
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Table_Record is record

      Level    : Altitude_Strings;

      Altitude : Altitude_Strings;

   end record;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Table : array (1..27) of Table_Record := (("FL010  ", No_Altitude_String),
                                             ("FL015  ", No_Altitude_String),
                                             ("FL020  ", No_Altitude_String),
                                             ("FL025  ", No_Altitude_String),
                                             ("FL030  ", No_Altitude_String),
                                             ("FL035  ", No_Altitude_String),
                                             ("FL040  ", No_Altitude_String),
                                             ("FL045  ", No_Altitude_String),
                                             ("FL050  ", No_Altitude_String),
                                             ("FL055  ", No_Altitude_String),
                                             ("FL060  ", No_Altitude_String),
                                             ("FL065  ", No_Altitude_String),
                                             ("FL070  ", No_Altitude_String),
                                             ("FL075  ", No_Altitude_String),
                                             ("FL080  ", No_Altitude_String),
                                             ("FL090  ", No_Altitude_String),
                                             ("FL100  ", No_Altitude_String),
                                             ("FL110  ", No_Altitude_String),
                                             ("FL120  ", No_Altitude_String),
                                             ("FL130  ", No_Altitude_String),
                                             ("FL140  ", No_Altitude_String),
                                             ("FL150  ", No_Altitude_String),
                                             ("FL160  ", No_Altitude_String),
                                             ("FL170  ", No_Altitude_String),
                                             ("FL180  ", No_Altitude_String),
                                             ("FL190  ", No_Altitude_String),
                                             ("FL200  ", No_Altitude_String));

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Table offset
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Offset : Natural := 0;

   --===========================================================================
   --
   --===========================================================================
   procedure Regenerate_Table is
   begin

      for I in Table'Range loop

         Table (I).Altitude := To_Altitude (Table (I).Level, True);

      end loop;

   end Regenerate_Table;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Update_Qnh_Value is
   begin

      Btn_Qnh.Set_Label (Float_Image (Get_Qnh, 0));

      if Get_Qnh_Valid then

         Btn_Qnh.Set_Background_Color (Color_Pink);

         Btn_Qnh.Set_Border_Color (Color_Magenta);

         Btn_Qnh.Set_Label_Color (Line_Cyan);

      else

         Btn_Qnh.Set_Background_Color (Color_Gray_3);

         Btn_Qnh.Set_Border_Color (Color_Gray_4);

         Btn_Qnh.Set_Label_Color (Line_Gray);

      end if;

      Regenerate_Table;

   end Update_Qnh_Value;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Init (X, W : Float) is

      Allocation : Allocation_Record;

   begin

      -- QNH panel
      ------------------------------------------------------

      Allocation.X := X;
      Allocation.Y := 0.010;
      Allocation.W := W;
      Allocation.H := 0.850;

      Pnl_Qnh.Set_Allocation (Allocation);

      Pnl_Qnh.Set_Background_Color (Color_Black);

      Pnl_Qnh.Set_Transparency (0.60);

      Pnl_Qnh.Set_Show_Border (True);

      Pnl_Qnh.Set_Label ("QNH / FL", Label_Left);

      Pnl_Qnh.Set_Font_Size (0.03, 0.25);

      Pnl_Qnh.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      -- Decrease QNH button

      Btn_Qnh_Min.Set_Label ("-");

      Btn_Qnh_Min.Set_Background_Color (Color_Sky);

      Btn_Qnh_Min.Set_Border_Color (Color_Blue);

      Btn_Qnh_Min.Set_Label_Color (Color_White);

      Btn_Qnh_Min.Set_Font_Size (0.7, 0.5);

      Allocation.X := X + 0.01;
      Allocation.Y := Allocation.Y + Allocation.H - 0.15;
      Allocation.W := 0.07;
      Allocation.H := 0.10;

      Btn_Qnh_Min.Set_Allocation (Allocation);

      -- Increase QNH button

      Btn_Qnh_Plus.Set_Label ("+");

      Btn_Qnh_Plus.Set_Background_Color (Color_Sky);

      Btn_Qnh_Plus.Set_Border_Color (Color_Blue);

      Btn_Qnh_Plus.Set_Label_Color (Color_White);

      Btn_Qnh_Plus.Set_Font_Size (0.7, 0.5);

      Allocation.X := X + W - 0.08;

      Btn_Qnh_Plus.Set_Allocation (Allocation);

      -- QNH button

      Update_Qnh_Value;

      Btn_Qnh.Set_Font_Size (0.5, 0.3, 0.5);

      Allocation.X := Btn_Qnh_Min.Get_Allocation.X  + Btn_Qnh_Min.Get_Allocation.W + 0.01;
      Allocation.W := Btn_Qnh_Plus.Get_Allocation.X - Allocation.X - 0.01;

      Btn_Qnh.Set_Allocation (Allocation);

      On_Qnh_Changed.Connect (Update_Qnh_Value'Access);

      -- Move QNH list up

      Btn_Down.Set_Label ("}");

      Btn_Down.Set_Background_Color (Color_Sky);

      Btn_Down.Set_Border_Color (Color_Blue);

      Btn_Down.Set_Label_Color (Color_White);

      Btn_Down.Set_Font_Size (0.7, 0.5);

      Allocation.X := Btn_Qnh_Plus.Get_Allocation.X;
      Allocation.Y := Btn_Qnh_Plus.Get_Allocation.Y - 1.2 * Btn_Qnh_Min.Get_Allocation.H;
      Allocation.W := 0.07;
      Allocation.H := 0.10;

      Btn_Down.Set_Allocation (Allocation);

      -- Move QNH list down

      Btn_Up.Set_Label ("{");

      Btn_Up.Set_Background_Color (Color_Sky);

      Btn_Up.Set_Border_Color (Color_Blue);

      Btn_Up.Set_Label_Color (Color_White);

      Btn_Up.Set_Font_Size (0.7, 0.5);

      Allocation.Y := Pnl_Qnh.Get_Allocation.Y + 0.02;

      Btn_Up.Set_Allocation (Allocation);

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      use Gl.Fonts;

      X, Y : Float;

      Font : Font_Style_Record := (Width     => 0.010,
                                   Height    => 0.035,
                                   Space     => 0.008,
                                   Rendering => Gl.Fonts.Font_Glow,
                                   Thickness => Gl.Fonts.Font_Regular);

      Color  : Line_Color_Record := Line_Gray;
      Switch : Boolean           := False;

   begin

      Pnl_Qnh.Draw;

      Btn_Qnh_Min.Draw;

      Btn_Qnh_Plus.Draw;

      Btn_Qnh.Draw;

      X := Btn_Qnh_Min.Get_Allocation.X + 0.01;
      Y := Btn_Qnh_Min.Get_Allocation.Y - 0.04;

      for I in 1 + Offset .. 10 + Offset loop

         if Utility.Atmosphere.Get_Qnh_Valid then

            if Switch then
               Color := Line_Yellow;
            else
               Color := Line_White;
            end if;
            Switch := not Switch;

         end if;

         Gl.Fonts.Draw (Table (I).Level,
                        X         => X,
                        Y         => Y,
                        Style     => Font,
                        Color     => Color,
                        Alignment => Alignment_TL);

         Gl.Fonts.Draw (Trim (Table (I).Altitude),
                        X         => X + 0.20,
                        Y         => Y,
                        Style     => Font,
                        Color     => Color,
                        Alignment => Alignment_TR);

         Y := Y - 0.065;

      end loop;

      -- Up and down bottons

      Btn_Up.Draw;

      Btn_Down.Draw;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin

      -- Use buttons to adapt QNH

      if Pnl_Qnh.Contains (X, Y) then

         if Btn_Qnh.Contains (X, Y) then

            -- Toggle QNH valid

            Utility.Atmosphere.Set_Qnh (Get_Qnh, not Get_Qnh_Valid);

            Refresh := True;

         elsif Btn_Qnh_Plus.Contains (X, Y) then

            -- Increase QNH

            Utility.Atmosphere.Set_Qnh (Get_Qnh + 1.0, True);

            Refresh := True;

         elsif Btn_Qnh_Min.Contains (X, Y) then

            -- Decrease QNH

            Utility.Atmosphere.Set_Qnh (Get_Qnh - 1.0, True);

            Refresh := True;

         elsif Btn_Up.Contains (X, Y) then

            Offset := Natural'Min (Table'Last - 10, Offset + 1);

            Refresh := True;

         elsif Btn_Down.Contains (X, Y) then

            Offset := Natural'Max (0, Offset - 1);

            Refresh := True;

         end if;

      end if;

   end Screen_Pressed;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Handles a key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin

      -- Use knob to adapt QNH

      case Key is

         when Panel_Wheel_Left =>

            -- Decrease QNH

            if Get_Qnh_Valid then

               Utility.Atmosphere.Set_Qnh (Get_Qnh - 1.0, True);

               Refresh := True;

            end if;

         when Panel_Wheel_Right =>

            -- Increase QNH

            if Get_Qnh_Valid then

               Utility.Atmosphere.Set_Qnh (Get_Qnh + 1.0, True);

               Refresh := True;

            end if;

         when Panel_Wheel_Button =>

            Utility.Atmosphere.Set_Qnh (Get_Qnh, not Get_Qnh_Valid);

            Refresh := True;

         when others =>
            null;

      end case;

   end Key_Changed;
   -----------------------------------------------------------------------------



end Display.Panels.Baro;
--------------------------------------------------------------------------------
