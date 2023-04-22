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

-- Gnav
with Flight.Parsing;
with Flight.Signal;
use  Flight.Signal;
with Flight.Stream;
with Gl.Colors;
use  Gl.Colors;
with Gl.Fonts;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Panels.Stream is

   -- Widgets
   ---------------------------------

   Pnl_Stream    : Panel_Record;

   Btn_Sats      : Button_Record;

   Btn_Source    : Button_Record;

   -- Fonts
   ---------------------------------

   Font_1        : Gl.Fonts.Font_Style_Record := (Width     => 0.010,
                                                  Height    => 0.030,
                                                  Space     => 0.004,
                                                  Rendering => Gl.Fonts.Font_Glow,
                                                  Thickness => Gl.Fonts.Font_Regular);
   -- Internal variables
   ---------------------------------

   Show_Constellation : Boolean := False;

   Selected_Source    : Satellite_Kinds := Satellite_Kinds'First;

   --===========================================================================
   --
   --===========================================================================
   procedure Init (X, W : Float) is

      Allocation : Allocation_Record;

   begin

      Allocation.X := X;
      Allocation.Y := 0.010;
      Allocation.W := W;
      Allocation.H := 0.850;

      Pnl_Stream.Set_Allocation (Allocation);

      Pnl_Stream.Set_Background_Color (Color_Black);

      Pnl_Stream.Set_Transparency (0.60);

      Pnl_Stream.Set_Show_Border (True);

      Pnl_Stream.Set_Label ("STREAMING", Label_Left);

      Pnl_Stream.Set_Font_Size (0.03, 0.25);

      Pnl_Stream.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      --

      Allocation.X := X + 0.010;
      Allocation.Y := 0.330;
      Allocation.W := W - 0.020;
      Allocation.H := 0.100;

      Btn_Sats.Set_Allocation (Allocation);

      Btn_Sats.Set_Label ("SATS");

      Btn_Sats.Set_Background_Color (Color_Sky);

      Btn_Sats.Set_Font_Size (0.4, 0.3);

      --

      Allocation.X := X + 0.010;
      Allocation.Y := Pnl_Stream.Get_Allocation.Y + Pnl_Stream.Get_Allocation.H - 0.14;
      Allocation.W := W - 0.020;
      Allocation.H := 0.100;

      Btn_Source.Set_Allocation (Allocation);

      Btn_Source.Set_Label ("GPS");

      Btn_Source.Set_Background_Color (Color_Sky);

      Btn_Source.Set_Font_Size (0.4, 0.3);

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      use Gl.Fonts;

      X, Y  : Float;

   begin

      -- Acquisition panel
      --------------------------------------------

      Pnl_Stream.Draw;

      if Show_Constellation then

         Btn_Source.Draw;

         X := Btn_Source.Get_Allocation.X + 0.01;

         Y := Btn_Source.Get_Allocation.Y - 1.8 * Font_1.Height;

         if Number_Of_Satellites (Selected_Source) > 0 then

            for Sat of Satellites (Selected_Source) loop

               if Sat.Valid then

                  Draw (Sat.Id & ">",
                        X, Y,
                        Font_1,
                        Line_Cyan,
                        Alignment_LL);

                  if Sat.Strength < 5 then

                     Draw (Natural'Image (Sat.Strength),
                           X + 0.12, Y,
                           Font_1,
                           Line_Gray,
                           Alignment_LR);

                  elsif Sat.Strength < 20 then

                     Draw (Natural'Image (Sat.Strength),
                           X + 0.12, Y,
                           Font_1,
                           Line_Yellow,
                           Alignment_LR);

                  else

                     Draw (Natural'Image (Sat.Strength),
                           X + 0.12, Y,
                           Font_1,
                           Line_Green,
                           Alignment_LR);

                  end if;

                  Draw (Natural'Image (Sat.Azimuth),
                        X + 0.20, Y,
                        Font_1,
                        Line_Gray,
                        Alignment_LR);

                  Draw (Natural'Image (Sat.Elevation),
                        X + 0.28, Y,
                        Font_1,
                        Line_Gray,
                        Alignment_LR);

                  Y := Y - 1.9 * Font_1.Height;

               end if;

            end loop;

         else

            Draw ("NO SATELLITES",
                  X, Y,
                  Font_1,
                  Line_Red);

         end if;

      else

         -- Source kind

         X := Pnl_Stream.Get_Allocation.X + 0.02;

         Y := Pnl_Stream.Get_Allocation.Y + Pnl_Stream.Get_Allocation.H - 0.1;

         Draw ("SOURCE:",
               X, Y,
               Font_1,
               Line_Gray);

         X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;

         case Flight.Stream.Get_Source_Kind is

         when Flight.Stream.Stream_Source_File =>

            Draw ("RECORDING",
                  X, Y,
                  Font_1,
                  Line_Gray,
                  Alignment_LR);

         when Flight.Stream.Stream_Source_Udp =>

            Draw ("UDP",
                  X, Y,
                  Font_1,
                  Line_Gray,
                  Alignment_LR);

         when Flight.Stream.Stream_Source_Serial =>

            Draw ("SERIAL",
                  X, Y,
                  Font_1,
                  Line_Gray,
                  Alignment_LR);

         when others =>

            Draw ("NOT SET",
                  X, Y,
                  Font_1,
                  Line_Gray,
                  Alignment_LR);

         end case;

         -- Data formating sytem

         X := Pnl_Stream.Get_Allocation.X + 0.02;

         Y := Y - 2.0 * Font_1.Height;

         Draw ("FORMAT:",
               X, Y,
               Font_1,
               Line_Gray);

         X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;

         Draw (Flight.Stream.Get_Protocol_Key (Flight.Stream.Get_Protocol_Kind),
               X, Y,
               Font_1,
               Line_Gray,
               Alignment_LR);

         --

         X := Pnl_Stream.Get_Allocation.X + 0.02;

         Y := Y - 2.0 * Font_1.Height;

         Draw ("STATUS:",
               X, Y,
               Font_1,
               Line_Gray); -- OK / ERROR

         X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;

         if Flight.Stream.Is_Active then

            Draw ("ACTIVE",
                  X, Y,
                  Font_1,
                  Line_Green,
                  Alignment_LR);

         elsif Blink then

            Draw ("ERROR",
                  X, Y,
                  Font_1,
                  Line_Red,
                  Alignment_LR);

         end if;

         -- Incoming data rate

         X := Pnl_Stream.Get_Allocation.X + 0.02;

         Y := Y - 2.0 * Font_1.Height;

         Draw ("RATE:",
               X, Y,
               Font_1,
               Line_Gray); -- 4P/S

         X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;

         if Flight.Stream.Get_Rate > 0 then

            Draw (Natural'Image (Flight.Stream.Get_Rate),
                  X, Y,
                  Font_1,
                  Line_Green,
                  Alignment_LR);

         else

            Draw ("NO INPUT",
                  X, Y,
                  Font_1,
                  Line_Red,
                  Alignment_LR);

         end if;

         -- GPS fix status

         X := Pnl_Stream.Get_Allocation.X + 0.02;

         Y := Y - 2.0 * Font_1.Height;

         Draw ("GPS:",
               X, Y,
               Font_1,
               Line_Gray);

         X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;

         if Flight.Data.Age (Flight.Field_Position)  < 3.0 then

            Draw ("OK",
                  X, Y,
                  Font_1,
                  Line_Green,
                  Alignment_LR);

         else

            Draw ("NO FIX",
                  X, Y,
                  Font_1,
                  Line_Red,
                  Alignment_LR);

         end if;

         -- Total number of satellites in view

         declare
            Total : Natural := 0;
         begin

            X := Pnl_Stream.Get_Allocation.X + 0.02;

            Y := Y - 2.0 * Font_1.Height;

            for K in Satellite_Kinds loop

               Total := Total + Number_Of_Satellites (K);

            end loop;

            if Total > 0 then

               Draw ("SATELLITES:",
                     X, Y,
                     Font_1,
                     Line_Gray);

               X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;

               Draw (Natural'Image (Total),
                     X, Y,
                     Font_1,
                     Line_Green,
                     Alignment_LR);

            else

               Draw ("NO SATELLITES",
                     X, Y,
                     Font_1,
                     Line_Red);

            end if;

         end;

         Btn_Sats.Draw;

         X := Pnl_Stream.Get_Allocation.X + 0.02;

         Y := Btn_Sats.Get_Allocation.Y - 2.0 * Font_1.Height;

         if Flight.Parsing.Error_Detected then

            declare
               Color : Line_Color_Record;
            begin

               if Flight.Parsing.Recent_Error then
                  Color := Line_Red;
               else
                  Color := Line_Gray;
               end if;

               Draw ("PARSING ERROR",
                     X, Y,
                     Font_1,
                     Color,
                     Alignment_LL);

            end;

         else

            Draw ("DATA OK",
                  X, Y,
                  Font_1,
                  Line_Gray,
                  Alignment_LL);

         end if;

         Y := Y - 2.0 * Font_1.Height;

         if Flight.Stream.Recording then

            Draw ("RECORDING",
                  X, Y,
                  Font_1,
                  Line_Green,
                  Alignment_LL);

         else

            Draw ("NOT RECORDING",
                  X, Y,
                  Font_1,
                  Line_Gray,
                  Alignment_LL);

         end if;

      end if;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Update_Source_Button is
   begin

      case Selected_Source is

         when Sat_Gps =>

            Btn_Source.Set_Label ("GPS");

         when Sat_Galileo =>

            Btn_Source.Set_Label ("GALILEO");

         when Sat_Glonass =>

            Btn_Source.Set_Label ("GLONASS");

         when Sat_Beidou =>

            Btn_Source.Set_Label ("BEIDOU");

      end case;

   end Update_Source_Button;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin

      if Show_Constellation and then Btn_Source.Contains (X, Y) then

         if Selected_Source = Satellite_Kinds'Last then

            Show_Constellation := False;

            Refresh            := True;

         else

            Selected_Source := Satellite_Kinds'Succ (Selected_Source);

            Refresh         := True;

            Update_Source_Button;

         end if;

      elsif Btn_Sats.Contains (X, Y) then

         Show_Constellation := True;

         Selected_Source    := Satellite_Kinds'First;

         Refresh            := True;

         Update_Source_Button;

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



end Display.Panels.Stream;
--------------------------------------------------------------------------------
