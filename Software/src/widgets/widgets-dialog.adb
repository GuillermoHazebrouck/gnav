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
with Gl.Colors;
use  Gl.Colors;
with Gl.Fonts;
use  Gl.Fonts;
with Utility.Strings;
use  Utility.Strings;
with Widgets.Widget;
use  Widgets.Widget;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Widgets.Dialog is

   Pnl_Background : Panel_Record;

   Pnl_Dialog     : Panel_Record;

   Btn_Ok         : Button_Record;

   Btn_Cancel     : Button_Record;

   Visible        : Boolean := False;

   Label          : String (1..30);

   Font           : Font_Style_Record := (Width     => 0.010,
                                          Height    => 0.033,
                                          Space     => 0.004,
                                          Rendering => Gl.Fonts.Font_Glow,
                                          Thickness => Gl.Fonts.Font_Regular);

   Current_Handler    : Action_Handlers := null;

   Preselected_Action : Dialog_Result_Kind := Dialog_Cancel;

   --===========================================================================
   --
   --===========================================================================
   procedure Init is

      Allocation : Allocation_Record;

   begin

      Allocation.X := 0.0;
      Allocation.Y := 0.0;
      Allocation.W := 1.0;
      Allocation.H := 1.0;

      Pnl_Background.Set_Allocation (Allocation);

      Pnl_Background.Set_Background_Color (Color_Black);

      Pnl_Background.Set_Transparency (0.40);

      Pnl_Background.Set_Show_Border (True);

      Pnl_Background.Set_Font_Size (0.03, 0.25);

      Pnl_Background.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Allocation.X := 0.2;
      Allocation.Y := 0.2;
      Allocation.W := 0.6;
      Allocation.H := 0.6;

      Pnl_Dialog.Set_Allocation (Allocation);

      Pnl_Dialog.Set_Background_Color (Color_Black);

      Pnl_Dialog.Set_Transparency (0.70);

      Pnl_Dialog.Set_Show_Border (True);

      Pnl_Dialog.Set_Border_Color (Color_White);

      Pnl_Dialog.Set_Label ("ATTENTION", Label_Left);

      Pnl_Dialog.Set_Font_Size (0.03, 0.25);

      Pnl_Dialog.Set_Label_Color (Color_Red);

      Allocation.X := Allocation.X + 0.05;
      Allocation.Y := Allocation.Y + 0.10;
      Allocation.W := 0.15;
      Allocation.H := 0.10;

      Btn_Ok.Set_Allocation (Allocation);

      Btn_Ok.Set_Label_Color (Color_White);

      Btn_Ok.Set_Font_Size (0.4, 0.3);

      Btn_Ok.Set_Background_Color (Color_Green);

      Btn_Ok.Set_Label ("OK");

      Allocation.X := 1.0 - Allocation.X - Allocation.W;

      Btn_Cancel.Set_Allocation (Allocation);

      Btn_Cancel.Set_Label_Color (Color_White);

      Btn_Cancel.Set_Font_Size (0.4, 0.3);

      Btn_Cancel.Set_Background_Color (Color_Red);

      Btn_Cancel.Set_Label ("CANCEL");

      --

      Preselect_Action (Dialog_Cancel);

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Confirm (Message : String; Handler : Action_Handlers) is
   begin

      -- Cancel active handler
      -------------------------------------
      if Current_Handler /= null then

         Current_Handler (Dialog_Cancel);

      end if;

      -- Replace handler
      -------------------------------------
      Override (Label, Message);

      Visible := True;

      Preselect_Action (Dialog_Cancel);

      Current_Handler := Handler;

   end Confirm;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Is_Open return Boolean is
   begin

      return Visible;

   end Is_Open;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is
   begin

      if Visible then

         Pnl_Background.Draw;

         Pnl_Dialog.Draw;

         Btn_Ok.Draw;

         Btn_Cancel.Draw;

         Draw (Trim (Label), 0.5, 0.55, Font, Line_White, Alignment_CC);

      end if;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Handle_Action (X, Y : Float) is
   begin

      if Visible then

         if Btn_Ok.Contains (X, Y) then

            Visible := False;

            if Current_Handler /= null then

               Current_Handler (Dialog_Ok);

               Current_Handler := null;

            end if;

         elsif Btn_Cancel.Contains (X, Y) then

            Visible := False;

            if Current_Handler /= null then

               Current_Handler (Dialog_Cancel);

               Current_Handler := null;

            end if;

         end if;

      end if;

   end Handle_Action;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Preselect_Action (Value : Dialog_Result_Kind) is
   begin

      Preselected_Action := Value;

      case Preselected_Action is

         when Dialog_Ok =>

            Btn_Ok.Set_Border_Color     (Color_Grass);

            Btn_Ok.Set_Label_Color      (Color_Green);

            Btn_Ok.Set_Background_Color (Color_Greeny);

            Btn_Cancel.Set_Border_Color     (Color_Gray_2);

            Btn_Cancel.Set_Label_Color      (Color_Gray_4);

            Btn_Cancel.Set_Background_Color (Color_Gray_3);

         when Dialog_Cancel =>

            Btn_Cancel.Set_Border_Color     (Color_Reddish);

            Btn_Cancel.Set_Label_Color      (Color_Red);

            Btn_Cancel.Set_Background_Color (Color_Reddish);

            Btn_Ok.Set_Border_Color     (Color_Gray_2);

            Btn_Ok.Set_Label_Color      (Color_Gray_4);

            Btn_Ok.Set_Background_Color (Color_Gray_3);

      end case;

   end Preselect_Action;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Expedite_Action is
   begin

      if Current_Handler /= null then

         Current_Handler (Preselected_Action);

         Current_Handler := null;

      end if;

      Preselect_Action (Dialog_Cancel);

      Visible := False;

   end Expedite_Action;
   -----------------------------------------------------------------------------


end Widgets.Dialog;
--------------------------------------------------------------------------------
