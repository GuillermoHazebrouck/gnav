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
with Flight;
with Flight.Plan;
with Display.Route;
with Math.Vector2;
use  Math.Vector2;
with Maps;
with Maps.Layers;
with Maps.Terrain;
with Utility.Colors;
use  Utility.Colors;
with Utility.Strings;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Keyboard;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Route_Edition is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The map view on the editor
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   View : Maps.Map_View_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The step of each move in km
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Step : Long_Float := 0.1;

   -- Map view adaptation buttons
   ---------------------------------

   Btn_Zoom_In         : Button_Record;

   Btn_Zoom_Out        : Button_Record;

   Btn_View_West       : Button_Record;

   Btn_View_East       : Button_Record;

   Btn_View_South      : Button_Record;

   Btn_View_North      : Button_Record;

   Btn_Center_Home     : Button_Record;

   Btn_Center_Wyptn    : Button_Record;

   Btn_Center_Aircraft : Button_Record;

   Ent_Route           : aliased Button_Record;

   Btn_Route_Next      : Button_Record;

   Btn_Route_Previous  : Button_Record;

   Btn_Route_Append    : Button_Record;

   Btn_Route_Remove    : Button_Record;

   Ent_Waypoint        : aliased Button_Record;

   Btn_Wypnt_Next      : Button_Record;

   Btn_Wypnt_Previous  : Button_Record;

   Btn_Wypnt_Prepend   : Button_Record;

   Btn_Wypnt_Append    : Button_Record;

   Btn_Wypnt_Remove    : Button_Record;

   Btn_Wypnt_Left      : Button_Record;

   Btn_Wypnt_Right     : Button_Record;

   Btn_Wypnt_Up        : Button_Record;

   Btn_Wypnt_Down      : Button_Record;

   Btn_Wypnt_Step      : Button_Record;

   -- Name edition system
   -----------------------------------------------------------------------------

   Text_Focus          : access Button_Record := null;

   Ent_Route_Access    : constant access Button_Record := Ent_Route'Access;

   Ent_Waypoint_Access : constant access Button_Record := Ent_Waypoint'Access;

   --===========================================================================
   -- Manages the focused edit
   --===========================================================================
   procedure Change_Focus (New_Focus : access Button_Record) is
   begin

      if New_Focus /= Text_Focus then

         if Text_Focus /= null then

            Text_Focus.Set_Background_Color (Color_Gray_4);

            Refresh := True;

         end if;

         Text_Focus := New_Focus;

         if Text_Focus /= null then

            Text_Focus.Set_Background_Color (Color_Sky);

            Refresh := True;

         end if;

      end if;

   end Change_Focus;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Init is

      use Flight.Plan;
      use Utility.Strings;

      Allocation : Allocation_Record;

      H : Dimension_Float := 0.10;
      W : Dimension_Float := 0.06;
      C : Float           := 0.11;
      D : Float           := 0.18;

   begin

      View.Center      := Flight.Data.Position;

      View.Zoom        := 0.5 * (Maps.Upper_Zoom + Maps.Lower_Zoom);

      View.Shrink      := Float (Maps.Shrinkage (Flight.Data.Position.Lat));

      --####################################################
      --## Route selection functions
      --####################################################

      -- Route name
      ------------------------------------------------------

      H := 0.10;
      W := 0.21;

      Allocation.H := H;
      Allocation.W := W;
      Allocation.X := 1.0 - 1.1 * W;
      Allocation.Y := 1.0 - 1.1 * H;

      Ent_Route.Set_Label (Trim (Flight_Plan.Name));

      Ent_Route.Set_Allocation (Allocation);

      Ent_Route.Set_Background_Color (Color_Gray_4);

      Ent_Route.Set_Border_Color (Color_Black);

      Ent_Route.Set_Label_Color (Color_White, Color_Gray_3);

      Ent_Route.Set_Font_Size (0.4, 0.3);

      -- Next route
      ------------------------------------------------------

      W := 0.5 * Ent_Route.Get_Allocation.W;

      Allocation.W := W;
      Allocation.Y := Ent_Route.Get_Allocation.Y - H;

      Btn_Route_Previous.Set_Label ("<");

      Btn_Route_Previous.Set_Allocation (Allocation);

      Btn_Route_Previous.Set_Label_Color (Color_White);

      Btn_Route_Previous.Set_Transparency (0.3);

      Btn_Route_Previous.Set_Font_Size (0.5, 0.5);

      --

      Allocation.X := Btn_Route_Previous.Get_Allocation.X + W;

      Btn_Route_Next.Set_Label (">");

      Btn_Route_Next.Set_Allocation (Allocation);

      Btn_Route_Next.Set_Label_Color (Color_White);

      Btn_Route_Next.Set_Transparency (0.3);

      Btn_Route_Next.Set_Font_Size (0.5, 0.5);

      -- Append or remove route
      ------------------------------------------------------

      W := 0.5 * Ent_Route.Get_Allocation.W;

      Allocation.W := W;
      Allocation.X := Btn_Route_Previous.Get_Allocation.X;
      Allocation.Y := Btn_Route_Previous.Get_Allocation.Y - H - 0.01;

      Btn_Route_Append.Set_Label ("+");

      Btn_Route_Append.Set_Allocation (Allocation);

      Btn_Route_Append.Set_Label_Color (Color_Green);

      Btn_Route_Append.Set_Transparency (0.3);

      Btn_Route_Append.Set_Font_Size (0.5, 0.5);

      --

      Allocation.X := Btn_Route_Append.Get_Allocation.X + W;

      Btn_Route_Remove.Set_Label ("X");

      Btn_Route_Remove.Set_Allocation (Allocation);

      Btn_Route_Remove.Set_Label_Color (Color_Red);

      Btn_Route_Remove.Set_Transparency (0.3);

      Btn_Route_Remove.Set_Font_Size (0.3, 0.5);

      --####################################################
      --## Waypoint adaptation functions
      --####################################################

      -- Waypoint name
      ------------------------------------------------------

      H := Ent_Route.Get_Allocation.H;
      W := Ent_Route.Get_Allocation.W;

      Allocation.H := H;
      Allocation.W := W;
      Allocation.X := Ent_Route.Get_Allocation.X;
      Allocation.Y := Btn_Route_Append.Get_Allocation.Y - H - 0.03;

      Ent_Waypoint.Set_Label (Trim (Flight.Plan.Next_Waypoint.Name));

      Ent_Waypoint.Set_Allocation (Allocation);

      Ent_Waypoint.Set_Background_Color (Color_Gray_4);

      Ent_Waypoint.Set_Border_Color (Color_Black);

      Ent_Waypoint.Set_Label_Color (Color_White, Color_Gray_3);

      Ent_Waypoint.Set_Font_Size (0.4, 0.3);

      -- Previous waypoint
      ------------------------------------------------------

      W := 0.5 * Ent_Waypoint.Get_Allocation.W;

      Allocation.W := W;
      Allocation.Y := Ent_Waypoint.Get_Allocation.Y - H;

      Btn_Wypnt_Previous.Set_Label ("<");

      Btn_Wypnt_Previous.Set_Allocation (Allocation);

      Btn_Wypnt_Previous.Set_Label_Color (Color_White);

      Btn_Wypnt_Previous.Set_Transparency (0.3);

      Btn_Wypnt_Previous.Set_Font_Size (0.5, 0.5);

      -- Next waypoint
      ------------------------------------------------------

      Allocation.X := Btn_Wypnt_Previous.Get_Allocation.X + W;

      Btn_Wypnt_Next.Set_Label (">");

      Btn_Wypnt_Next.Set_Allocation (Allocation);

      Btn_Wypnt_Next.Set_Label_Color (Color_White);

      Btn_Wypnt_Next.Set_Transparency (0.3);

      Btn_Wypnt_Next.Set_Font_Size (0.5, 0.5);

      -- Prepend waypoint
      ------------------------------------------------------

      W := Ent_Waypoint.Get_Allocation.W / 3.0;

      Allocation.W := W;
      Allocation.X := Btn_Wypnt_Previous.Get_Allocation.X;
      Allocation.Y := Btn_Wypnt_Previous.Get_Allocation.Y - H - 0.01;

      Btn_Wypnt_Prepend.Set_Label ("<+");

      Btn_Wypnt_Prepend.Set_Allocation (Allocation);

      Btn_Wypnt_Prepend.Set_Label_Color (Color_Green);

      Btn_Wypnt_Prepend.Set_Transparency (0.3);

      Btn_Wypnt_Prepend.Set_Font_Size (0.3, 0.5);

      -- Append waypoint
      ------------------------------------------------------

      Allocation.X := Btn_Wypnt_Prepend.Get_Allocation.X + W;

      Btn_Wypnt_Append.Set_Label ("+>");

      Btn_Wypnt_Append.Set_Allocation (Allocation);

      Btn_Wypnt_Append.Set_Label_Color (Color_Green);

      Btn_Wypnt_Append.Set_Transparency (0.3);

      Btn_Wypnt_Append.Set_Font_Size (0.3, 0.5);

      -- Remove waypoint
      ------------------------------------------------------

      Allocation.X := Btn_Wypnt_Append.Get_Allocation.X + W;

      Btn_Wypnt_Remove.Set_Label ("X");

      Btn_Wypnt_Remove.Set_Allocation (Allocation);

      Btn_Wypnt_Remove.Set_Label_Color (Color_Red);

      Btn_Wypnt_Remove.Set_Transparency (0.3);

      Btn_Wypnt_Remove.Set_Font_Size (0.3, 0.5);

      -- Waypoint move arrows
      ------------------------------------------------------

      H := 0.10;
      W := 0.06;

      C := Ent_Waypoint.Get_Allocation.X + 0.5 * Ent_Waypoint.Get_Allocation.W;
      D := Btn_Wypnt_Prepend.Get_Allocation.Y - 1.5 * H - 0.01;

      -- Move down button
      ------------------------------------------------------

      Allocation.H := H;
      Allocation.W := W;
      Allocation.X := C - 0.5 * W;
      Allocation.Y := D - 1.5 * H;

      Btn_Wypnt_Down.Set_Label ("{");

      Btn_Wypnt_Down.Set_Allocation (Allocation);

      Btn_Wypnt_Down.Set_Label_Color (Color_Magenta);

      Btn_Wypnt_Down.Set_Transparency (0.3);

      Btn_Wypnt_Down.Set_Font_Size (0.5, 0.5);

      -- Move up button
      ------------------------------------------------------

      Allocation.X := C - 0.5 * W;
      Allocation.Y := D + 0.5 * H;

      Btn_Wypnt_Up.Set_Label ("}");

      Btn_Wypnt_Up.Set_Allocation (Allocation);

      Btn_Wypnt_Up.Set_Label_Color (Color_Magenta);

      Btn_Wypnt_Up.Set_Transparency (0.3);

      Btn_Wypnt_Up.Set_Font_Size (0.5, 0.5);

      -- Move right button
      ------------------------------------------------------

      Allocation.X := C + 0.5 * W;
      Allocation.Y := D - 0.5 * H;

      Btn_Wypnt_Right.Set_Label (">");

      Btn_Wypnt_Right.Set_Allocation (Allocation);

      Btn_Wypnt_Right.Set_Label_Color (Color_Magenta);

      Btn_Wypnt_Right.Set_Transparency (0.3);

      Btn_Wypnt_Right.Set_Font_Size (0.5, 0.5);

      -- Move left button
      ------------------------------------------------------

      Allocation.X := C - 1.5 * W;
      Allocation.Y := D - 0.5 * H;

      Btn_Wypnt_Left.Set_Label ("<");

      Btn_Wypnt_Left.Set_Allocation (Allocation);

      Btn_Wypnt_Left.Set_Label_Color (Color_Magenta);

      Btn_Wypnt_Left.Set_Transparency (0.3);

      Btn_Wypnt_Left.Set_Font_Size (0.5, 0.5);

      -- Change step button
      ------------------------------------------------------

      Allocation.H := H;
      Allocation.W := W;
      Allocation.X := C - 0.5 * W;
      Allocation.Y := D - 0.5 * H;

      Step := 1.0;

      Btn_Wypnt_Step.Set_Label ("1");

      Btn_Wypnt_Step.Set_Allocation (Allocation);

      Btn_Wypnt_Step.Set_Label_Color (Color_Magenta);

      Btn_Wypnt_Step.Set_Transparency (0.3);

      Btn_Wypnt_Step.Set_Font_Size (0.3, 0.2);

      --####################################################
      --## View adaptation functions
      --####################################################

      C := 0.10;
      D := 0.16;

      -- Down button
      ------------------------------------------------------

      H := 0.10;
      W := 0.06;

      Allocation.H := H;
      Allocation.W := W;
      Allocation.X := C - 0.5 * W;
      Allocation.Y := D - 1.5 * H;

      Btn_View_South.Set_Label ("S");

      Btn_View_South.Set_Allocation (Allocation);

      Btn_View_South.Set_Label_Color (Color_White);

      Btn_View_South.Set_Transparency (0.3);

      Btn_View_South.Set_Font_Size (0.5, 0.4);

      -- Up button
      ------------------------------------------------------

      Allocation.X := C - 0.5 * W;
      Allocation.Y := D + 0.5 * H;

      Btn_View_North.Set_Label ("N");

      Btn_View_North.Set_Allocation (Allocation);

      Btn_View_North.Set_Label_Color (Color_White);

      Btn_View_North.Set_Transparency (0.3);

      Btn_View_North.Set_Font_Size (0.5, 0.4);

      -- Right button
      ------------------------------------------------------

      Allocation.X := C + 0.5 * W;
      Allocation.Y := D - 0.5 * H;

      Btn_View_East.Set_Label ("E");

      Btn_View_East.Set_Allocation (Allocation);

      Btn_View_East.Set_Label_Color (Color_White);

      Btn_View_East.Set_Transparency (0.3);

      Btn_View_East.Set_Font_Size (0.5, 0.4);

      -- Left button
      ------------------------------------------------------

      Allocation.X := C - 1.5 * W;
      Allocation.Y := D - 0.5 * H;

      Btn_View_West.Set_Label ("W");

      Btn_View_West.Set_Allocation (Allocation);

      Btn_View_West.Set_Label_Color (Color_White);

      Btn_View_West.Set_Transparency (0.3);

      Btn_View_West.Set_Font_Size (0.5, 0.4);

      -- Zoom out button
      ------------------------------------------------------

      Allocation.X := C - 1.5 * W;
      Allocation.H := H;
      Allocation.Y := D - 1.5 * H;
      Allocation.W := W;

      Btn_Zoom_Out.Set_Label ("-");

      Btn_Zoom_Out.Set_Allocation (Allocation);

      Btn_Zoom_Out.Set_Label_Color (Color_White);

      Btn_Zoom_Out.Set_Font_Size (0.6);

      -- Zoom in button
      ------------------------------------------------------

      Btn_Zoom_In.Set_Label ("+");

      Allocation.Y := D + 0.5 * H;

      Btn_Zoom_In.Set_Allocation (Allocation);

      Btn_Zoom_In.Set_Label_Color (Color_White);

      Btn_Zoom_In.Set_Font_Size (0.5);

      -- Center on home (first waypoint)
      ------------------------------------------------------

      Allocation.X := C + 0.5 * W;
      Allocation.H := H;
      Allocation.Y := D - 1.5 * H;
      Allocation.W := W;

      Btn_Center_Home.Set_Label ("X");

      Btn_Center_Home.Set_Allocation (Allocation);

      Btn_Center_Home.Set_Label_Color (Color_Green);

      Btn_Center_Home.Set_Font_Size (0.2);

      -- Center on current waypoint
      ------------------------------------------------------

      Btn_Center_Wyptn.Set_Label ("X");

      Allocation.Y := D + 0.5 * H;

      Btn_Center_Wyptn.Set_Allocation (Allocation);

      Btn_Center_Wyptn.Set_Label_Color (Color_Magenta);

      Btn_Center_Wyptn.Set_Font_Size (0.2);

      -- Center on current position
      ------------------------------------------------------

      Btn_Center_Aircraft.Set_Label ("X");

      Allocation.X := C - 0.5 * W;

      Allocation.Y := D - 0.5 * H;

      Btn_Center_Aircraft.Set_Allocation (Allocation);

      Btn_Center_Aircraft.Set_Label_Color (Color_White);

      Btn_Center_Aircraft.Set_Font_Size (0.2);

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (Width, Height : Float) is
   begin

      View.Height := Height;

      View.Width  := Width;

      -- Map
      --------------------------------

      Maps.Terrain.Draw (View);

      Maps.Layers.Draw (View);

      Display.Route.Draw (View);

      -- Route adaptation

      Ent_Route.Draw;

      Btn_Route_Previous.Draw;

      Btn_Route_Next.Draw;

      Btn_Route_Append.Draw;

      Btn_Route_Remove.Draw;

      -- Waypoint adaptation

      Ent_Waypoint.Draw;

      Btn_Wypnt_Next.Draw;

      Btn_Wypnt_Previous.Draw;

      Btn_Wypnt_Append.Draw;

      Btn_Wypnt_Prepend.Draw;

      Btn_Wypnt_Remove.Draw;

      -- Waypoint move

      Btn_Wypnt_Right.Draw;

      Btn_Wypnt_Left.Draw;

      Btn_Wypnt_Up.Draw;

      Btn_Wypnt_Down.Draw;

      Btn_Wypnt_Step.Draw;

      -- Map view

      Btn_View_West.Draw;

      Btn_View_East.Draw;

      Btn_View_South.Draw;

      Btn_View_North.Draw;

      Btn_Zoom_In.Draw;

      Btn_Zoom_Out.Draw;

      Btn_Center_Home.Draw;

      Btn_Center_Wyptn.Draw;

      Btn_Center_Aircraft.Draw;

      -- Keyboard

      if Text_Focus /= null then

         Widgets.Keyboard.Set_Allocation ((0.20, 0.01, 0.56, 0.30));

         Widgets.Keyboard.Draw;

      end if;

   end Draw;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Refresh_Data is

      use Flight.Plan;
      use Utility.Strings;

   begin

      Ent_Route.Set_Label (Trim (Flight_Plan.Name));

      Ent_Waypoint.Set_Label (Trim (Flight.Plan.Next_Waypoint.Name));

      Change_Focus (null);

   end Refresh_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is

      use Flight.Plan;

   begin

      if
        Btn_Zoom_In.Contains (X, Y)
      then

         View.Zoom_In;

         Refresh := True;

      elsif
        Btn_Zoom_Out.Contains (X, Y)
      then

         View.Zoom_Out;

         Refresh := True;

      elsif
        Btn_View_East.Contains (X, Y)
      then

         View.Center.Lon := View.Center.Lon + 0.15 * Long_Float (View.Zoom * View.Width);

         Refresh := True;

      elsif
        Btn_View_West.Contains (X, Y)
      then

         View.Center.Lon := View.Center.Lon - 0.15 * Long_Float (View.Zoom * View.Width);

         Refresh := True;

      elsif
        Btn_View_North.Contains (X, Y)
      then

         View.Center.Lat := View.Center.Lat + 0.15 * Long_Float (View.Zoom * View.Height);

         Refresh := True;

      elsif
        Btn_View_South.Contains (X, Y)
      then

         View.Center.Lat := View.Center.Lat - 0.15 * Long_Float (View.Zoom * View.Height);

         Refresh := True;

      elsif
        Btn_Center_Aircraft.Contains (X, Y)
      then

         View.Center := Flight.Data.Position;

         Refresh := True;

      elsif
        Btn_Center_Home.Contains (X, Y)
      then

         View.Center := Flight.Plan.Home_Waypoint.Position;

         Refresh := True;

      elsif
        Btn_Center_Wyptn.Contains (X, Y)
      then

         View.Center := Flight.Plan.Next_Waypoint.Position;

         Refresh := True;

      elsif
        Btn_Wypnt_Left.Contains (X, Y)
      then

         declare
            Step_Vector : Vector2_Record;
         begin

            Step_Vector.Set (-Step, 0.0);

            Flight.Plan.Next_Waypoint.Position := Maps.Position (Flight.Plan.Next_Waypoint.Position, Step_Vector);

         end;

         Flight_Plan.Recompute_Tasks;

         Flight.Plan.Modified := True;

         Refresh := True;

      elsif
        Btn_Wypnt_Right.Contains (X, Y)
      then

         declare
            Step_Vector : Vector2_Record;
         begin

            Step_Vector.Set (Step, 0.0);

            Flight.Plan.Next_Waypoint.Position := Maps.Position (Flight.Plan.Next_Waypoint.Position, Step_Vector);

         end;

         Flight_Plan.Recompute_Tasks;

         Flight.Plan.Modified := True;

         Refresh := True;

      elsif
        Btn_Wypnt_Up.Contains (X, Y)
      then

         declare
            Step_Vector : Vector2_Record;
         begin

            Step_Vector.Set (0.0, Step);

            Flight.Plan.Next_Waypoint.Position := Maps.Position (Flight.Plan.Next_Waypoint.Position, Step_Vector);

         end;

         Flight_Plan.Recompute_Tasks;

         Flight.Plan.Modified := True;

         Refresh := True;

      elsif
        Btn_Wypnt_Down.Contains (X, Y)
      then

         declare
            Step_Vector : Vector2_Record;
         begin

            Step_Vector.Set (0.0, -Step);

            Flight.Plan.Next_Waypoint.Position := Maps.Position (Flight.Plan.Next_Waypoint.Position, Step_Vector);

         end;

         Flight_Plan.Recompute_Tasks;

         Flight.Plan.Modified := True;

         Refresh := True;

      elsif
        Btn_Wypnt_Step.Contains (X, Y)
      then

         -- Change the step, choose between 0.1km, 1km, 10km
         -------------------------------------------------------

         Step := 10.0 * Step;

         if Step > 10.0 then

            Step := 0.1;

         end if;

         if Step < 1.0 then

            Btn_Wypnt_Step.Set_Label (Utility.Strings.Float_Image (Float (Step), 1));

         else

            Btn_Wypnt_Step.Set_Label (Utility.Strings.Float_Image (Float (Step), 0));

         end if;

         Refresh := True;

      elsif
        Btn_Wypnt_Previous.Contains (X, Y)
      then

         Flight.Plan.Goto_Previous_Waypoint;

         Flight.Plan.Update_Flight_Plan;

         Refresh_Data;

         Refresh := True;

      elsif
        Btn_Wypnt_Next.Contains (X, Y)
      then

         Flight.Plan.Goto_Next_Waypoint;

         Flight.Plan.Update_Flight_Plan;

         Refresh_Data;

         Refresh := True;

      elsif
        Btn_Wypnt_Remove.Contains (X, Y)
      then

         if Flight.Plan.Remove_Active_Waypoint then

            Refresh_Data;

            Refresh := True;

         end if;

      elsif
        Btn_Wypnt_Append.Contains (X, Y)
      then

         if Flight.Plan.Append_Waypoint then

            Refresh_Data;

            Refresh := True;

         end if;

      elsif
        Btn_Wypnt_Prepend.Contains (X, Y)
      then

         if Flight.Plan.Prepend_Waypoint then

            Refresh_Data;

            Refresh := True;

         end if;

      elsif
        Btn_Route_Next.Contains (X, Y)
      then

         if Flight.Plan.Next_Flight_Plan then

            Refresh_Data;

            Refresh := True;

         end if;

      elsif
        Btn_Route_Previous.Contains (X, Y)
      then

         if Flight.Plan.Previous_Flight_Plan then

            Refresh_Data;

            Refresh := True;

         end if;

      elsif
        Btn_Route_Append.Contains (X, Y)
      then

         if Flight.Plan.Append_Flight_Plan then

            Refresh_Data;

            Refresh := True;

         end if;

      elsif
        Btn_Route_Remove.Contains (X, Y)
      then

         if Flight.Plan.Remove_Flight_Plan then

            Refresh_Data;

            Refresh := True;

         end if;

      elsif
        Ent_Route.Contains (X, Y)
      then

         Change_Focus (Ent_Route_Access);

         Widgets.Keyboard.Set_Text (Utility.Strings.Trim (Flight_Plan.Name));

         Refresh := True;

      elsif
        Ent_Waypoint.Contains (X, Y)
      then

         Change_Focus (Ent_Waypoint_Access);

         Widgets.Keyboard.Set_Text (Utility.Strings.Trim (Next_Waypoint.Name));

         Refresh := True;

      elsif
        Text_Focus /= null and then
        Widgets.Keyboard.Key_Pressed (X, Y)
      then

         Text_Focus.Set_Label (Widgets.Keyboard.Get_Text);

         if
           Text_Focus = Ent_Route_Access
         then

            Utility.Strings.Override (Flight_Plan.Name, Widgets.Keyboard.Get_Text);

         elsif
           Text_Focus = Ent_Waypoint_Access
         then

            Utility.Strings.Override (Next_Waypoint.Name, Widgets.Keyboard.Get_Text);

         end if;

         Refresh := True;

      else

         Change_Focus (null);

      end if;

   end Screen_Pressed;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
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

         when others =>

            null;

      end case;

   end Key_Changed;
   -----------------------------------------------------------------------------

end Display.Pages.Route_Edition;
--------------------------------------------------------------------------------
