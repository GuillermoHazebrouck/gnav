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
with Gl;
with Gl.Fonts;
with Gl.Resources;
use  Gl.Resources;
with Gl.Shaders;
with Utility.Colors;
use  Utility.Colors;
with Utility.Strings;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Widgets.Button is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (This : in out Button_Record) is

      X, Y, H, W, S, D : Float;

      L : constant Float := Float (This.Length);

      A : Allocation_Record := This.Get_Allocation;

   begin

      -- Draw the area and border from the widget class
      --------------------------------------------------------------------------

      Widget_Record (This).Draw;

      -- Draw the text
      --------------------------------------------------------------------------

      H := This.Font_Size * A.H;

      W := This.Width_Ratio * H;

      S := This.Space_Ratio * W;

      D := (W + S) * L - S;

      X := A.X + 0.5 * (A.W - D);

      Y := A.Y + 0.5 * (A.H - H);

      -- Foreground
      -----------------------------------

      Gl.Shaders.Load_Diameter (3.0, 0.4);

      Gl.Shaders.Load_Width (3.0, 1.5);

      Gl.Shaders.Load_Color (This.Font_Glow.R,
                             This.Font_Glow.G,
                             This.Font_Glow.B,
                             This.Font_Glow.A);

      Gl.Fonts.Draw (This.Label, X, Y, W, H, S);

      -- Foreground
      -----------------------------------

      Gl.Shaders.Load_Diameter (1.0, 0.4);

      Gl.Shaders.Load_Width (1.1, 1.5);

      Gl.Shaders.Load_Color (This.Font_Fore.R,
                             This.Font_Fore.G,
                             This.Font_Fore.B,
                             This.Font_Fore.A);

      Gl.Fonts.Draw (This.Label, X, Y, W, H, S);

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Initialize (This : in out Button_Record) is
   begin

      Widget_Record (This).Initialize;

      This.Label  := (others => ' ');

      This.Length     := 0;

      This.Font_Fore := Color_White;

      This.Font_Glow := Color_Black;

      This.Font_Size  := 0.6;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Finalize (This : in out Button_Record) is
   begin

      null;

   end Finalize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Adjust (This : in out Button_Record) is
   begin

      Widget_Record (This).Adjust;

   end Adjust;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Label (This : in out Button_Record; Text : String) is

      New_Label : Button_String;

   begin

      Utility.Strings.Override (New_Label, Text);

      if This.Label /= New_Label then

         This.Label  := New_Label;

         This.Length := Text'Length;

      end if;

   end Set_Label;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Label_Color (This : in out Button_Record;
                              Fore : Color_Record;
                              Glow : Color_Record := Color_Black) is
   begin

      This.Font_Fore := Fore;

      This.Font_Glow := Glow;

   end Set_Label_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Font_Size (This        : in out Button_Record;
                            Value       : Ratio_Float;
                            Width_Ratio : Ratio_Float := 0.6;
                            Space_Ratio : Ratio_Float := 0.7) is
   begin

      This.Font_Size := Value;

      This.Width_Ratio := Width_Ratio;

      This.Space_Ratio := Space_Ratio;

   end Set_Font_Size;
   -----------------------------------------------------------------------------

end Widgets.Button;
--------------------------------------------------------------------------------
