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
package body Widgets.Panel is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (This : in out Panel_Record) is

      use Gl.Fonts;

      use type Gl.Gl_Uint;

      X, Y, H, W, S, D : Float;

      L : constant Float := Float (This.Length);

      A : Allocation_Record := This.Get_Allocation;

      Alignment : Font_Alignment_Types := Alignment_LL;

      function Get_Area_Buffer return Gl.Gl_Float_Vec is

         Area_Buffer : aliased Gl.Gl_Float_Vec (1..12);

      begin

         Area_Buffer (1)  := A.X + D   + 0.5 * W + S;
         Area_Buffer (2)  := A.Y + A.H - 1.0 * H;

         Area_Buffer (3)  := Area_Buffer (1);
         Area_Buffer (4)  := A.Y + A.H;

         Area_Buffer (5)  := A.X + A.W;
         Area_Buffer (6)  := A.Y + A.H;

         Area_Buffer (7)  := A.X + A.W;
         Area_Buffer (8)  := A.Y;

         Area_Buffer (9)  := A.X;
         Area_Buffer (10) := A.Y;

         Area_Buffer (11) := A.X;
         Area_Buffer (12) := Area_Buffer (2);

         if This.Label_Side = Label_Right then

            for I in 1..6 loop

               Area_Buffer (2 * I - 1) := 2.0 * A.X - Area_Buffer (2 * I - 1) + A.W;

            end loop;

         end if;

         return Area_Buffer;

      end;

   begin

      -- Compute font metrics
      --------------------------------------------------------------------------

      H := This.Font_Size;

      W := This.Width_Ratio * H;

      S := This.Space_Ratio * W;

      D := (W + S) * L - S;

      -- Draw the area and border
      --------------------------------------------------------------------------

      if This.Reload_Base then

         declare
            Area_Buffer : aliased Gl.Gl_Float_Vec := Get_Area_Buffer;
         begin

            Gl.Resources.Update_Resource (This.Area_Buffer_Id, Area_Buffer'Access);

         end;

         This.Reload_Base := False;

      end if;

      if This.Area_Buffer_Id /= 0 then

         Gl.Shaders.Bind_Shader (Gl.Shaders.Monochrome_2D);

         Gl.Bind_Buffer (GL_ARRAY_BUFFER, This.Area_Buffer_Id);

         Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

         Gl.Shaders.Load_Color (This.Get_Background_Color.R,
                                This.Get_Background_Color.G,
                                This.Get_Background_Color.B,
                                This.Get_Background_Color.A);

         Gl.Draw_Arrays (GL_TRIANGLE_FAN, 0, 6);

         if This.Show_Border then

            Gl.Shaders.Load_Color (This.Get_Border_Color.R,
                                   This.Get_Border_Color.G,
                                   This.Get_Border_Color.B,
                                   This.Get_Border_Color.A);

            Gl.Draw_Arrays (GL_LINE_LOOP, 0, 6);

         end if;

      end if;

      -- Draw the text
      --------------------------------------------------------------------------

      case This.Label_Side is

         when Label_Left =>

            X := A.X;

            Alignment := Alignment_LL;

         when Label_Right =>

            X := A.X + A.W;

            Alignment := Alignment_LR;

      end case;

      Y := A.Y + A.H - 0.5 * H;

      -- Glow
      -----------------------------------

      Gl.Shaders.Load_Diameter (3.0, 0.4);

      Gl.Shaders.Load_Width (3.0, 1.5);

      Gl.Shaders.Load_Color (This.Font_Glow.R,
                             This.Font_Glow.G,
                             This.Font_Glow.B,
                             This.Font_Glow.A);

      Gl.Fonts.Draw (This.Label (1..This.Length), X, Y, W, H, S, Alignment);

      -- Foreground
      -----------------------------------

      Gl.Shaders.Load_Diameter (1.0, 0.4);

      Gl.Shaders.Load_Width (1.1, 1.5);

      Gl.Shaders.Load_Color (This.Font_Fore.R,
                             This.Font_Fore.G,
                             This.Font_Fore.B,
                             This.Font_Fore.A);

      Gl.Fonts.Draw (This.Label (1..This.Length), X, Y, W, H, S, Alignment);

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Initialize (This : in out Panel_Record) is
   begin

      Widget_Record (This).Initialize;

      This.Label  := (others => ' ');

      This.Length     := 0;

      This.Font_Fore := Color_White;

      This.Font_Glow := Color_Black;

      This.Font_Size  := 0.05;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Finalize (This : in out Panel_Record) is
   begin

      null;

   end Finalize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Adjust (This : in out Panel_Record) is
   begin

      -- Force the creation of own resources

      Widget_Record (This).Adjust;

      This.Area_Buffer_Id := 0;

      This.Reload_Base := True;

   end Adjust;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Label (This : in out Panel_Record; Text : String; Side : Label_Sides := Label_Left) is

      New_Label  : Panel_String;

      Old_Length : Natural := This.Length;

   begin

      Utility.Strings.Override (New_Label, Text);

      if This.Label /= New_Label or This.Label_Side /= Side then

         This.Label  := New_Label;

         This.Length := Natural'Min (Text'Length, New_Label'Length);

         This.Label_Side := Side;

         This.Reload_Base := True;

      end if;

   end Set_Label;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Label_Color (This : in out Panel_Record;
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
   procedure Set_Font_Size (This        : in out Panel_Record;
                            Value       : Dimension_Float;
                            Width_Ratio : Ratio_Float := 0.6;
                            Space_Ratio : Ratio_Float := 0.7) is
   begin

      This.Font_Size := Value;

      This.Width_Ratio := Width_Ratio;

      This.Space_Ratio := Space_Ratio;

      This.Reload_Base := True;

   end Set_Font_Size;
   -----------------------------------------------------------------------------

end Widgets.Panel;
--------------------------------------------------------------------------------
