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


--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Gl.Colors is

   subtype Color_Float is Float range 0.0..1.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The definition of a color
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Color_Record is tagged record

      R : Color_Float;

      G : Color_Float;

      B : Color_Float;

      A : Color_Float;

   end record;
   -----------------------------------------------------------------------------

   --===========================================================================
   -- Returns the same color with a different alpha
   --===========================================================================
   function With_Alpha (This : Color_Record; New_A : Color_Float) return Color_Record;

   --===========================================================================
   -- Makes the color darker
   --===========================================================================
   procedure Dim (This : in out Color_Record; Factor : Color_Float);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The definition of a color
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Line_Color_Record is tagged record

      Fore : Color_Record;

      Glow : Color_Record;

   end record;

   -- General

   Color_White      : constant Color_Record := (R => 1.0, G => 1.0, B => 1.0, A => 1.0);
   Color_Red        : constant Color_Record := (R => 1.0, G => 0.0, B => 0.0, A => 1.0);
   Color_Green      : constant Color_Record := (R => 0.0, G => 1.0, B => 0.0, A => 1.0);
   Color_Blue       : constant Color_Record := (R => 0.0, G => 0.0, B => 1.0, A => 1.0);
   Color_Cyan       : constant Color_Record := (R => 0.0, G => 1.0, B => 1.0, A => 1.0);
   Color_Yellow     : constant Color_Record := (R => 1.0, G => 1.0, B => 0.0, A => 1.0);
   Color_Magenta    : constant Color_Record := (R => 1.0, G => 0.0, B => 1.0, A => 1.0);
   Color_Black      : constant Color_Record := (R => 0.0, G => 0.0, B => 0.0, A => 1.0);

   Color_Ocean      : constant Color_Record := (R => 0.1, G => 0.1, B => 0.8, A => 1.0);
   Color_Sky        : constant Color_Record := (R => 0.1, G => 0.1, B => 0.8, A => 1.0);
   Color_Water      : constant Color_Record := (R => 0.0, G => 0.8, B => 0.8, A => 1.0);
   Color_Reddish    : constant Color_Record := (R => 0.9, G => 0.2, B => 0.2, A => 1.0);
   Color_Orange     : constant Color_Record := (R => 1.0, G => 0.6, B => 0.3, A => 1.0);
   Color_Amber      : constant Color_Record := (R => 0.7, G => 0.5, B => 0.0, A => 1.0);
   Color_Brown      : constant Color_Record := (R => 0.5, G => 0.3, B => 0.1, A => 1.0);
   Color_Earth      : constant Color_Record := (R => 0.3, G => 0.2, B => 0.1, A => 1.0);
   Color_Grass      : constant Color_Record := (R => 0.0, G => 0.2, B => 0.0, A => 1.0);
   Color_Tree       : constant Color_Record := (R => 0.7, G => 1.0, B => 0.7, A => 1.0);
   Color_Forest     : constant Color_Record := (R => 0.4, G => 0.7, B => 0.4, A => 1.0);
   Color_Greeny     : constant Color_Record := (R => 0.6, G => 1.0, B => 0.6, A => 1.0);
   Color_Violet     : constant Color_Record := (R => 0.2, G => 0.1, B => 0.2, A => 1.0);
   Color_Pink       : constant Color_Record := (R => 1.0, G => 0.5, B => 0.8, A => 1.0);
   Color_Rails      : constant Color_Record := (R => 0.7, G => 0.6, B => 0.6, A => 1.0);
   Color_Purple     : constant Color_Record := (R => 0.7, G => 0.0, B => 1.0, A => 1.0);

   Color_Gray_1     : constant Color_Record := (R => 0.1, G => 0.1, B => 0.1, A => 1.0);
   Color_Gray_2     : constant Color_Record := (R => 0.2, G => 0.2, B => 0.2, A => 1.0);
   Color_Gray_3     : constant Color_Record := (R => 0.3, G => 0.3, B => 0.3, A => 1.0);
   Color_Gray_4     : constant Color_Record := (R => 0.4, G => 0.4, B => 0.4, A => 1.0);
   Color_Gray_5     : constant Color_Record := (R => 0.5, G => 0.5, B => 0.5, A => 1.0);
   Color_Gray_6     : constant Color_Record := (R => 0.6, G => 0.6, B => 0.6, A => 1.0);
   Color_Gray_7     : constant Color_Record := (R => 0.7, G => 0.7, B => 0.7, A => 1.0);
   Color_Gray_8     : constant Color_Record := (R => 0.8, G => 0.8, B => 0.8, A => 1.0);
   Color_Gray_9     : constant Color_Record := (R => 0.9, G => 0.9, B => 0.9, A => 1.0);

   -- Lines

   Line_White    : constant Line_Color_Record := (Color_White,   Color_Black);
   Line_Magenta  : constant Line_Color_Record := (Color_Magenta, Color_Violet);
   Line_Cyan     : constant Line_Color_Record := (Color_Cyan,    Color_Sky);
   Line_Grass    : constant Line_Color_Record := (Color_Green,   Color_Grass);
   Line_Red      : constant Line_Color_Record := (Color_Red,     Color_Black);
   Line_Brown    : constant Line_Color_Record := (Color_Brown,   Color_Black);
   Line_Gray     : constant Line_Color_Record := (Color_Gray_6,  Color_Black);
   Line_Purple   : constant Line_Color_Record := (Color_Purple,  Color_Black);
   Line_Home     : constant Line_Color_Record := (Color_Purple,  Color_Black);
   Line_Green    : constant Line_Color_Record := (Color_Green,   Color_Black);
   Line_Yellow   : constant Line_Color_Record := (Color_Yellow,  Color_Black);

end Gl.Colors;
--------------------------------------------------------------------------------
