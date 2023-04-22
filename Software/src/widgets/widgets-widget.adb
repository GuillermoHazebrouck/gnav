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
with Ada.Text_IO;
-- Gnav
with Gl;
with Gl.Resources;
with Gl.Shaders;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Widgets.Widget is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (This : in out Widget_Record) is

      use type Gl.Gl_Uint;

      function Get_Area_Buffer return Gl.Gl_Float_Vec is

         Area_Buffer : aliased Gl.Gl_Float_Vec (1..8);

      begin

         Area_Buffer (1) := This.Allocation.X;
         Area_Buffer (2) := This.Allocation.Y;

         Area_Buffer (3) := This.Allocation.X;
         Area_Buffer (4) := This.Allocation.Y + This.Allocation.H;

         Area_Buffer (5) := This.Allocation.X + This.Allocation.W;
         Area_Buffer (6) := This.Allocation.Y + This.Allocation.H;

         Area_Buffer (7) := This.Allocation.X + This.Allocation.W;
         Area_Buffer (8) := This.Allocation.Y;

         return Area_Buffer;

      end;

   begin

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

         Gl.Shaders.Load_Color (This.Background_Color.R,
                                This.Background_Color.G,
                                This.Background_Color.B,
                                This.Background_Color.A);

         Gl.Draw_Arrays (GL_TRIANGLE_FAN, 0, 4);

         if This.Show_Border then

            Gl.Shaders.Load_Color (This.Border_Color.R,
                                   This.Border_Color.G,
                                   This.Border_Color.B,
                                   This.Border_Color.A);

            Gl.Draw_Arrays (GL_LINE_LOOP, 0, 4);

         end if;

      end if;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Reload (This : in out Widget_Record) is
   begin

      This.Reload_Base := True;

   end Reload;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Allocation (This : in out Widget_Record; Value : Allocation_Record) is
   begin

      This.Reload_Base := This.Reload_Base or else This.Allocation /= Value;

      This.Allocation := Value;

   end Set_Allocation;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Gets the area covered by the widget
   --===========================================================================
   function Get_Allocation (This : in Widget_Record'Class) return Allocation_Record is
   begin

      return This.Allocation;

   end Get_Allocation;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Visible (This : in out Widget_Record'Class; Value : Boolean) is
   begin

      This.Visible := Value;

   end Set_Visible;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Visible (This : in out Widget_Record; Value : Boolean) return Boolean is
   begin

      return This.Visible;

   end Get_Visible;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Background_Color (This : in out Widget_Record'Class; Value : Color_Record) is
   begin

      if This.Background_Color /= Value then

         This.Background_Color := Value;

      end if;

   end Set_Background_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Transparency (This : in out Widget_Record'Class; Value : Float) is
   begin

      This.Background_Color.A := Value;

   end Set_Transparency;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Show_Border (This : in out Widget_Record'Class; Value : Boolean) is
   begin

      This.Show_Border := Value;

   end Set_Show_Border;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Border_Color (This : in out Widget_Record'Class; Value : Color_Record) is
   begin

      if This.Border_Color /= Value then

         This.Border_Color := Value;

      end if;

   end Set_Border_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Background_Color (This : in out Widget_Record'Class) return Color_Record is
   begin

      return This.Background_Color;

   end Get_Background_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Border_Color (This : in out Widget_Record'Class) return Color_Record is
   begin

      return This.Border_Color;

   end Get_Border_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Contains (This : in out Widget_Record'Class; X, Y : Float) return Boolean is
   begin

      return
        This.Visible                              and then
        X > This.Allocation.X                     and then
        Y > This.Allocation.Y                     and then
        X < This.Allocation.X + This.Allocation.W and then
        Y < This.Allocation.Y + This.Allocation.H;

   end Contains;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Initialize (This : in out Widget_Record) is
   begin

      This.Background_Color := Color_Gray_4;
      This.Background_Color.A := 0.7;
      This.Border_Color     := Color_Black;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Finalize (This : in out Widget_Record) is
   begin

      null;

   end Finalize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Adjust (This : in out Widget_Record) is
   begin

      -- Force the creation of own resources

      This.Area_Buffer_Id := 0;

      This.Reload_Base := True;

   end Adjust;
   -----------------------------------------------------------------------------

end Widgets.Widget;
--------------------------------------------------------------------------------
