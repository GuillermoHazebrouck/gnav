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

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Gl.Resources is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Update_Resource (Resource : in out Gl_Uint; Buffer : access Gl_Float_Vec; Length : Natural := 0) is

      use type Gl_Uint;
      use type Gl_Sizei;

      Buffers : aliased Gl_Uint_Vec := (1 => 0);
      Size    : Gl_Sizeiptr;

   begin
      if Resource = 0 then

         Gl.Gen_Buffers (1, Buffers'Access);

         Resource := Buffers (1);

      end if;

      if Resource > 0 then

         -- NOTE: OpenGL should reallocate the GPU memory automatically by doing
         --       this.
         -----------------------------------------------------------------------

         Size := Gl_Sizeiptr (Buffer'Length * Gl_Float_Size);

         if Length /= 0 and then Length < Buffer'Length then
            Size := Gl_Sizeiptr (Gl_Sizei (Length) * Gl_Float_Size);
         end if;

         Gl.Bind_Buffer (GL_ARRAY_BUFFER, Resource);

         Gl.Buffer_Data (GL_ARRAY_BUFFER, Size, Buffer, GL_STATIC_DRAW);

      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("while updating GPU resources");

   end Update_Resource;
   -----------------------------------------------------------------------------




end Gl.Resources;
--------------------------------------------------------------------------------
