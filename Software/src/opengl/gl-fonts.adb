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
with Gl.Resources;
with Gl.Shaders;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Gl.Fonts is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Font_Data_Record is record

      -- The identifier in the GPU
      -- (it could be a list or a buffer, depending on the GL mode)
      --------------------------------------------------------------------------
      Id : Gl_Uint;

      -- The number of nodes in the buffer
      --------------------------------------------------------------------------
      Size : Gl_Sizei;

   end record;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A list of all possible font data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Character_Buffer is array (Character) of Font_Data_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The list of all font data items
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Font_Data : Character_Buffer := (others => (0, 0));

   --===========================================================================
   -- Returns the lines buffer for an specific character
   --===========================================================================
   function Get_Lines_Buffer (C : Character) return Gl_Float_Vec is
   begin

      case C is

         when 'A' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.5;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := 0.5;
               Buffer (6)  := 1.0;

               Buffer (7)  := 1.0;
               Buffer (8)  := 0.0;
               --

               -- Line
               Buffer (9)  := 0.15;
               Buffer (10) := 0.3;

               Buffer (11) := 0.85;
               Buffer (12) := 0.3;

               return Buffer;

            end;

         when 'B' =>

            declare
               Buffer : Gl_Float_Vec (1..40);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.8;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 1.0;
               Buffer (12) := 0.8;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.7;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 0.8;
               Buffer (20) := 0.5;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 1.0;
               Buffer (24) := 0.3;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 1.0;
               Buffer (28) := 0.1;
               --

               -- Line
               Buffer (29) := Buffer (27);
               Buffer (30) := Buffer (28);

               Buffer (31) := 0.8;
               Buffer (32) := 0.0;
               --

               -- Line
               Buffer (33) := Buffer (31);
               Buffer (34) := Buffer (32);

               Buffer (35) := 0.0;
               Buffer (36) := 0.0;
               --

               -- Line
               Buffer (37) := Buffer (19);
               Buffer (38) := Buffer (20);

               Buffer (39) := 0.0;
               Buffer (40) := 0.5;

               return Buffer;

            end;

         when 'C' =>

            declare
               Buffer : Gl_Float_Vec (1..28);
            begin

               -- Line
               Buffer (1)  := 1.0;
               Buffer (2)  := 0.2;

               Buffer (3)  := 0.8;
               Buffer (4)  := 0.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.2;
               Buffer (8)  := 0.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.0;
               Buffer (12) := 0.2;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 0.0;
               Buffer (16) := 0.8;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 0.2;
               Buffer (20) := 1.0;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.8;
               Buffer (24) := 1.0;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 1.0;
               Buffer (28) := 0.8;

               return Buffer;

            end;

         when 'D' =>

            declare
               Buffer : Gl_Float_Vec (1..24);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.7;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 1.0;
               Buffer (12) := 0.7;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.3;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 0.7;
               Buffer (20) := 0.0;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.0;
               Buffer (24) := 0.0;
               --

               return Buffer;

            end;

         when 'E' =>

            declare
               Buffer : Gl_Float_Vec (1..16);
            begin

               -- Line
               Buffer (1)  := 1.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 0.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.0;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 1.0;
               Buffer (12) := 1.0;
               --

               -- Line
               Buffer (13) := 0.0;
               Buffer (14) := 0.5;

               Buffer (15) := 0.6;
               Buffer (16) := 0.5;
               --

               return Buffer;

            end;

         when 'F' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 1.0;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := 0.0;
               Buffer (10) := 0.5;

               Buffer (11) := 0.6;
               Buffer (12) := 0.5;
               --

               return Buffer;

            end;

         when 'G' =>

            declare
               Buffer : Gl_Float_Vec (1..36);
            begin

               -- Line
               Buffer (1)  := 0.5;
               Buffer (2)  := 0.5;

               Buffer (3)  := 1.0;
               Buffer (4)  := 0.5;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 1.0;
               Buffer (8)  := 0.2;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.8;
               Buffer (12) := 0.0;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 0.2;
               Buffer (16) := 0.0;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 0.0;
               Buffer (20) := 0.2;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.0;
               Buffer (24) := 0.8;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 0.2;
               Buffer (28) := 1.0;
               --

               -- Line
               Buffer (29) := Buffer (27);
               Buffer (30) := Buffer (28);

               Buffer (31) := 0.8;
               Buffer (32) := 1.0;
               --

               -- Line
               Buffer (33) := Buffer (31);
               Buffer (34) := Buffer (32);

               Buffer (35) := 1.0;
               Buffer (36) := 0.8;
               --

               return Buffer;

            end;

         when 'H' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := 1.0;
               Buffer (6)  := 0.0;

               Buffer (7)  := 1.0;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := 0.0;
               Buffer (10) := 0.5;

               Buffer (11) := 1.0;
               Buffer (12) := 0.5;
               --

               return Buffer;

            end;

         when 'I' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)  := 0.2;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.8;
               Buffer (4)  := 0.0;
               --

               -- Line
               Buffer (5)  := 0.2;
               Buffer (6)  := 1.0;

               Buffer (7)  := 0.8;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := 0.5;
               Buffer (10) := 0.0;

               Buffer (11) := 0.5;
               Buffer (12) := 1.0;
               --

               return Buffer;

            end;

         when 'J' =>

            declare
               Buffer : Gl_Float_Vec (1..20);
            begin

               -- Line
               Buffer (1)  := 0.5;
               Buffer (2)  := 1.0;

               Buffer (3)  := 1.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 1.0;
               Buffer (8)  := 0.2;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.8;
               Buffer (12) := 0.0;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 0.2;
               Buffer (16) := 0.0;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 0.0;
               Buffer (20) := 0.2;
               --

               return Buffer;

            end;

         when 'K' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := 0.0;
               Buffer (6)  := 0.5;

               Buffer (7)  := 1.0;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := 0.0;
               Buffer (10) := 0.5;

               Buffer (11) := 1.0;
               Buffer (12) := 0.0;
               --

               return Buffer;

            end;

         when 'L' =>

            declare
               Buffer : Gl_Float_Vec (1..8);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := 0.0;
               Buffer (6)  := 0.0;

               Buffer (7)  := 0.8;
               Buffer (8)  := 0.0;
               --

               return Buffer;

            end;

         when 'M' =>

            declare
               Buffer : Gl_Float_Vec (1..16);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.5;
               Buffer (8)  := 0.5;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 1.0;
               Buffer (12) := 1.0;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.0;
               --

               return Buffer;

            end;

         when 'N' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 1.0;
               Buffer (8)  := 0.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 1.0;
               Buffer (12) := 1.0;
               --

               return Buffer;

            end;

         when 'O' =>

            declare
               Buffer : Gl_Float_Vec (1..32);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.2;

               Buffer (3)  := 0.0;
               Buffer (4)  := 0.8;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.2;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.8;
               Buffer (12) := 1.0;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.8;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 1.0;
               Buffer (20) := 0.2;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.8;
               Buffer (24) := 0.0;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 0.2;
               Buffer (28) := 0.0;
               --

               -- Line
               Buffer (29) := Buffer (27);
               Buffer (30) := Buffer (28);

               Buffer (31) := 0.0;
               Buffer (32) := 0.2;
               --

               return Buffer;

            end;

         when 'P' =>

            declare
               Buffer : Gl_Float_Vec (1..24);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.8;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 1.0;
               Buffer (12) := 0.8;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.7;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 0.8;
               Buffer (20) := 0.5;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.0;
               Buffer (24) := 0.5;
               --

               return Buffer;

            end;

         when 'Q' =>

            declare
               Buffer : Gl_Float_Vec (1..36);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.2;

               Buffer (3)  := 0.0;
               Buffer (4)  := 0.8;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.2;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.8;
               Buffer (12) := 1.0;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.8;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 1.0;
               Buffer (20) := 0.2;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.8;
               Buffer (24) := 0.0;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 0.2;
               Buffer (28) := 0.0;
               --

               -- Line
               Buffer (29) := Buffer (27);
               Buffer (30) := Buffer (28);

               Buffer (31) := 0.0;
               Buffer (32) := 0.2;
               --

               -- Line
               Buffer (33) := 0.6;
               Buffer (34) := 0.4;

               Buffer (35) := 1.0;
               Buffer (36) := 0.0;
               --

               return Buffer;

            end;

         when 'R' =>

            declare
               Buffer : Gl_Float_Vec (1..28);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.8;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 1.0;
               Buffer (12) := 0.8;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.7;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 0.8;
               Buffer (20) := 0.5;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.0;
               Buffer (24) := 0.5;
               --

               -- Line
               Buffer (25) := 0.3;
               Buffer (26) := 0.5;

               Buffer (27) := 1.0;
               Buffer (28) := 0.0;
               --

               return Buffer;

            end;

         when 'S' =>

            declare
               Buffer : Gl_Float_Vec (1..36);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.1;

               Buffer (3)  := 0.2;
               Buffer (4)  := 0.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.8;
               Buffer (8)  := 0.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 1.0;
               Buffer (12) := 0.1;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.3;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 0.0;
               Buffer (20) := 0.7;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.0;
               Buffer (24) := 0.9;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 0.2;
               Buffer (28) := 1.0;
               --

               -- Line
               Buffer (29) := Buffer (27);
               Buffer (30) := Buffer (28);

               Buffer (31) := 0.8;
               Buffer (32) := 1.0;
               --

               -- Line
               Buffer (33) := Buffer (31);
               Buffer (34) := Buffer (32);

               Buffer (35) := 1.0;
               Buffer (36) := 0.9;
               --

               return Buffer;

            end;

         when 'T' =>

            declare
               Buffer : Gl_Float_Vec (1..8);
            begin

               -- Line
               Buffer (1)  := 0.5;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.5;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := 0.0;
               Buffer (6)  := 1.0;

               Buffer (7)  := 1.0;
               Buffer (8)  := 1.0;
               --

               return Buffer;

            end;

         when 'U' =>

            declare
               Buffer : Gl_Float_Vec (1..20);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 1.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 0.2;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.2;
               Buffer (8)  := 0.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.8;
               Buffer (12) := 0.0;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.2;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 1.0;
               Buffer (20) := 1.0;
               --

               return Buffer;

            end;

         when 'V' =>

            declare
               Buffer : Gl_Float_Vec (1..8);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 1.0;

               Buffer (3)  := 0.5;
               Buffer (4)  := 0.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 1.0;
               Buffer (8)  := 1.0;
               --

               return Buffer;

            end;

         when 'W' =>

            declare
               Buffer : Gl_Float_Vec (1..16);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 1.0;

               Buffer (3)  := 0.1;
               Buffer (4)  := 0.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.5;
               Buffer (8)  := 0.6;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.9;
               Buffer (12) := 0.0;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 1.0;
               --

               return Buffer;

            end;

         when 'X' =>

            declare
               Buffer : Gl_Float_Vec (1..8);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 1.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := 1.0;
               Buffer (6)  := 0.0;

               Buffer (7)  := 0.0;
               Buffer (8)  := 1.0;
               --

               return Buffer;

            end;

         when 'Y' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 1.0;

               Buffer (3)  := 0.5;
               Buffer (4)  := 0.5;
               --

               -- Line
               Buffer (5)  := 1.0;
               Buffer (6)  := 1.0;

               Buffer (7)  := 0.5;
               Buffer (8)  := 0.5;
               --

               -- Line
               Buffer (9)  := 0.5;
               Buffer (10) := 0.5;

               Buffer (11) := 0.5;
               Buffer (12) := 0.0;
               --

               return Buffer;

            end;

         when 'Z' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 1.0;

               Buffer (3)  := 1.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := 1.0;
               Buffer (6)  := 1.0;

               Buffer (7)  := 0.0;
               Buffer (8)  := 0.0;
               --

               -- Line
               Buffer (9)  := 0.0;
               Buffer (10) := 0.0;

               Buffer (11) := 1.0;
               Buffer (12) := 0.0;
               --

               return Buffer;

            end;

         when '0' =>

            declare
               Buffer : Gl_Float_Vec (1..36);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.2;

               Buffer (3)  := 0.0;
               Buffer (4)  := 0.8;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.2;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.8;
               Buffer (12) := 1.0;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.8;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 1.0;
               Buffer (20) := 0.2;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.8;
               Buffer (24) := 0.0;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 0.2;
               Buffer (28) := 0.0;
               --

               -- Line
               Buffer (29) := Buffer (27);
               Buffer (30) := Buffer (28);

               Buffer (31) := 0.0;
               Buffer (32) := 0.2;
               --

               -- Line
               Buffer (33) := 0.3;
               Buffer (34) := 0.3;

               Buffer (35) := 0.7;
               Buffer (36) := 0.7;
               --

               return Buffer;

            end;

         when '1' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)  := 0.2;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.8;
               Buffer (4)  := 0.0;
               --

               -- Line
               Buffer (5)  := 0.5;
               Buffer (6)  := 0.0;

               Buffer (7)  := 0.5;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.3;
               Buffer (12) := 0.7;
               --

               return Buffer;

            end;

         when '2' =>

            declare
               Buffer : Gl_Float_Vec (1..32);
            begin

               -- Line
               Buffer (1)  := 1.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 0.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.0;
               Buffer (8)  := 0.2;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.8;
               Buffer (12) := 0.6;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.7;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 1.0;
               Buffer (20) := 0.9;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.8;
               Buffer (24) := 1.0;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 0.2;
               Buffer (28) := 1.0;
               --

               -- Line
               Buffer (29) := Buffer (27);
               Buffer (30) := Buffer (28);

               Buffer (31) := 0.0;
               Buffer (32) := 0.9;
               --

               return Buffer;

            end;

         when '3' =>

            declare
               Buffer : Gl_Float_Vec (1..44);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.9;

               Buffer (3)  := 0.2;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.8;
               Buffer (8)  := 1.0;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 1.0;
               Buffer (12) := 0.9;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.6;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 0.8;
               Buffer (20) := 0.5;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 1.0;
               Buffer (24) := 0.4;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 1.0;
               Buffer (28) := 0.1;
               --

               -- Line
               Buffer (29) := Buffer (27);
               Buffer (30) := Buffer (28);

               Buffer (31) := 0.8;
               Buffer (32) := 0.0;
               --

               -- Line
               Buffer (33) := Buffer (31);
               Buffer (34) := Buffer (32);

               Buffer (35) := 0.2;
               Buffer (36) := 0.0;
               --

               -- Line
               Buffer (37) := Buffer (35);
               Buffer (38) := Buffer (36);

               Buffer (39) := 0.0;
               Buffer (40) := 0.1;
               --

               -- Line
               Buffer (41) := Buffer (19);
               Buffer (42) := Buffer (20);

               Buffer (43) := 0.5;
               Buffer (44) := 0.5;
               --

               return Buffer;

            end;

         when '4' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)  := 0.8;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.8;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.0;
               Buffer (8)  := 0.4;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 1.0;
               Buffer (12) := 0.4;
               --

               return Buffer;

            end;

         when '5' =>

            declare
               Buffer : Gl_Float_Vec (1..28);
            begin

               -- Line
               Buffer (1)  := 1.0;
               Buffer (2)  := 1.0;

               Buffer (3)  := 0.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.0;
               Buffer (8)  := 0.6;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.8;
               Buffer (12) := 0.6;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 1.0;
               Buffer (16) := 0.4;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 1.0;
               Buffer (20) := 0.2;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.8;
               Buffer (24) := 0.0;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 0.0;
               Buffer (28) := 0.0;
               --

               return Buffer;

            end;

         when '6' =>

            declare
               Buffer : Gl_Float_Vec (1..40);
            begin

               -- Line
               Buffer (1)  := 0.9;
               Buffer (2)  := 1.0;

               Buffer (3)  := 0.4;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.0;
               Buffer (8)  := 0.6;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.0;
               Buffer (12) := 0.2;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 0.2;
               Buffer (16) := 0.0;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 0.8;
               Buffer (20) := 0.0;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 1.0;
               Buffer (24) := 0.2;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 1.0;
               Buffer (28) := 0.5;
               --

               -- Line
               Buffer (29) := Buffer (27);
               Buffer (30) := Buffer (28);

               Buffer (31) := 0.8;
               Buffer (32) := 0.6;
               --

               -- Line
               Buffer (33) := Buffer (31);
               Buffer (34) := Buffer (32);

               Buffer (35) := 0.2;
               Buffer (36) := 0.6;
               --

               -- Line
               Buffer (37) := Buffer (35);
               Buffer (38) := Buffer (36);

               Buffer (39) := 0.0;
               Buffer (40) := 0.5;
               --

               return Buffer;

            end;

         when '7' =>

            declare
               Buffer : Gl_Float_Vec (1..8);
            begin

               -- Line
               Buffer (1)  := 0.5;
               Buffer (2)  := 0.0;

               Buffer (3)  := 1.0;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.0;
               Buffer (8)  := 1.0;
               --

               return Buffer;

            end;

         when '8' =>

            declare
               Buffer : Gl_Float_Vec (1..60);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.1;

               Buffer (3)  := 0.0;
               Buffer (4)  := 0.3;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.2;
               Buffer (8)  := 0.5;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.0;
               Buffer (12) := 0.7;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 0.0;
               Buffer (16) := 0.9;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 0.2;
               Buffer (20) := 1.0;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.8;
               Buffer (24) := 1.0;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 1.0;
               Buffer (28) := 0.9;
               --

               -- Line
               Buffer (29) := Buffer (27);
               Buffer (30) := Buffer (28);

               Buffer (31) := 1.0;
               Buffer (32) := 0.7;
               --

               -- Line
               Buffer (33) := Buffer (31);
               Buffer (34) := Buffer (32);

               Buffer (35) := 0.8;
               Buffer (36) := 0.5;
               --

               -- Line
               Buffer (37) := Buffer (35);
               Buffer (38) := Buffer (36);

               Buffer (39) := 1.0;
               Buffer (40) := 0.3;
               --

               -- Line
               Buffer (41) := Buffer (39);
               Buffer (42) := Buffer (40);

               Buffer (43) := 1.0;
               Buffer (44) := 0.1;
               --

               -- Line
               Buffer (45) := Buffer (43);
               Buffer (46) := Buffer (44);

               Buffer (47) := 0.8;
               Buffer (48) := 0.0;
               --

               -- Line
               Buffer (49) := Buffer (47);
               Buffer (50) := Buffer (48);

               Buffer (51) := 0.2;
               Buffer (52) := 0.0;
               --

               -- Line
               Buffer (53) := Buffer (51);
               Buffer (54) := Buffer (52);

               Buffer (55) := 0.0;
               Buffer (56) := 0.1;
               --

               -- Line
               Buffer (57) := Buffer (7);
               Buffer (58) := Buffer (8);

               Buffer (59) := Buffer (35);
               Buffer (60) := Buffer (36);
               --

               return Buffer;

            end;

         when '9' =>

            declare
               Buffer : Gl_Float_Vec (1..40);
            begin

               -- Line
               Buffer (1)  := 0.1;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.8;
               Buffer (4)  := 0.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 1.0;
               Buffer (8)  := 0.4;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 1.0;
               Buffer (12) := 0.8;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 0.8;
               Buffer (16) := 1.0;
               --

               -- Line
               Buffer (17) := Buffer (15);
               Buffer (18) := Buffer (16);

               Buffer (19) := 0.2;
               Buffer (20) := 1.0;
               --

               -- Line
               Buffer (21) := Buffer (19);
               Buffer (22) := Buffer (20);

               Buffer (23) := 0.0;
               Buffer (24) := 0.8;
               --

               -- Line
               Buffer (25) := Buffer (23);
               Buffer (26) := Buffer (24);

               Buffer (27) := 0.0;
               Buffer (28) := 0.5;
               --

               -- Line
               Buffer (29) := Buffer (27);
               Buffer (30) := Buffer (28);

               Buffer (31) := 0.2;
               Buffer (32) := 0.4;
               --

               -- Line
               Buffer (33) := Buffer (31);
               Buffer (34) := Buffer (32);

               Buffer (35) := 0.8;
               Buffer (36) := 0.4;
               --

               -- Line
               Buffer (37) := Buffer (35);
               Buffer (38) := Buffer (36);

               Buffer (39) := 1.0;
               Buffer (40) := 0.5;
               --

               return Buffer;

            end;

         when '=' =>

            declare
               Buffer : Gl_Float_Vec (1..8);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.3;

               Buffer (3)  := 1.0;
               Buffer (4)  := 0.3;
               --

               -- Line
               Buffer (5)  := 0.0;
               Buffer (6)  := 0.7;

               Buffer (7)  := 1.0;
               Buffer (8)  := 0.7;
               --

               return Buffer;

            end;

         when ':' =>

            declare
               Buffer : Gl_Float_Vec (1..8);
            begin

               -- Line
               Buffer (1)  := 0.4;
               Buffer (2)  := 0.2;

               Buffer (3)  := 0.5;
               Buffer (4)  := 0.2;
               --

               -- Line
               Buffer (5)  := 0.4;
               Buffer (6)  := 0.7;

               Buffer (7)  := 0.5;
               Buffer (8)  := 0.7;
               --

               return Buffer;

            end;

         when '+' =>

            declare
               Buffer : Gl_Float_Vec (1..8);
            begin

               -- Line
               Buffer (1)  := 0.5;
               Buffer (2)  := 0.1;

               Buffer (3)  := 0.5;
               Buffer (4)  := 0.9;
               --

               -- Line
               Buffer (5)  := 0.1;
               Buffer (6)  := 0.5;

               Buffer (7)  := 0.9;
               Buffer (8)  := 0.5;
               --

               return Buffer;

            end;

         when '-' =>

            declare
               Buffer : Gl_Float_Vec (1..4);
            begin

               -- Line
               Buffer (1)  := 0.1;
               Buffer (2)  := 0.5;

               Buffer (3)  := 0.9;
               Buffer (4)  := 0.5;
               --

               return Buffer;

            end;

         when '_' =>

            declare
               Buffer : Gl_Float_Vec (1..4);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 1.0;
               Buffer (4)  := 0.0;
               --

               return Buffer;

            end;

         when '.' =>

            declare
               Buffer : Gl_Float_Vec (1..4);
            begin

               -- Line
               Buffer (1)  := 0.5;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.5;
               Buffer (4)  := 0.1;
               --

               return Buffer;

            end;

         when ',' =>

            declare
               Buffer : Gl_Float_Vec (1..4);
            begin

               -- Line
               Buffer (1)  := 0.6;
               Buffer (2)  := 0.1;

               Buffer (3)  := 0.4;
               Buffer (4)  :=-0.1;
               --

               return Buffer;

            end;

         when '*' | '°' =>

            declare
               Buffer : Gl_Float_Vec (1..16);
            begin

               -- Line
               Buffer (1)  := 0.3;
               Buffer (2)  := 1.0;

               Buffer (3)  := 0.7;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := Buffer (3);
               Buffer (6)  := Buffer (4);

               Buffer (7)  := 0.7;
               Buffer (8)  := 0.6;
               --

               -- Line
               Buffer (9)  := Buffer (7);
               Buffer (10) := Buffer (8);

               Buffer (11) := 0.3;
               Buffer (12) := 0.6;
               --

               -- Line
               Buffer (13) := Buffer (11);
               Buffer (14) := Buffer (12);

               Buffer (15) := 0.3;
               Buffer (16) := 1.0;
               --

               return Buffer;

            end;

         when ''' =>

            declare
               Buffer : Gl_Float_Vec (1..4);
            begin

               -- Line
               Buffer (1)  := 0.5;
               Buffer (2)  := 1.0;

               Buffer (3)  := 0.5;
               Buffer (4)  := 0.7;
               --

               return Buffer;

            end;

         when '"' =>

            declare
               Buffer : Gl_Float_Vec (1..8);
            begin

               -- Line
               Buffer (1)  := 0.3;
               Buffer (2)  := 1.0;

               Buffer (3)  := 0.3;
               Buffer (4)  := 0.7;
               --

               -- Line
               Buffer (5)  := 0.7;
               Buffer (6)  := 1.0;

               Buffer (7)  := 0.7;
               Buffer (8)  := 0.7;
               --

               return Buffer;

            end;

         when '/' =>

            declare
               Buffer : Gl_Float_Vec (1..4);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 0.0;

               Buffer (3)  := 1.0;
               Buffer (4)  := 1.0;
               --

               return Buffer;

            end;

         when '\' =>

            declare
               Buffer : Gl_Float_Vec (1..4);
            begin

               -- Line
               Buffer (1)  := 0.0;
               Buffer (2)  := 1.0;

               Buffer (3)  := 1.0;
               Buffer (4)  := 0.0;
               --

               return Buffer;

            end;

         when '|' =>

            declare
               Buffer : Gl_Float_Vec (1..4);
            begin

               -- Line
               Buffer (1)  := 0.5;
               Buffer (2)  := 0.0;

               Buffer (3)  := 0.5;
               Buffer (4)  := 1.0;
               --

               return Buffer;

            end;

         when '!' =>

            declare
               Buffer : Gl_Float_Vec (1..8);
            begin

               -- Line
               Buffer (1)  := 0.5;
               Buffer (2)  := 0.2;

               Buffer (3)  := 0.5;
               Buffer (4)  := 1.0;
               --

               -- Line
               Buffer (5)  := 0.5;
               Buffer (6)  := 0.0;

               Buffer (7)  := 0.5;
               Buffer (8)  := 0.1;
               --

               return Buffer;

            end;

         when '~' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)  := 0.00;
               Buffer (2)  := 0.40;

               Buffer (3)  := 0.25;
               Buffer (4)  := 0.60;
               --

               -- Line
               Buffer (5)  := 0.25;
               Buffer (6)  := 0.60;

               Buffer (7)  := 0.75;
               Buffer (8)  := 0.40;
               --

               -- Line
               Buffer (9)  := 0.75;
               Buffer (10) := 0.40;

               Buffer (11) := 1.00;
               Buffer (12) := 0.60;
               --

               return Buffer;

            end;

         when '>' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)   := 0.0;
               Buffer (2)   := 0.5;

               Buffer (3)   := 1.0;
               Buffer (4)   := 0.5;
               --

               -- Line
               Buffer (5)   := 0.4;
               Buffer (6)   := 0.8;

               Buffer (7)   := 1.0;
               Buffer (8)   := 0.5;
               --

               -- Line
               Buffer (9)   := 0.4;
               Buffer (10)  := 0.2;

               Buffer (11)  := 1.0;
               Buffer (12)  := 0.5;
               --

               return Buffer;

            end;

         when '<' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)   := 0.0;
               Buffer (2)   := 0.5;

               Buffer (3)   := 1.0;
               Buffer (4)   := 0.5;
               --

               -- Line
               Buffer (5)   := 0.6;
               Buffer (6)   := 0.8;

               Buffer (7)   := 0.0;
               Buffer (8)   := 0.5;
               --

               -- Line
               Buffer (9)   := 0.6;
               Buffer (10)  := 0.2;

               Buffer (11)  := 0.0;
               Buffer (12)  := 0.5;
               --

               return Buffer;

            end;

         when '}' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)   := 0.5;
               Buffer (2)   := 0.1;

               Buffer (3)   := 0.5;
               Buffer (4)   := 0.9;
               --

               -- Line
               Buffer (5)   := 0.2;
               Buffer (6)   := 0.6;

               Buffer (7)   := 0.5;
               Buffer (8)   := 0.9;
               --

               -- Line
               Buffer (9)   := 0.8;
               Buffer (10)  := 0.6;

               Buffer (11)  := 0.5;
               Buffer (12)  := 0.9;
               --

               return Buffer;

            end;

         when '{' =>

            declare
               Buffer : Gl_Float_Vec (1..12);
            begin

               -- Line
               Buffer (1)   := 0.5;
               Buffer (2)   := 0.1;

               Buffer (3)   := 0.5;
               Buffer (4)   := 0.9;
               --

               -- Line
               Buffer (5)   := 0.2;
               Buffer (6)   := 0.4;

               Buffer (7)   := 0.5;
               Buffer (8)   := 0.1;
               --

               -- Line
               Buffer (9)   := 0.8;
               Buffer (10)  := 0.4;

               Buffer (11)  := 0.5;
               Buffer (12)  := 0.1;
               --

               return Buffer;

            end;

         when others =>

            declare
               Buffer : Gl_Float_Vec (1..4);
            begin

               Buffer (1) := 0.0;
               Buffer (2) := 0.5;

               Buffer (3) := 1.0;
               Buffer (4) := 0.5;

               return Buffer;

            end;

      end case;

   end Get_Lines_Buffer;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Init is

      subtype Upper_Case_Range is Character range 'A'..'Z';

      subtype Lower_Case_Range is Character range 'a'..'z';

      subtype Number_Range is Character range '0'..'9';

      --========================================================================
      --
      --========================================================================
      procedure Load_Character (C : Character) is

         use type Gl_Uint;

         Buffer : aliased Gl_Float_Vec := Get_Lines_Buffer (C);
         I, J   : Positive := 1;

      begin

         if Font_Data (C).Id = 0 then

            Gl.Resources.Update_Resource (Font_Data (C).Id, Buffer'Access);

            Font_Data (C).Size := Gl_Sizei (Buffer'Length / 2);

         end if;

      end Load_Character;
      --------------------------------------------------------------------------

   begin

      -- Upper case
      ------------------------------
      for C in Upper_Case_Range loop

         Load_Character (C);

      end loop;

      -- Lower case
      ------------------------------
      for C in Lower_Case_Range loop

         Load_Character (C);

      end loop;

      -- Numbers
      ------------------------------
      for C in Number_Range loop

         Load_Character (C);

      end loop;

      -- Other symbols
      ------------------------------

      Load_Character ('=');
      Load_Character ('+');
      Load_Character ('-');
      Load_Character ('_');
      Load_Character ('.');
      Load_Character (',');
      Load_Character (':');
      Load_Character ('*');
      Load_Character ('°');
      Load_Character (''');
      Load_Character ('"');
      Load_Character ('/');
      Load_Character ('\');
      Load_Character ('|');
      Load_Character ('!');
      Load_Character ('~');
      Load_Character ('>'); -- arrow right
      Load_Character ('<'); -- arrow left
      Load_Character ('}'); -- arrow up
      Load_Character ('{'); -- arrow down

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (Text      : String;
                   X, Y      : Float;
                   Width     : Font_Float;
                   Height    : Font_Float;
                   Space     : Font_Float;
                   Alignment : Font_Alignment_Types := Alignment_LL) is

      use Gl;
      use type Gl_Uint;

      M1, M2 : Gl_Mat_4 := Gl.Shaders.Get_Active_Matrix;
      Data   : Font_Data_Record;
      H      : Float := X;
      V      : Float := Y;
      W      : Float := 0.0;

   begin

      if Text'Length = 0 or Width <= 0.0 or Height <= 0.0 then

         return;

      end if;

      case Alignment is

         when Alignment_LC =>

            H := H - 0.5 * (Width * Float (Text'Length) + Space * Float (Text'Length - 1));

         when Alignment_LR =>

            H := H - Width * Float (Text'Length) - Space * Float (Text'Length - 1);

         when Alignment_TL =>

            V := V - Height;

         when Alignment_TC =>

            H := H - 0.5 * (Width * Float (Text'Length) + Space * Float (Text'Length - 1));

            V := V - Height;

         when Alignment_TR =>

            H := H - Width * Float (Text'Length) - Space * Float (Text'Length - 1);

            V := V - Height;

         when Alignment_CC =>

            H := H - 0.5 * (Width * Float (Text'Length) + Space * Float (Text'Length - 1));

            V := V - 0.5 * Height;

         when others =>

            null;

      end case;

      W := Width + Space;

      for C in Text'First..Text'Last loop

         Data := Font_Data (Text (C));

         if Data.Id /= 0 then

            M2 := M1;

            Translate (M2, H, V, 0.0);

            Scale (M2, Width, Height, 1.0);

            Gl.Shaders.Load_Matrix (M2);

            Gl.Bind_Buffer (GL_ARRAY_BUFFER, Data.Id);

            Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

            Gl.Shaders.Bind_Shader (Gl.Shaders.Points_2D);

            Gl.Draw_Arrays (GL_POINTS, 0, Data.Size);

            Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

            Gl.Draw_Arrays (GL_LINES, 0, Data.Size);

         end if;

         H := H + W;

      end loop;

      Gl.Shaders.Load_Matrix (M1);

   end Draw;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- Draws the given text
   -- X, Y: the position using the current matrix transformation
   --===========================================================================
   procedure Draw (Text      : String;
                   X, Y      : Float;
                   Style     : Font_Style_Record;
                   Color     : Line_Color_Record;
                   Alignment : Font_Alignment_Types := Alignment_LL) is
   begin

      case Style.Rendering is

         when Font_Simple =>

            case Style.Thickness is

               when Font_Thin =>

                  Gl.Shaders.Load_Diameter (0.5, 0.4);

                  Gl.Shaders.Load_Width (0.6, 1.5);

               when Font_Bold =>

                  Gl.Shaders.Load_Diameter (1.5, 0.4);

                  Gl.Shaders.Load_Width (1.6, 1.5);

               when others =>

                  Gl.Shaders.Load_Diameter (1.0, 0.4);

                  Gl.Shaders.Load_Width (1.1, 1.5);

            end case;

            Gl.Shaders.Load_Color (Color.Fore);

            Draw (Text, X, Y, Style.Width, Style.Height, Style.Space, Alignment);

         when Font_Glow =>

            -- Background
            -----------------------------------

            case Style.Thickness is

               when Font_Thin =>

                  Gl.Shaders.Load_Diameter (2.0, 1.2);

                  Gl.Shaders.Load_Width (2.0, 2.5);

               when Font_Bold =>

                  Gl.Shaders.Load_Diameter (3.5, 0.4);

                  Gl.Shaders.Load_Width (3.6, 1.5);

               when others =>

                  Gl.Shaders.Load_Diameter (3.0, 0.4);

                  Gl.Shaders.Load_Width (3.0, 1.5);

            end case;

            Gl.Shaders.Load_Color (Color.Glow);

            Draw (Text, X, Y, Style.Width, Style.Height, Style.Space, Alignment);

            -- Foreground
            -----------------------------------

            case Style.Thickness is

               when Font_Thin =>

                  Gl.Shaders.Load_Diameter (0.5, 0.4);

                  Gl.Shaders.Load_Width (0.6, 1.5);

               when Font_Bold =>

                  Gl.Shaders.Load_Diameter (1.5, 0.4);

                  Gl.Shaders.Load_Width (1.6, 1.5);

               when others =>

                  Gl.Shaders.Load_Diameter (1.0, 0.4);

                  Gl.Shaders.Load_Width (1.1, 1.5);

            end case;

            Gl.Shaders.Load_Color (Color.Fore);

            Draw (Text, X, Y, Style.Width, Style.Height, Style.Space, Alignment);

      end case;

   end Draw;
   -----------------------------------------------------------------------------


end Gl.Fonts;
--------------------------------------------------------------------------------
