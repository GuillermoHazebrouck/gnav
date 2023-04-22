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
with Ada.Streams;
use  Ada.Streams;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Utility.Streams is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The two tipes of endiannes
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Endianness_Types is (Little_Endian, Big_Endian);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Allows a sequential reading of a memory buffer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stream_Buffer_Type is new Root_Stream_Type with record

      Endianness_Type : Endianness_Types := Little_Endian;

      End_Off_Buffer  : Boolean := False;

      Cursor : Stream_Element_Offset := Stream_Element_Offset'First;

      Buffer : access Stream_Element_Array := null;

   end record;
   -----------------------------------------------------------------------------

   type Stream_Buffer_Access is not null access Stream_Buffer_Type;

   --===========================================================================
   -- Reads on the stream at the current position
   --===========================================================================
   procedure Read (This : in out Stream_Buffer_Type;
                   Item : out Stream_Element_Array;
                   Last : out Stream_Element_Offset);

   --===========================================================================
   -- Loads the stream from another stream
   --===========================================================================
   procedure Write (This : in out Stream_Buffer_Type;
                    Item : Stream_Element_Array);

   --===========================================================================
   --
   --===========================================================================
   procedure Advance_Cursor (This : in out Stream_Buffer_Type;
                             Step : Stream_Element_Offset);
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   procedure Move_Cursor (This   : in out Stream_Buffer_Type;
                          Cursor : Stream_Element_Offset);

end Utility.Streams;
--------------------------------------------------------------------------------
