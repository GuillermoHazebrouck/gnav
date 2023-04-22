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
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Streams is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Read (This : in out Stream_Buffer_Type;
                   Item : out Stream_Element_Array;
                   Last : out Stream_Element_Offset) is

      J : Stream_Element_Offset := 0;

   begin

      if This.Cursor + Item'Length - 1 in This.Buffer'Range then

         case This.Endianness_Type is

            when Little_Endian =>

               for I in Item'Range loop

                  Item (I) := This.Buffer (This.Cursor + J);

                  J := J + 1;

               end loop;

            when Big_Endian =>

            for I in reverse Item'Range loop

               Item (I) := This.Buffer (This.Cursor + J);

               J := J + 1;

            end loop;

         end case;

         This.Cursor := This.Cursor + Item'Length;

         Last := Item'Last;

         if This.Cursor > This.Buffer'Last then

            This.End_Off_Buffer := True;

            Utility.Log.Put_Message ("end of buffer");

         end if;

      else

         This.End_Off_Buffer := True;

         This.Cursor := This.Buffer'Last + 1;

         Item := (others => 0);

         Last := Item'Last;

         Utility.Log.Put_Message ("buffer out of range");

      end if;

   end Read;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Write (This : in out Stream_Buffer_Type;
                    Item : Stream_Element_Array) is

      J : Stream_Element_Offset := 0;

   begin

      if not (This.Cursor + Item'Length in This.Buffer'Range) then

         This.End_Off_Buffer := True;

         This.Cursor := This.Buffer'Last;

         Utility.Log.Put_Message ("end of buffer");

         return;

      end if;

      case This.Endianness_Type is

         when Little_Endian =>

            for I in Item'Range loop

               This.Buffer (This.Cursor + J) := Item (I);

               J := J + 1;

            end loop;

         when Big_Endian =>

            for I in reverse Item'Range loop

               This.Buffer (This.Cursor + J) := Item (I);

               J := J + 1;

            end loop;

      end case;

      This.Cursor := This.Cursor + Item'Length;

   end Write;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Advance_Cursor (This : in out Stream_Buffer_Type;
                             Step : Stream_Element_Offset) is
   begin

      This.Cursor := This.Cursor + Step;

      if not (This.Cursor + Step in This.Buffer'Range) then

         This.End_Off_Buffer := True;

         This.Cursor := This.Buffer'Last;

         Utility.Log.Put_Message ("end of buffer");

      end if;

   end Advance_Cursor;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Move_Cursor (This   : in out Stream_Buffer_Type;
                          Cursor : Stream_Element_Offset) is
   begin

      This.Cursor := Cursor;

      if not (This.Cursor in This.Buffer'Range) then

         This.End_Off_Buffer := True;

         This.Cursor := This.Buffer'Last;

         Utility.Log.Put_Message ("end of buffer");

      end if;

   end Move_Cursor;
   -----------------------------------------------------------------------------

end Utility.Streams;
--------------------------------------------------------------------------------
