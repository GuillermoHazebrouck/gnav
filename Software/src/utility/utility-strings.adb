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

-- External
with Ada.Text_IO;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Strings is

   --===========================================================================
   -- To dynamic string
   --===========================================================================
   function "&" (Left : Dynamic_String; Right : String) return Dynamic_String is
   begin
      return Left & To_Unbounded_String (Right);

   end;
   -----------------------------------------------------------------------------

   subtype Upper_Case_Range is Character range 'A'..'Z';

   subtype Lower_Case_Range is Character range 'a'..'z';

   type Upper_Case_Characters is array (Upper_Case_Range) of Lower_Case_Range;

   type Lower_Case_Characters is array (Lower_Case_Range) of Upper_Case_Range;

   Upper_Case_Dictionary : constant Upper_Case_Characters := ('A' => 'a',
                                                              'B' => 'b',
                                                              'C' => 'c',
                                                              'D' => 'd',
                                                              'E' => 'e',
                                                              'F' => 'f',
                                                              'G' => 'g',
                                                              'H' => 'h',
                                                              'I' => 'i',
                                                              'J' => 'j',
                                                              'K' => 'k',
                                                              'L' => 'l',
                                                              'M' => 'm',
                                                              'N' => 'n',
                                                              'O' => 'o',
                                                              'P' => 'p',
                                                              'Q' => 'q',
                                                              'R' => 'r',
                                                              'S' => 's',
                                                              'T' => 't',
                                                              'U' => 'u',
                                                              'V' => 'v',
                                                              'W' => 'w',
                                                              'X' => 'x',
                                                              'Y' => 'y',
                                                              'Z' => 'z');

   Lower_Case_Dictionary : constant Lower_Case_Characters := ('a' => 'A',
                                                              'b' => 'B',
                                                              'c' => 'C',
                                                              'd' => 'D',
                                                              'e' => 'E',
                                                              'f' => 'F',
                                                              'g' => 'G',
                                                              'h' => 'H',
                                                              'i' => 'I',
                                                              'j' => 'J',
                                                              'k' => 'K',
                                                              'l' => 'L',
                                                              'm' => 'M',
                                                              'n' => 'N',
                                                              'o' => 'O',
                                                              'p' => 'P',
                                                              'q' => 'Q',
                                                              'r' => 'R',
                                                              's' => 'S',
                                                              't' => 'T',
                                                              'u' => 'U',
                                                              'v' => 'V',
                                                              'w' => 'W',
                                                              'x' => 'X',
                                                              'y' => 'Y',
                                                              'z' => 'Z');

   --===========================================================================
   --
   --===========================================================================
   procedure Load (Object : in out String_Buffer; Data : String) is
   begin

      if Data'Length = 0 then

         Object.Loaded := False;

         Object.Offset := 1;

      elsif Data'Length <= Object.Buffer_Size then

         Object.Loaded := True;

         Object.Buffer (1..Data'Length) := Data;

         Object.Length := Data'Length;

         Object.Offset := 1;

      else

         Object.Loaded := True;

         Object.Buffer := Data (Data'First..Data'First - 1 + Object.Buffer_Size);

         Object.Length := Object.Buffer_Size;

         Object.Offset := 1;

         Ada.Text_IO.Put_Line("Warning: the buffer has been trimmed");

      end if;

   end Load;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Current (Object : String_Buffer) return Character is
   begin

      return Object.Buffer (Object.Offset);

   end Current;
   pragma Inline (Current);
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   function Read_Next (Object    : in out String_Buffer;
                       Separator : Character := ' ') return String is

      I : Positive := Object.Offset;
      J : Positive := Object.Offset;

   begin

      if Object.Loaded then

         -- Case A: null string
         -----------------------------------------------------------------------
         if Object.Current = Separator then

            Object.Offset := Object.Offset + 1;

            return "";

         -- Case B: some string
         -----------------------------------------------------------------------
         else

            I := Object.Offset;
            J := Object.Offset;

            -- Move to the next separator
            --------------------------------------------------------------------
            while Object.Offset < Object.Length and then Object.Current /= Separator loop

               J := Object.Offset;

               Object.Offset := Object.Offset + 1;

            end loop;

            -- Check if the end has been reached
            --------------------------------------------------------------------
            if Object.Offset = Object.Length then

               if Object.Current /= Separator then

                  J := Object.Offset;

               end if;

               Object.Loaded := False;

            else

               -- Move the offset behind the last separator
               -----------------------------------------------------------------

               Object.Offset := Object.Offset + 1;

            end if;

            return Object.Buffer (I..J);

         end if;

      else

         return "";

      end if;

   end Read_Next;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Move_To_Next (Object    : in out String_Buffer;
                           Separator : Character := ' ') is
   begin

      if Object.Loaded then

         -- Move to the next separator
         --------------------------------------------------------------------
         while Object.Offset < Object.Length and then Object.Current /= Separator loop

            Object.Offset := Object.Offset + 1;

         end loop;

         -- Check if the end has been reached
         --------------------------------------------------------------------
         if Object.Offset = Object.Length then

            Object.Loaded := False;

         end if;

      end if;

   end Move_To_Next;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function End_Of_Stream (Object : in out String_Buffer) return Boolean is
   begin

      return not Object.Loaded;

   end End_Of_Stream;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Dump_Content (Object : in out String_Buffer) is
   begin

      if Object.Loaded then

         Ada.Text_IO.Put_Line (Object.Buffer (1..Object.Length));

      end if;

   end Dump_Content;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Upper_Case (Text : String) return String is

      New_Text : String := Text;

   begin

      for I in 1..New_Text'Length loop

         if New_Text (I) in Lower_Case_Range then

            New_Text (I) := Lower_Case_Dictionary (New_Text (I));

         end if;

      end loop;

      return New_Text;

   end Get_Upper_Case;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Lower_Case (Text : String) return String is

      New_Text : String := Text;

   begin

      for I in 1..New_Text'Length loop

         if New_Text (I) in Upper_Case_Range then

            New_Text (I) := Upper_Case_Dictionary (New_Text (I));

         end if;

      end loop;

      return New_Text;

   end Get_Lower_Case;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Override (Text_1: in out String; Text_2 : String; Fill : Character := ' '; Reversed : Boolean := False) is

      J : Natural := Text_2'First;

   begin

      if Text_2'Length = 0 then

         Text_1 := (others => Fill);

         return;

      end if;

      if Reversed then

         J := Text_2'Last;

         for I in reverse Text_1'First..Text_1'Last loop

            if J >= Text_2'First then

               Text_1 (I) := Text_2 (J);

               J := J - 1;

            else

               Text_1 (I) := Fill;

            end if;

         end loop;

      else

         for I in Text_1'First..Text_1'Last loop

            if J <= Text_2'Last then

               Text_1 (I) := Text_2 (J);

               J := J + 1;

            else

               Text_1 (I) := Fill;

            end if;

         end loop;

      end if;

   end Override;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Contains (Text : String; Pattern : String) return Boolean is

      Last : Natural;

   begin

      for I in Text'Range loop

         Last := I + Pattern'Length - 1;

         if Last <= Text'Last then

            if Text (I..Last) = Pattern then

               return True;

            end if;

         else

            return False;

         end if;

      end loop;

      return False;

   end Contains;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Trim (Text : String) return String is

      M, N : Positive := 1;

      Empty : Boolean := True;

   begin

      -- Leading character
      -----------------------------------
      for I in Text'First..Text'Last loop

         if Text (I) /= ' ' then

            Empty := False;

            M := I;

            exit;

         end if;

      end loop;

      if not Empty then

         -- Trailing character
         -----------------------------------
         for J in reverse Text'First..Text'Last loop

            if Text (J) /= ' ' then

               N := J;

               exit;

            end if;

         end loop;

         return Text (M..N);

      else

         return "";

      end if;

   end Trim;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Replace (Text : String; Original : Character; Target : Character) return String is

      New_Text : String := Text;

   begin

      for I in New_Text'Range loop

         if New_Text (I) = Original then

            New_Text (I) := Target;

         end if;

      end loop;

      return New_Text;

   end Replace;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Integer_Image (Value : Integer) return String is
   begin

      return Trim (Integer'Image (Value));

   end Integer_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Float_Image (Value : Float; Decimals : Natural) return String is

      Factor : Float := 10.0 ** Decimals;

      Number : String := Trim (Integer'Image (Integer (Value * Factor)));

      --------------------------------------------------------------------------
      --
      --------------------------------------------------------------------------
      function Complete_Decimals return String is

         Fraction : String (1..Decimals) := (others => '0');

         Offset   : Natural := Decimals - Number'Length;

         C        : Natural := Fraction'First;

      begin

         for I in Number'Range loop

            Fraction (Offset + C) := Number (I);

            C := C + 1;

         end loop;

         return Fraction;

      end Complete_Decimals; pragma Inline (Complete_Decimals);
      --------------------------------------------------------------------------

   begin

      if Decimals = 0 then

         return Number;

      else

         if Decimals >= Number'Length then

            return "0." & Complete_Decimals;

         else

            return Number (Number'First..Number'Last-Decimals) & "." & Number (Number'Last-Decimals+1..Number'Last);

         end if;

      end if;

   end Float_Image;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Time_Image (Value : Duration) return String is

      Hours   : Float   := Float (Value / (3600.0));
      H       : Natural := Natural (Float'Floor (Hours));
      M       : Natural := Natural (Float'Floor ((Hours - Float (H)) * 60.0));
      HH      : String  := "00";
      MM      : String  := "00";

   begin

      Override (HH, Trim (Natural'Image (H)), '0', True);

      Override (MM, Trim (Natural'Image (M)), '0', True);

      return HH & ":" & MM;

   end Time_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Hour_Image (Value : Duration) return String is

      Hours   : Float   := Float (Value / (3600.0));
      H       : Natural := Natural (Float'Floor (Hours));
      HH      : String  := "00";

   begin

      Override (HH, Trim (Natural'Image (H)), '0', True);

      return HH;

   end Hour_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Minute_Image (Value : Duration) return String is

      Hours   : Float   := Float (Value / (3600.0));
      H       : Natural := Natural (Float'Floor (Hours));
      M       : Natural := Natural (Float'Floor ((Hours - Float (H)) * 60.0));
      MM      : String  := "00";

   begin

      Override (MM, Trim (Natural'Image (M)), '0', True);

      return MM;

   end Minute_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Second_Image (Value : Duration) return String is

      Minutes : Float   := Float (Value / (60.0));
      M       : Natural := Natural (Float'Floor (Minutes));
      S       : Natural := Natural (Float'Floor ((Minutes - Float (M)) * 60.0));
      SS      : String  := "00";

   begin

      Override (SS, Trim (Natural'Image (S)), '0', True);

      return SS;

   end Second_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Time_Image (Value : Time) return String is
   begin

      return Time_Image (Ada.Calendar.Seconds (Value));

   end Time_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Hour_Image (Value : Time) return String is
   begin

      return Hour_Image (Ada.Calendar.Seconds (Value));

   end Hour_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Minute_Image (Value : Time) return String is
   begin

      return Minute_Image (Ada.Calendar.Seconds (Value));

   end Minute_Image;
   -----------------------------------------------------------------------------

end Utility.Strings;
--------------------------------------------------------------------------------
