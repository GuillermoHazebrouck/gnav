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

-- Gnav
with Timing.Events;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Traffic is

   procedure Maintain_Tracks;

   --===========================================================================
   --
   --===========================================================================
   procedure Init is
   begin

      Timing.Events.Register_Timer (0.5, Maintain_Tracks'Access);

   end Init;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- > Marks the tracks older than 4 seconds as coasted.
   -- > Removes the tracks that are older than 8 seconds.
   --===========================================================================
   procedure Maintain_Tracks is

      Age : Duration := 0.0;

   begin

      for T in Traffic_Range loop

         if Traffic_Data (T).Active then

            Age := (Clock - Traffic_Data (T).Time_Stamp);

            if Age > 8.0 then

               Traffic_Data (T) := No_Traffic_Record;

            elsif Age > 4.0 then

               Traffic_Data (T).Coasted := True;

            end if;

         end if;

      end loop;

   end Maintain_Tracks;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Keep_Track (Element : Traffic_Record) is
   begin

      for T in Traffic_Range loop

         if Traffic_Data (T).Id = Element.Id then

            Traffic_Data (T)            := Element;

            Traffic_Data (T).Active     := True;

            Traffic_Data (T).Time_Stamp := Clock;

            Traffic_Data (T).Coasted    := False;

            return;

         end if;

      end loop;

      for T in Traffic_Range loop

         if not Traffic_Data (T).Active then

            Traffic_Data (T)            := Element;

            Traffic_Data (T).Active     := True;

            Traffic_Data (T).Time_Stamp := Clock;

            Traffic_Data (T).Coasted    := False;

            return;

         end if;

      end loop;

   end Keep_Track;
   -----------------------------------------------------------------------------

end Flight.Traffic;
--------------------------------------------------------------------------------
