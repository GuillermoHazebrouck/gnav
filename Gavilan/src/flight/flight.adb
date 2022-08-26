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
use  Gl;
with Gl.Resources;
with Gl.Shaders;
with Maps;
use  Maps;
with Utility.Log;


--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Age (This : Flight_Data_Record; Field : Data_Field_Kind) return Duration is
   begin

      return Cached_Time - This.Ages (Field);

   end Age;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Relative_Age (This : Flight_Data_Record; Field : Data_Field_Kind) return Duration is
   begin

      return This.Timestamp - This.Ages (Field);

   end Relative_Age;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Is_Update (This : Flight_Data_Record; Field : Data_Field_Kind) return Boolean is
   begin

      return This.Origin (Field) /= Origin_None;

   end Is_Update;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Is_Recent (This : Flight_Data_Record; Field : Data_Field_Kind) return Boolean is
   begin

      return Cached_Time - This.Ages (Field) <= 2.0;

   end Is_Recent;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Cache_Timestamp (This : in out Flight_Data_Record; Field : Data_Field_Kind) is
   begin

      This.Ages (Field) := This.Timestamp;

   end Cache_Timestamp;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the previous index in the circular history buffer
   --===========================================================================
   procedure Get_Previous_Index (Index : in out History_Range) is
   begin

      if Index > History_Range'First then

         Index := Index - 1;

      else

         Index := History_Range'Last;

      end if;

   end Get_Previous_Index;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the next index in the circular history buffer
   --===========================================================================
   procedure Get_Next_Index (Index : in out History_Range) is
   begin

      if Index < History_Range'Last then

         Index := Index + 1;

      else

         Index := History_Range'First;

      end if;

   end Get_Next_Index;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Cache_Data is
   begin

      Previous := History (Current);

      Get_Next_Index (Current);

      History (Current) := Data;

      On_Data_Cached.Trigger;

   end Cache_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Clear_History is
   begin

      Current := History_Range'First;

      History := (others => No_Flight_Data);

      Data     := No_Flight_Data;

      Previous := No_Flight_Data;

      On_Data_Cleared.Trigger;

   end Clear_History;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Previous return Flight_Data_Record is
   begin

      return Previous;

   end Get_Previous;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compute_Wind is
   begin

      if Wind_Source = Wind_Source_Computation then

         case Wind_Computation is

            when Wind_Computation_Differential =>

               if
                 Data.Is_Update (Field_Airspeed) and
                 Data.Is_Update (Field_Speed)    and
                 Data.Is_Update (Field_Course)   and
                 Data.Is_Update (Field_Heading)
               then

                  null;

               end if;

            when Wind_Computation_Path_Drift =>

               -- Go back in the history and check if we have turned more than
               -- 360 deg in (quasi) free motion. Then use homologous
               -- trajectory points to compute the trayectory drift.
               --
               -- NOTE:
               -- > The turn-rate must remain above a 8 deg/s.
               -- > The turn-rate must not vary more thant 20%.
               --
               -- NOTE:
               -- > The algorithm is simple and might not always be accurate,
               --   specially when there are large corrections. The calculation
               --   will stop if the trajectory is corrected considerably, but
               --   small corrections might pollute the result inadvertently.
               -- > The algorithm might not work in strong wind (high path
               --   curvature expected).
               -- > In simulations the algorithm seems to work quite well.
               -- > A better algorithm might be trying to fit a cicloid using
               --   least squares.
               --
               -----------------------------------------------------------------

               declare

                  H         : History_Range := Current;
                  J         : Natural    := 0;
                  N         : Long_Float := 0.0;
                  Angle     : Long_Float := 0.0;     -- deg
                  Span      : Duration   := 0.0;     -- s
                  Deviation : Float      := 0.0;     -- deg
                  V1, V2    : Vector2_Record;        -- m/s
                  Min_Turn  : constant Float := 8.0; -- deg/s

               begin

                  if
                    Data.Is_Update (Field_Turn) and
                    Data.Is_Update (Field_Course)
                  then

                     if abs Data.Turn < Min_Turn then

                        -- The turn rate is too slow for wind computation
                        --------------------------------------------------------

                        return;

                     end if;

                     V1.Set_From_Polar (Long_Float (Data.Course) / 180.0 * Math.Pi, 1.0);

                     -- Check if the turn has been maintained for the
                     -- necessary time span to make two complete turns
                     -----------------------------------------------------------

                     for K in History_Range loop

                        Get_Previous_Index (H);

                        -- Skip data that is not valid
                        --------------------------------------------------------
                        if
                          History (H).Is_Update (Field_Turn)   and
                          History (H).Is_Update (Field_Course) and
                          History (H).Turn > Min_Turn
                        then

                           Deviation := abs ((Data.Turn - History (H).Turn) / Data.Turn);

                           V2.Set_From_Polar (Long_Float (History (H).Course) / 180.0 * Math.Pi, 1.0);

                           Angle := Angle + V1.Angle (V2) * 180.0 / Math.Pi;

                           V1 := V2;

                           Span := Data.Timestamp - History (H).Timestamp;

                           exit when abs Angle > 360.0 or Deviation > 0.2;

                        end if;

                     end loop;

                     if Span > 0.0 and abs Angle > 360.0 and Deviation < 0.2 then

                        Data.Blow := Vector (History (H).Position, Data.Position);

                        Data.Blow.Scale (1000.0 / Long_Float (Span));

                        Data.Ages   (Field_Blow) := Cached_Time;

                        Data.Origin (Field_Blow) := Origin_Internal;

                     end if;

                     -- Take the mean blow during the last 5 minutes
                     -----------------------------------------------------------

                     if Data.Is_Update (Field_Blow) then

                        H := Current;

                        V1 := Data.Blow;

                        N  := 1.0;

                        for K in History_Range loop

                           Get_Previous_Index (H);

                           -- Skip data that is not valid
                           -----------------------------------------------------
                           if History (H).Is_Update (Field_Blow) then

                              V1 := V1 + History (H).Blow;

                              N  := N + 1.0;

                              Span := Data.Timestamp - History (H).Timestamp;

                              exit when Span > 300.0;

                           end if;

                        end loop;

                        if N > 0.0 then

                           Data.Wind := V1;

                           Data.Wind.Scale (1.0 / N);

                           Data.Ages   (Field_Wind) := Cached_Time;

                           Data.Origin (Field_Wind) := Origin_Internal;

                        end if;

                     end if;

                  end if;

               end;

            when others =>

               null;

         end case;

      end if;

   end Compute_Wind;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compute_Course is
   begin

      null;

   end Compute_Course;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compute_Turn_Rate is

      H      : History_Range := Current;
      Angle  : Float    := 0.0;
      Span   : Duration := 0.0;
      V1, V2 : Vector2_Record;

   begin

      -- The turn is computed as the mean course variation during the last 5s
      --------------------------------------------------------------------------

      if Data.Is_Update (Field_Course) and not Data.Is_Update (Field_Turn) then

         V1.Set_From_Polar (Long_Float (Data.Course) / 180.0 * Math.Pi, 1.0);

         for K in History_Range loop

            Get_Previous_Index (H);

            if History (H).Is_Update (Field_Course) then

               exit when Cached_Time - History (H).Timestamp > 5.0;

               V2.Set_From_Polar (Long_Float (History (H).Course) / 180.0 * Math.Pi, 1.0);

               Angle := Angle + Float (V1.Angle (V2) * 180.0 / Math.Pi);

               V1 := V2;

               Span := Data.Timestamp - History (H).Timestamp;

            end if;

         end loop;

         if Span > 0.0 then

            Data.Turn := Angle / Float (Span);

            Data.Ages   (Field_Turn) := Cached_Time;

            Data.Origin (Field_Turn) := Origin_Internal;

         end if;

      end if;

   end Compute_Turn_Rate;
   -----------------------------------------------------------------------------

end Flight;
--------------------------------------------------------------------------------
