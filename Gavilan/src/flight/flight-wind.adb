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
package body Flight.Wind is

   --===========================================================================
   -- Sets a manual entry for the wind. The value will be used when the source
   -- is set to manual.
   --===========================================================================
   procedure Set_Wind (Wind : Vector2_Record) is
   begin

      Last_Manual_Wind := Wind;

      Last_Manual_Wind_Time := Cached_Time;

   end Set_Wind;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compute_Wind is
   begin

      --------------------------------------------------------------------------
      -- Always make an own wind computation, so that it is available
      -- during the flight as first or second resource
      --------------------------------------------------------------------------
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

               H         : History_Range  := Current;
               J         : Natural        := 0;
               N         : Long_Float     := 0.0;
               Angle     : Long_Float     := 0.0; -- deg
               Span      : Duration       := 0.0; -- s
               Deviation : Float          := 0.0; -- deg
               Min_Turn  : constant Float := 8.0; -- deg/s
               V1, V2    : Vector2_Record;        -- m/s

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

                     Flight.Get_Previous_Index (H);

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

                        Last_Computed_Wind := V1;

                        Last_Computed_Wind.Scale (1.0 / N);

                        Last_Manual_Wind_Time := Cached_Time;

                     end if;

                  end if;

               end if;

            end;

         when others =>

            null;

      end case;

      --------------------------------------------------------------------------
      -- Decide which value to use based on the setup
      --------------------------------------------------------------------------
      case Wind_Source is

         when Wind_Source_Computation =>

            Data.Wind := Last_Computed_Wind;

            Data.Ages   (Field_Wind) := Last_Computed_Wind_Time;

            Data.Origin (Field_Wind) := Origin_Internal;

         when Wind_Source_Manual =>

            Data.Wind := Last_Manual_Wind;

            Data.Ages   (Field_Wind) := Last_Manual_Wind_Time;

            Data.Origin (Field_Wind) := Origin_Internal;

         when Wind_Source_Stream =>

            -- Thi is done during the parsing, no action required.

            null;

      end case;

   end Compute_Wind;
   -----------------------------------------------------------------------------

end Flight.Wind;
--------------------------------------------------------------------------------
