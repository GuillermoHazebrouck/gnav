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

      return This.Timestamp = This.Ages (Field);

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




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Dots_Array is array (Dots_Range) of Flight_Data_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Cluster_Record is record

      Active  : Boolean;

      Loaded  : Boolean;

      Dots    : Dots_Array;

      Last    : Dots_Range;

      Line_Id : Gl_Uint;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Cluster_Record : constant Cluster_Record := (Active  => False,
                                                   Loaded  => False,
                                                   Dots    => (others => No_Flight_Data),
                                                   Last    => Dots_Range'First,
                                                   Line_Id => 0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Cluster_Array is array (Cluster_Range) of Cluster_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   History  : Cluster_Array := (others => No_Cluster_Record);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Current  : Cluster_Range := Cluster_Range'First;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Previous : Flight_Data_Record := No_Flight_Data;




   --===========================================================================
   -- Assigns the previous dots in the history
   --===========================================================================
   procedure Get_Previous_Dot (C : in out Cluster_Range; D : in out Dots_Range) is
   begin

      if D > Dots_Range'First then

         D := D - 1;

      elsif C > Cluster_Range'First then

         C := C - 1;

      else

         C := Cluster_Range'Last;

      end if;

      D := History (C).Last;

   end Get_Previous_Dot;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Cache_Data is
   begin

      -- Check if the current cluster is full
      --------------------------------------------------------------------------

      Previous := History (Current).Dots (History (Current).Last);

      if History (Current).Last = Dots_Range'Last then

         if Current = Cluster_Range'Last then

            Current := Cluster_Range'First;

         else

            Current := Current + 1;

         end if;

         -- Reset the next cluster
         --------------------------------------------

         History (Current).Active := False;
         History (Current).Last   := Dots_Range'First;
         History (Current).Dots   := (others => No_Flight_Data);

      end if;

      -- Advance the slot
      --------------------------------------------------------------------------

      if History (Current).Active then

         History (Current).Last := History (Current).Last + 1;

      else

         History (Current).Active := True;

      end if;

      -- Load data in the next slot and mark for
      -- reloading on the GPU
      --------------------------------------------------------------------------

      History (Current).Dots (History (Current).Last) := Data;

      History (Current).Loaded := False;

   end Cache_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Clear_History is
   begin

      Current := Cluster_Range'First;

      for I in Cluster_Range loop

         History (I).Active := False;

         History (I).Loaded := False;

         History (I).Last   := Dots_Range'First;

         History (I).Dots   := (others => No_Flight_Data);

      end loop;

      Previous := No_Flight_Data;

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
               -- two times in constant curvature. Then use homologous trajectory
               -- points to compute the trayectory drift.

               if Data.Is_Update (Field_Turn) then

                  -- Compute how far in the past to go based on the current
                  -- turn rate.
                  --------------------------------------------------------------

                  if Data.Turn < 8.0 then

                     -- The turn rate is too slow for wind computation
                     -------------------------------------------------

                     return;

                  end if;

                  -- Check if the turn has been maintained for the
                  -- necessary time span to make two complete turns
                  -----------------------------------------------------

                  declare

                     Actual    : Time     := Clock;
                     Time_Span : constant Duration := Duration (720.0 / Data.Turn);
                     Deviation : Float    := 0.0;
                     Complete  : Boolean  := False;

                     Ci : constant Cluster_Range := Current;
                     Di : constant Dots_Range    := History (Ci).Last;
                     Cj : Cluster_Range          := Ci;
                     Dj : Dots_Range             := Di;
                     Ck : Cluster_Range          := Ci;
                     Dk : Dots_Range             := Di;

                  begin

                     loop

                        -- Skip data that is not valid
                        --------------------------------------------------------
                        if History (Cj).Dots (Dj).Is_Update (Field_Turn) then

                           Deviation := abs ((Data.Turn - History (Cj).Dots (Dj).Turn) / Data.Turn);

                           -- Stop if the turn rate is not constant
                           ----------------------------------------
                           exit when Deviation > 0.15;

                        end if;

                        -- Stop when reached the necessary time span
                        --------------------------------------------------------
                        if Actual - History (Cj).Dots (Dj).Timestamp > Time_Span then

                           Complete := True;

                           exit;

                        end if;

                        Get_Previous_Dot (Cj, Dj);

                        -- Stop in any case when reaching the first dot
                        --------------------------------------------------------
                        exit when Cj = Ci and Dj = Di;

                     end loop;

                     if Complete then

                        -- There is a sufficentyl long constant turn
                        -- Compare homologous points in the turn
                        --------------------------------------------------------

                        Ck := Ci;
                        Dk := Di;

                        loop

                           Get_Previous_Dot (Ck, Dk);

                           -- Stop in any case when reaching the last dot
                           -----------------------------------------------------
                           exit when Ck = Cj and Dk = Dj;

                        end loop;

                     end if;

                  end;

               end if;

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

      Ci : constant Cluster_Range := Current;
      Di : constant Dots_Range    := History (Ci).Last;
      Cj : Cluster_Range          := Ci;
      Dj : Dots_Range             := Di;

   begin

      if
        Data.Origin (Field_Course) = Origin_Internal or
        Data.Age    (Field_Course) > 2.0
      then

         for I in 1..4 loop

            if not Last_Four (I).Is_Update (Field_Course) then

               return;

            end if;

            Course (I) := Last_Four (I).Course;

            Q1 := Q1 or Course (I) <=  90.0;

            Q4 := Q4 or Course (I) >= 270.0;

            Ada.Text_IO.Put_Line ("course" & Natural'Image (I) & "=" & Float'Image (Course (I)));

         end loop;

         -- Che
      end if;

   end Compute_Course;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compute_Turn_Rate is

      Courses : array (1..4) of Float := (others => 0.0);
      Q1, Q4  : Boolean := False;
      Step    : Float;

   begin

      -- When there is no turn rate from the stream (too old), compute the turn
      -- rate using the course of the previous four steps.
      -- We need four valid points in the history.
      --------------------------------------------------------------------------

      if
        Data.Origin (Field_Turn) = Origin_Internal or
        Data.Age    (Field_Turn) > 2.0
      then

         for I in 1..4 loop

            if not Last_Four (I).Is_Valid (Field_Course) then

               return;

            end if;

            Course (I) := Last_Four (I).Course;

            Q1 := Q1 or Course (I) <=  90.0;

            Q4 := Q4 or Course (I) >= 270.0;

            Ada.Text_IO.Put_Line ("course" & Natural'Image (I) & "=" & Float'Image (Course (I)));

         end loop;

         -- Check if the course is changing between quadrants from 1 and 4
         -----------------------------------------------------------------------
         if Q1 and Q4 then

            for I in 1..4 loop

               if Course (I) <= 90.0 then

                  Course (I) := Course (I) + 360.0;

               end if;

            end loop;

         end if;

         Step := 0.0;

         for I in 2..4 loop

            Step := Step + Float (Last_Four (I).Timestamp - Last_Four (I-1).Timestamp);

         end loop;

         Step := Step / 3.0;

         Ada.Text_IO.Put_Line ("step =" & Float'Image (Step));

         -- Calculate derivative using finite difference formula
         -- (4 points in the past)
         -----------------------------------------------------------------------

         if Step > 0.0 then

            Data.Turn := (-2.0 * Course (1) + 9.0 * Course (1) - 18.0 * Course (3) + 11.0 * Course (4)) / (6.0 * Step);

            Data.Ages   (Field_Turn) := Clock;

            Data.Origin (Field_Turn) := Origin_Internal;

            Ada.Text_IO.Put_Line ("turn =" & Float'Image (Data.Turn));

         end if;

      end if;

   end Compute_Turn_Rate;
   -----------------------------------------------------------------------------

end Flight;
--------------------------------------------------------------------------------
