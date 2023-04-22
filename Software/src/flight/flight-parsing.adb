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
with Ada.Command_Line;
with Ada.Directories;
-- Gnav
with Flight.Signal;
with Flight.Traffic;
with Flight.Wind;
use  Flight.Wind;
with Maps.Terrain;
with Timing.Events;
with Utility.Log;
with Utility.Strings;
use  Utility.Strings;
with Utility.Units;
use  Utility.Units;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Parsing is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the incoming messges must be prompted
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Log_Messages : Boolean := True;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Signal_Timer : access Timing.Events.Timer_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Error_Timer : access Timing.Events.Timer_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Error_Found : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Error  : Time := Clock - 60.0;

   --===========================================================================
   --
   --===========================================================================
   procedure Clear_Satellites is
      use Flight.Signal;
   begin

      for K in Satellite_Kinds loop

         Satellites := (others => (others => No_Satellite));

      end loop;

      Number_Of_Satellites := (others => 0);

   end Clear_Satellites;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Clear_Error is
   begin

      Error_Found := False;

   end Clear_Error;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- Checks the validity of the datafields
   --===========================================================================
   procedure Check_Data is

      use Maps.Terrain;

      Previous_Position : Position_Record := Flight.Get_Previous.Position;

   begin

      -- Calculate elevation (if not externally given or too old)
      --------------------------------------------------------------------------

      if
        Flight.Data.Origin (Field_Elevation) = Origin_Internal or
        Flight.Data.Age    (Field_Elevation) > 2.0
      then

         if Flight.Data.Is_Update (Field_Altitude) then

            Flight.Data.Elevation := Flight.Data.Altitude - Get_Elevation (Flight.Data.Position);

            Flight.Data.Ages   (Field_Elevation) := Cached_Time;

            Flight.Data.Origin (Field_Elevation) := Origin_Internal;

         end if;

      end if;

      -- Use backup method for course
      --------------------------------------------------------------------------

      if
        Flight.Data.Origin (Field_Course) = Origin_Internal or
        Flight.Data.Age    (Field_Course) > 2.0
      then

         if Previous_Position /= Flight.Data.Position then

            Maps.Coordinates (Position_A => Previous_Position,
                              Position_B => Flight.Data.Position,
                              Distance   => Flight.Data.Step,
                              Bearing    => Flight.Data.Course);

            Flight.Data.Ages   (Field_Course) := Cached_Time;

            Flight.Data.Origin (Field_Course) := Origin_Internal;

         end if;

      end if;

      -- Compute the turn rate if necessary
      --------------------------------------------------------------------------
      Flight.Compute_Turn_Rate;

      -- Compute the wind if necessary
      --------------------------------------------------------------------------
      Flight.Wind.Compute_Wind;

   end Check_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Decodes a G-NAV message (for Flight-Gear or other sources)
   --===========================================================================
   procedure Parse_Gnav_Message (Message : String) is

      Reader : String_Buffer (Message'Size);

   begin

      if Log_Messages then

         Utility.Log.Put_Message ("received " & Message);

      end if;

      Message_Counter := Message_Counter + 1;

      Reader.Load (Message);

      loop

         declare
            Key : String := Reader.Read_Next ('=');
            Val : String := Reader.Read_Next (';');
         begin

            exit when Key'Length = 0;

            case Key (Key'First) is

               when 'X' =>

                  Flight.Data.Position.Lon := Long_Float'Value (Val);

                  Flight.Data.Ages   (Field_Position) := Cached_Time;

                  Flight.Data.Origin (Field_Position) := Origin_External;

               when 'Y' =>

                  Flight.Data.Position.Lat := Long_Float'Value (Val);

                  Flight.Data.Ages   (Field_Position) := Cached_Time;

                  Flight.Data.Origin (Field_Position) := Origin_External;

               when 'A' =>

                  Flight.Data.Altitude := Float'Value (Val);

                  Flight.Data.Ages   (Field_Altitude) := Cached_Time;

                  Flight.Data.Origin (Field_Altitude) := Origin_External;

               when 'S' =>

                  Flight.Data.Speed := Convert (Float'Value (Val), Unit_Kilometer_Hour, Unit_Meter_Second);

                  Flight.Data.Ages   (Field_Speed) := Cached_Time;

                  Flight.Data.Origin (Field_Speed) := Origin_External;

               when 'H' =>

                  Flight.Data.Heading := Float'Value (Val);

               when 'C' =>

                  Flight.Data.Course := Float'Value (Val);

                  Flight.Data.Ages   (Field_Course) := Cached_Time;

                  Flight.Data.Origin (Field_Course) := Origin_External;

               when 'W' =>

                  if Flight.Wind.Get_Source = Wind_Source_Stream then

                     -- NOTE: the FlightGear convension is used for the wind.
                     -- Y => North wind (+ means coming from the north)
                     -- X => East wind  (+ means coming from the east)

                     if Key'Length > 1 then

                        case Key (Key'First+1) is

                        when 'X' =>

                           Flight.Data.Wind.Set_X (-Long_Float'Value (Val));

                        when 'Y' =>

                           Flight.Data.Wind.Set_Y (-Long_Float'Value (Val));

                        when others => null;

                        end case;

                     end if;

                     Flight.Data.Ages   (Field_Wind) := Cached_Time;

                     Flight.Data.Origin (Field_Wind) := Origin_External;

                  end if;

               when 'V' =>

                  Flight.Data.Airspeed := Float'Value (Val);

               when others =>
                  null;

            end case;

         end;

      end loop;

      -- Check and augment data
      -----------------------------------------------
      Check_Data;

   exception
      when E : others =>

         Utility.Log.Put_Message (E, "error while parsing G-NAV message {" & Message & "}");

   end Parse_Gnav_Message;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Nmea_Data_Items is (Gpgga_Message_Id,
                            Gpgga_Utc_Time,
                            Gpgga_Latitude,
                            Gpgga_Ns_Indicator,
                            Gpgga_Longitude,
                            Gpgga_Ew_Indicator,
                            Gpgga_Position_Fix,
                            Gpgga_Satellites_Used,
                            Gpgga_Hdop,
                            Gpgga_Altitude,
                            Gpgga_Altitude_Unit,
                            Gpgga_Geoid_Separation,
                            Gpgga_Geoid_Separation_Unit,
                            Gpgga_Age_Of_Diff_Corr,
                            Gpgga_Diff_Ref_Station_Id,
                            Gpgga_End,

                            Gnrmc_Message_Id,
                            Gnrmc_Utc_Time,
                            Gnrmc_Status,
                            Gnrmc_Latitude,
                            Gnrmc_Ns_Indicator,
                            Gnrmc_Longitude,
                            Gnrmc_Ew_Indicator,
                            Gnrmc_Ground_Speed,
                            Gnrmc_Ground_Course,
                            Gnrmc_Date,
                            Gnrmc_Magnetic_Variation,
                            Gnrmc_East_West_Indicator,
                            Gnrmc_Mode,
                            Gnrmc_End,

                            Pflaa_Message_Id,
                            Pflaa_Alarm_Level,
                            Pflaa_Relative_North,
                            Pflaa_Relative_East,
                            Pflaa_Relative_Vertical,
                            Pflaa_Id_Type,
                            Pflaa_Id,
                            Pflaa_Track,
                            Pflaa_Turn_Rate,
                            Pflaa_Ground_Speed,
                            Pflaa_Climb_Rate,
                            Pflaa_Aircraft_Type,

                            Gpgsv_Message_Id,
                            Gpgsv_Message_Count,
                            Gpgsv_Message_Number,
                            Gpgsv_Satellites_Count,
                            Gpgsv_Satellite_1_Id,
                            Gpgsv_Satellite_1_Elevation,
                            Gpgsv_Satellite_1_Azimuth,
                            Gpgsv_Satellite_1_Strength,
                            Gpgsv_Satellite_2_Id,
                            Gpgsv_Satellite_2_Elevation,
                            Gpgsv_Satellite_2_Azimuth,
                            Gpgsv_Satellite_2_Strength,
                            Gpgsv_Satellite_3_Id,
                            Gpgsv_Satellite_3_Elevation,
                            Gpgsv_Satellite_3_Azimuth,
                            Gpgsv_Satellite_3_Strength,
                            Gpgsv_Satellite_4_Id,
                            Gpgsv_Satellite_4_Elevation,
                            Gpgsv_Satellite_4_Azimuth,
                            Gpgsv_Satellite_4_Strength,
                            Gpgsv_End
                            );

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Nmea_Gpgga_Items is Nmea_Data_Items range Gpgga_Utc_Time..Gpgga_End;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Nmea_Gnrmc_Items is Nmea_Data_Items range Gnrmc_Utc_Time..Gnrmc_End;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Flarm extension
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Nmea_Pflaa_Items is Nmea_Data_Items range Pflaa_Alarm_Level..Pflaa_Aircraft_Type;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Nmea_Gpgsv_Items is Nmea_Data_Items range Gpgsv_Message_Count..Gpgsv_Satellite_4_Strength;



   --===========================================================================
   -- Synchronizes the indicated time to the UTC time
   --===========================================================================
   procedure Synchronize_Time (Value : String) is
   begin

      -- Maximum one synchronization per 45s
      -----------------------------------------------
      if abs (Cached_Time - Last_Time_Sync) > 45.0 then

         if Value'Length > 5 then

            declare

               -- Data must be in HHMMSS.SS format
               -- The clock is corrected if the difference is
               -- more than 30 seconds.

               Gps_Time : Time;
               Gps_Span : Duration;
               Offset   : Duration;

            begin

               Gps_Span := Duration'Value (Value (Value'First  ..Value'First+1)) * 3600.0 +
                           Duration'Value (Value (Value'First+2..Value'First+3)) * 60.0   +
                           Duration'Value (Value (Value'First+4..Value'Last   ));

               Gps_Time := Ada.Calendar.Time_Of (Year    => Year  (Cached_Time),
                                                 Month   => Month (Cached_Time),
                                                 Day     => Day   (Cached_Time),
                                                 Seconds => Gps_Span);

               Offset := Gps_Time - Cached_Time;

               if abs (Clock_Offset - Offset) > 15.0 then

                  Clock_Offset := Offset;

               end if;

               Last_Time_Sync := Cached_Time;

            exception
               when others =>
                  Clock_Offset := 0.0;
                  Utility.Log.Put_Message ("time synchronization failed");

            end;

         end if;

      end if;

   end Synchronize_Time;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Parse_Nmea_Sentence (Sentence : String) is

      Reader   : String_Buffer (Sentence'Size);
      Fix      : Position_Record := No_Position_Record;
      Altitude : Float := No_Altitude;
      Speed    : Float := 0.0;
      Course   : Float := 0.0;
      Valid    : array (Data_Field_Kind) of Boolean := (others => False);
      Source   : Flight.Signal.Satellite_Kinds := Flight.Signal.Sat_Gps;

      --========================================================================
      procedure To_Latitude (Source : in out String) is
      begin

         -- Remove leading zeros
         ----------------------------
         for I in Source'Range loop

            if Source (I) = '0' then

               Source (I) := ' ';

            else

               exit;

            end if;

         end loop;

         Fix.Lat := Long_Float'Value (Source (Source'First..Source'First+1));

         Fix.Lat := Fix.Lat + Long_Float'Value (Source (Source'First+2..Source'Last)) / 60.0;

      end To_Latitude;
      --------------------------------------------------------------------------

      --========================================================================
      procedure To_Longitude (Source : in out String) is
      begin

         -- Remove leading zeros
         ----------------------------
         for I in Source'Range loop

            if Source (I) = '0' then

               Source (I) := ' ';

            else

               exit;

            end if;

         end loop;

         Fix.Lon := Long_Float'Value (Source (Source'First..Source'First+2));

         Fix.Lon := Fix.Lon + Long_Float'Value (Source (Source'First+3..Source'Last)) / 60.0;

      end To_Longitude;
      --------------------------------------------------------------------------

      --========================================================================
      procedure Load_Valid_Data is
      begin

         if Valid (Field_Position) then

            if Distance (Flight.Data.Position, Fix) > 1.0 then

               Utility.Log.Put_Message ("fix update at more than 1km: " & Image (Fix) & " from : " & Sentence);

            end if;

            Flight.Data.Position := Fix;

            Flight.Data.Ages   (Field_Position) := Cached_Time;

            Flight.Data.Origin (Field_Position) := Origin_External;

            if Valid (Field_Altitude) then

               Flight.Data.Altitude := Altitude;

               Flight.Data.Ages   (Field_Altitude) := Cached_Time;

               Flight.Data.Origin (Field_Altitude) := Origin_External;

            end if;

            if Valid (Field_Speed) then

               Flight.Data.Speed := Speed;

               Flight.Data.Ages   (Field_Speed) := Cached_Time;

               Flight.Data.Origin (Field_Speed) := Origin_External;

            end if;

            if Valid (Field_Course) then

               Flight.Data.Course := Course;

               Flight.Data.Ages   (Field_Course) := Cached_Time;

               Flight.Data.Origin (Field_Course) := Origin_External;

            end if;

         end if;

      end Load_Valid_Data;
      --------------------------------------------------------------------------

      First : Boolean := True;

   begin

      if Log_Messages then

         Utility.Log.Put_Message ("processing " & Sentence);

      end if;

      if Sentence'Length < 6 then

         Utility.Log.Put_Message ("the message is too short");

         return;

      end if;

      Reader.Load (Sentence);

      Message_Counter := Message_Counter + 1;

      declare
         Frame : String := Reader.Read_Next (',');
         Sat   : String (1..2) := Frame (2..3);
         Kind  : String (1..3) := Frame (4..6);
      begin

         if    Sat = "GP" then
            Source := Flight.Signal.Sat_Gps;
         elsif Sat = "GL" then
            Source := Flight.Signal.Sat_Glonass;
         elsif Sat = "GA" then
            Source := Flight.Signal.Sat_Galileo;
         elsif Sat = "GB" then
            Source := Flight.Signal.Sat_Beidou;
         end if;

         Valid := (others => False);

         if Kind = "GGA" then

            for Item in Nmea_Gpgga_Items loop

               declare
                  Value : String := Reader.Read_Next (',');
               begin

                  if Log_Messages then

                     Utility.Log.Put_Message (Nmea_Gpgga_Items'Image (Item) & "=" & Value);

                  end if;

                  if Value'Length = 0 then

                     goto Next_Gga_Item;

                  end if;

                  case Item is

                     when Gpgga_Utc_Time =>

                        Synchronize_Time (Value);

                     when Gpgga_Position_Fix =>

                        case Value (Value'First) is

                           when '1' | '2' | '6' =>

                              Valid (Field_Position) := True;

                           when others =>

                              null;

                        end case;

                     when Gpgga_Latitude =>

                        To_Latitude (Value);

                     when Gpgga_Ns_Indicator =>

                        if Value (Value'First) = 'S' then

                           Fix.Lat := -Fix.Lat;

                        end if;

                     when Gpgga_Longitude =>

                        To_Longitude (Value);

                     when Gpgga_Ew_Indicator =>

                        if Value (Value'First) = 'W' then

                           Fix.Lon := -Fix.Lon;

                        end if;

                     when Gpgga_Altitude =>

                        Altitude := Float'Value (Value);

                     when Gpgga_Altitude_Unit =>

                        if Value (Value'First) = 'M' then

                           Valid (Field_Altitude) := True;

                        end if;

                     when others =>
                        null;

                  end case;

                  <<Next_Gga_Item>>

               exception
                  when E : others =>
                     Utility.Log.Put_Message (E, "error while decoding NMEA message " & Sentence);
               end;

            end loop;

            Load_Valid_Data;

         elsif Kind = "RMC" then

            -- Recommended minimum
            ---------------------------------

            for Item in Nmea_Gnrmc_Items loop

               declare
                  Value : String := Reader.Read_Next (',');
               begin

                  if Log_Messages then

                     Utility.Log.Put_Message (Nmea_Gnrmc_Items'Image (Item) & "=" & Value);

                  end if;

                  if Value'Length = 0 then

                     goto Next_Rmc_Item;

                  end if;

                  case Item is

                     when Gnrmc_Utc_Time =>

                        Synchronize_Time (Value);

                     when Gnrmc_Status =>

                        if Value (Value'First) = 'A' then

                           Valid (Field_Position) := True;

                        end if;

                     when Gnrmc_Latitude =>

                        To_Latitude (Value);

                     when Gnrmc_Ns_Indicator =>

                        if Value (Value'First) = 'S' then

                           Fix.Lat := -Fix.Lat;

                        end if;

                     when Gnrmc_Longitude =>

                        To_Longitude (Value);

                     when Gnrmc_Ew_Indicator =>

                        if Value (Value'First) = 'W' then

                           Fix.Lon := -Fix.Lon;

                        end if;

                     when Gnrmc_Ground_Speed =>

                        Speed := Convert (Float'Value (Value), Unit_Knot, Unit_Meter_Second);

                        Valid (Field_Speed) := True;

                     when Gnrmc_Ground_Course =>

                        Course := Float'Value (Value);

                        Valid (Field_Course) := True;

                     when others =>
                        null;

                  end case;

                  <<Next_Rmc_Item>>

               exception
                  when E : others =>
                     Utility.Log.Put_Message (E, "error while decoding NMEA sentence " & Sentence);
               end;

            end loop;

            Load_Valid_Data;

         elsif Kind = "GSV" then

            -- Satellite status
            ---------------------------------

            declare
               use Flight.Signal;
               Index : Natural := Satellites (Source)'First; -- satellite index
               Total : Natural := 0; -- total number of GSV messages containing the info
               Count : Natural := 0; -- current message number
               Id    : String (1..2);
            begin

               for Item in Nmea_Gpgsv_Items loop

                  declare
                     Value : String := Reader.Read_Next (',');
                  begin

                     if Value'Length = 0 then

                        goto Next_Gpgsv_Item;

                     end if;

                     case Item is

                        when Gpgsv_Message_Count =>

                           Total := Natural'Value (Value);

                        when Gpgsv_Message_Number =>

                           Count := Natural'Value (Value);

                           if Count = 1 then

                              for S in Satellites (Source)'Range loop
                                 Satellites (Source) (S).Refreshed := False;
                              end loop;

                              -- Clear data if no info is received during more
                              -- than 5 seconds
                              --------------------------------------------------
                              if Signal_Timer = null then
                                 Signal_Timer := Timing.Events.Register_Timer (5.0, Clear_Satellites'Access);
                              else
                                 Signal_Timer.Restart;
                              end if;

                           end if;

                        when Gpgsv_Satellites_Count =>

                           Number_Of_Satellites (Source) := Natural'Value (Value);

                        when Gpgsv_Satellite_1_Id |
                             Gpgsv_Satellite_2_Id |
                             Gpgsv_Satellite_3_Id |
                             Gpgsv_Satellite_4_Id =>

                           Utility.Strings.Override (Id, Value);

                           -- Search satellite entry or add it to the list
                           -----------------------------------------------------
                           for S in Satellites (Source)'Range loop
                              if
                                Satellites (Source) (S).Id = Id or
                                Satellites (Source) (S).Id = No_Satellite.Id
                              then
                                 Index := S;
                                 exit;
                              end if;
                           end loop;

                           Satellites (Source) (Index).Id        := Id;
                           Satellites (Source) (Index).Refreshed := True;
                           Satellites (Source) (Index).Valid     := True;

                        when Gpgsv_Satellite_1_Elevation |
                             Gpgsv_Satellite_2_Elevation |
                             Gpgsv_Satellite_3_Elevation |
                             Gpgsv_Satellite_4_Elevation =>

                           Satellites (Source) (Index).Elevation := Natural'Value (Value);

                        when Gpgsv_Satellite_1_Azimuth |
                             Gpgsv_Satellite_2_Azimuth |
                             Gpgsv_Satellite_3_Azimuth |
                             Gpgsv_Satellite_4_Azimuth =>

                           Satellites (Source) (Index).Azimuth := Natural'Value (Value);

                        when Gpgsv_Satellite_1_Strength |
                             Gpgsv_Satellite_2_Strength |
                             Gpgsv_Satellite_3_Strength |
                             Gpgsv_Satellite_4_Strength =>

                           Satellites (Source) (Index).Strength := Natural'Value (Value);

                        when others =>

                           null;

                     end case;

                     <<Next_Gpgsv_Item>>

                  end;

               end loop;

               -- Invalidate not refreshed satellites at the end of the last
               -- message
               -----------------------------------------------------------------
               if Count = Total then

                  for Satellite of Satellites (Source) loop

                     if not Satellite.Refreshed then

                        Satellite := No_Satellite;

                     end if;

                  end loop;

               end if;

            end;

            --
            if Log_Messages then

               declare
                  use Flight.Signal;
               begin

                  Utility.Log.Put_Message ("current satellites for " & Satellite_Kinds'Image (Source));

                  for Sat of Satellites (Source) loop

                     if Sat.Valid then

                        Utility.Log.Put_Message (Sat.Id & Natural'Image (Sat.Strength) & Natural'Image (Sat.Azimuth) & Natural'Image (Sat.Elevation));

                     end if;

                  end loop;

               end;

            end if;

         elsif Frame = "$PFLAA" then

            -- Flarm extrension
            ---------------------------------

            declare
               use Flight.Traffic;
               Traffic_Item : Traffic_Record := No_Traffic_Record;
               X, Y, Z      : Long_Float     := 0.0;
            begin

               for Item in Nmea_Pflaa_Items loop

                  declare
                     Value : String := Reader.Read_Next (',');
                  begin

                     if Value'Length = 0 then

                        goto Next_Pflaa_Item;

                     end if;

                     case Item is

                        when Pflaa_Id =>

                           Utility.Strings.Override (Traffic_Item.Id, Value);

                        when Pflaa_Relative_East =>

                           X := Long_Float'Value (Value) / 1000.0;

                        when Pflaa_Relative_North =>

                           Y := Long_Float'Value (Value) / 1000.0;

                        when Pflaa_Relative_Vertical =>

                           Traffic_Item.Vertical := Float'Value (Value);

                        when Pflaa_Ground_Speed =>

                           Traffic_Item.Speed := Float'Value (Value);

                        when Pflaa_Climb_Rate =>

                           Traffic_Item.Climb_Rate := Float'Value (Value);

                        when Pflaa_Track =>

                           Traffic_Item.Course := Float'Value (Value);

                        when others =>

                           null;

                     end case;

                  end;

                  <<Next_Pflaa_Item>>

               end loop;

               Traffic_Item.Position := Position (Flight.Data.Position, New_Vector2_Record (X, Y));

               Keep_Track (Traffic_Item);

            end;

         end if;

      end;

      -- Check and augment data
      -----------------------------------------------
      Check_Data;

   exception
      when E : others =>

         Error_Found := True;
         Last_Error  := Cached_Time;

         -- Clear alert after 5 minutes
         --------------------------------------------------
         if Error_Timer = null then
            Error_Timer := Timing.Events.Register_Timer (300.0, Clear_Error'Access);
         else
            Error_Timer.Restart;
         end if;

         Utility.Log.Put_Message (E, "error while parsing NMEA message {" & Sentence & "}");

   end Parse_Nmea_Sentence;
   -----------------------------------------------------------------------------





   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Incomplete_Sentence : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Missing_Sentence    : Dynamic_String;

   --===========================================================================
   -- NOTE: NMEA parser can cope with incomplete messages and multiple entries
   --===========================================================================
   procedure Parse_Nmea_Message (Message : String) is

      F, L : Integer := Message'First;

      Open_Declaration : Boolean := False;

   begin

      if Log_Messages then

         Utility.Log.Put_Message ("received " & Message);

      end if;

      -- Check if the last message was incomplete and can be completed now
      --------------------------------------------------------------------------

      if Incomplete_Sentence and then Message (F) /= '$' then

         -- This is the reminder of a previous incomplete message

         for R in Message'Range loop

            -- Go to the first checksum character and complete missing message

            if Message (R) = '*' then

               L := R - 1;

               if L >= F then

                  if Log_Messages then

                     Utility.Log.Put_Message ("message completed...");

                  end if;

                  Missing_Sentence := Missing_Sentence & Message (F..L);

                  Parse_Nmea_Sentence (-Missing_Sentence);

                  Missing_Sentence := Empty_String;

               end if;

               F := R + 1;

               exit;

            end if;

         end loop;

         Incomplete_Sentence := False;

      end if;

      -- Processes the data messages
      --------------------------------------------------------------------------

      for I in F..Message'Last loop

         -- Start symbol
         if Message (I) = '$' then

            F := I;
            Open_Declaration := True;

         -- Checksum symbol
         elsif Message (I) = '*' then

            L := I - 1;
            Open_Declaration := False;

         end if;

         if L > F then

            Parse_Nmea_Sentence (Message (F..L));

            L := F;

         end if;

      end loop;

      -- Check if there is a missing part
      --------------------------------------------------------------------------

      if Open_Declaration then

         for I in F..Message'Last loop

            if
              I = Message'Last or else
              Message (I) = Ascii.CR or else
              Message (I) = Ascii.LF
            then

               Missing_Sentence    := +Message (F..I);

               Incomplete_Sentence := True;

               if Log_Messages then

                  Utility.Log.Put_Message ("incomplete message detected " & Message (F..I));

               end if;

            end if;

         end loop;

      end if;

   end Parse_Nmea_Message;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Error_Detected return Boolean is
   begin

      return Error_Found;

   end Error_Detected;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Recent_Error return Boolean is
   begin

      return (Cached_Time - Last_Error) < 5.0;

   end Recent_Error;
   -----------------------------------------------------------------------------

end Flight.Parsing;
--------------------------------------------------------------------------------
