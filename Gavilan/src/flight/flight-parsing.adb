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
with Flight.Traffic;
with Maps.Terrain;
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
      Flight.Compute_Wind;

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

                  Flight.Data.Speed := Float'Value (Val);

                  Flight.Data.Ages   (Field_Speed) := Cached_Time;

                  Flight.Data.Origin (Field_Speed) := Origin_External;

               when 'H' =>

                  Flight.Data.Heading := Float'Value (Val);

               when 'C' =>

                  Flight.Data.Course := Float'Value (Val);

                  Flight.Data.Ages   (Field_Course) := Cached_Time;

                  Flight.Data.Origin (Field_Course) := Origin_External;

               when 'W' =>

                  if Wind_Source = Wind_Source_Stream then

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
                            Gpgga_Checksum,

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
                            Gnrmc_Checksum,

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
                            Pflaa_Aircraft_Type);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Nmea_Gpgga_Items is Nmea_Data_Items range Gpgga_Utc_Time..Gpgga_Checksum;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Nmea_Gnrmc_Items is Nmea_Data_Items range Gnrmc_Utc_Time..Gnrmc_Checksum;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Nmea_Pflaa_Items is Nmea_Data_Items range Pflaa_Alarm_Level..Pflaa_Aircraft_Type;

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Parse_Nmea_Message (Message : String) is

      Reader   : String_Buffer (Message'Size);
      Fix      : Position_Record := No_Position_Record;
      Altitude : Float := No_Altitude;
      Speed    : Float := 0.0;
      Course   : Float := 0.0;
      Valid    : array (Data_Field_Kind) of Boolean := (others => False);

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

   begin

      if Log_Messages then

         Utility.Log.Put_Message ("received " & Message);

      end if;

      Reader.Load (Message);

      loop

         -- Move to the start of the next element
         -----------------------------------------------------------------------
         Reader.Move_To_Next ('$');

         exit when Reader.End_Of_Stream;

         Message_Counter := Message_Counter + 1;

         declare
            Frame : String := Reader.Read_Next (',');
         begin

            Valid := (others => False);

            if Frame = "$GPGGA" then

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
                        Utility.Log.Put_Message (E, "error while decoding NMEA message " & Message);
                  end;

               end loop;

               Load_Valid_Data;

            elsif Frame = "$GNRMC" then

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
                        Utility.Log.Put_Message (E, "error while decoding NMEA message " & Message);
                  end;

               end loop;

               Load_Valid_Data;

            elsif Frame = "$PFLAA" then

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

      end loop;

      -- Check and augment data
      -----------------------------------------------
      Check_Data;

   end Parse_Nmea_Message;
   -----------------------------------------------------------------------------

end Flight.Parsing;
--------------------------------------------------------------------------------
