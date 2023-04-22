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
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams;
use  Ada.Streams;
with Ada.Text_IO;
use  Ada.Text_IO;
with Gnat.Sockets;
use  Gnat.Sockets;
-- Gnav
with Flight.Parsing;
use  Flight.Parsing;
with Flight.Traffic;
with Maps.Loader;
with Timing.Events;
with Utility.Log;
with Utility.Serial;
use  Utility.Serial;
with Utility.Streams;
use  Utility.Streams;
with Utility.Strings;
use  Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Stream is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The active simulation
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Stream_Source : Stream_Source_Kind := Stream_Source_None;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The active simulation protocol
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Stream_Protocol : Stream_Protocol_Kind := Protocol_Nmea_Flarm;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the incoming messges must be recorded on the replay file
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Record_Messages : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The server socket configuration
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Socket_Id   : Socket_Type    := No_Socket;
   Address     : Sock_Addr_Type := No_Sock_Addr;
   From        : Sock_Addr_Type := No_Sock_Addr;
   Socket_Port : Dynamic_String := Empty_String;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The serial port and reconnection timer
   -- NOTE: the serial reconnection is tried every 4 seconds
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Serial_Id        : Serial_Port;
   Serial_Name      : Dynamic_String := +"/dev/ttyS0"; -- +"/dev/ttyACM0";
   Serial_Reconnect : access Timing.Events.Timer_Record;
   Serial_Ublox     : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- 2KiB static storage for the data stream (used for serial or UDP
   -- acquisition)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Data   : Stream_Element_Array (1..2000);
   Buffer : aliased Stream_Buffer_Type;
   Store  : not null access Stream_Buffer_Type := Buffer'Access;
   Last   : Stream_Element_Offset;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The replay file
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Replay_File      : Ada.Text_IO.File_Type;
   Replay_File_Name : Dynamic_String := Empty_String;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates the overal status of the data streaming
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Stream_Active : Boolean := False;
   Data_Rate     : Natural := 0;
   Replay_Active : Boolean := False;




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Source_Kind return Stream_Source_Kind is
   begin

      return Stream_Source;

   end Get_Source_Kind;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Protocol_Kind return Stream_Protocol_Kind is
   begin

      return Stream_Protocol;

   end Get_Protocol_Kind;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Is_Replay return Boolean is
   begin

      return Stream_Source = Stream_Source_File;

   end Is_Replay;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Protocol_Key (Value : Stream_Protocol_Kind) return String is
   begin

      case Value is

         when Protocol_Nmea_Flarm =>

            return "NMEA/FLARM";

         when Protocol_Gnav =>

            return "G-NAV";

      end case;

   end Get_Protocol_Key;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Calculate_Rate is
   begin

      Data_Rate := Natural (Float (Message_Counter) / 5.0);

      Message_Counter := 0;

   end Calculate_Rate;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Rate return Natural is
   begin

      return Data_Rate;

   end Get_Rate;
   -----------------------------------------------------------------------------



   --========================================================================
   function New_Replay_File_Name return String is
   begin

      return Utility.Base_Directory & "replay/" & Replace (Ada.Calendar.Formatting.Image (Flight.Get_Utc_Time), ' ', '_') & ".dat";

   end New_Replay_File_Name;
   --------------------------------------------------------------------------




   --===========================================================================
   -- Flushes the recording file
   --===========================================================================
   procedure Flush_Recording is
   begin

      if Is_Open (Replay_File) then

         Flush (Replay_File);

      elsif Time_Synchronized and then Flight.Data.Is_Recent (Field_Position) then

         -- Start replay after time is synchronized and there is a fix
         ---------------------------------------------------------------
         Utility.Log.Put_Message ("starting recording");

         Create (Replay_File, Out_File, New_Replay_File_Name);

         Put_Line (Replay_File, "#MAP=" & Maps.Get_Dataset_Name);

         Put_Line (Replay_File, "#PROTOCOL=" & Get_Protocol_Key (Stream_Protocol));

         Put_Line (Replay_File, "#");

         Record_Messages := True;

      end if;

   end Flush_Recording;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Setups the replay file for replay data acqusition
   --===========================================================================
   procedure Setup_Replay is

      Reader    : String_Buffer (100);
      File_Name : String := "replay/" & (-Replay_File_Name);

   begin

      Stream_Active   := False;

      Utility.Log.Put_Message ("setting up replay data acqusition from file " & File_Name);

      Stream_Source   := Stream_Source_File;

      Record_Messages := False;

      if Ada.Directories.Exists (File_Name) then

         Open (Replay_File, In_File, File_Name);

         Utility.Log.Put_Message ("reading replay data");

         -- Load the replay configuration
         --------------------------------------------------------------
         loop

            Reader.Load (Get_Line (Replay_File));

            exit when Reader.Current /= '#';

            declare
               Key   : String := Reader.Read_Next ('=');
               Value : String := Reader.Read_Next ('=');
            begin

               if Key = "#MAP" then

                  Maps.Loader.Load_Dataset (Value);

               elsif Key = "#PROTOCOL" then

                  for P in Stream_Protocol_Kind loop

                     if Get_Protocol_Key (P) = Value then

                        Stream_Protocol := P;

                        exit;

                     end if;

                  end loop;

               end if;

            end;

         end loop;

         Reset (Replay_File);

         Timing.Events.Register_Timer (Timer    => Timing.Time_Delta,
                                       Callback => Collect_Data'Access);

         Stream_Active := True;

      else

         Utility.Log.Put_Message ("warning: the provided replay file dos not exist");

      end if;

   exception
      when E: others =>

         Close (Replay_File);

         Utility.Log.Put_Message (E, "error while setting up replay");

   end Setup_Replay;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Setup the serial port for data acquisition
   --===========================================================================
   procedure Setup_Socket is
   begin

      Stream_Active := False;

      Utility.Log.Put_Message ("setting up server socket for data aquisition");

      Stream_Source := Stream_Source_Udp;

      Address.Addr  := Any_Inet_Addr; -- Addresses (Get_Host_By_Name ("localhost"), 1);

      if Length (Socket_Port) = 0 then
         Address.Port := 4000;
      else
         Address.Port := Port_Type'Value (-Socket_Port);
      end if;

      Utility.Log.Put_Message ("address: " & Image (Address.Addr));
      Utility.Log.Put_Message ("port   :"  & Port_Type'Image (Address.Port));

      Create_Socket     (Socket_Id, Family_Inet, Socket_Datagram);
      Set_Socket_Option (Socket_Id, Socket_Level, (Reuse_Address, True));
      Set_Socket_Option (Socket_Id, Socket_Level, (Receive_Timeout, Timeout => 0.001));
      Bind_Socket       (Socket_Id, Address);

      Stream_Active := True;

   end Setup_Socket;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Connects to the serial port for data acquisition
   --===========================================================================
   procedure Setup_Serial is
   begin

      Stream_Active := False;

      Utility.Log.Put_Message ("setting up serial port for data aquisition");

      Stream_Source := Stream_Source_Serial;

      Utility.Log.Put_Message ("target port: " & (-Serial_Name));

      begin

         Open (Port => Serial_Id,
               Name => Port_Name (-Serial_Name));

      exception
         when E : Serial_Error =>
            Utility.Log.Put_Message (E, "could not connect to serial device");
            return;
      end;

      Utility.Log.Put_Message ("serial open");

      Set (Port      => Serial_Id,
           Rate      => B9600,
           Bits      => CS8,
           Stop_Bits => One,
           Parity    => None,
           Block     => True,
           Local     => True,
           Flow      => None,
           Timeout   => 10.0);

      Utility.Log.Put_Message ("serial configured");

      Serial_Reconnect.Stop;

      Stream_Active := True;

      if Serial_Ublox then

         -- Configure U-BLOX for airborne mode < 1g
         -- (see UBX-CFG-NAV5 message [16#06#,16#24#])
         ----------------------------------------------------

         Write (Serial_Id,
                (16#B5#,16#62#,16#06#,16#24#,16#24#,16#00#,
                 16#01#,16#00#,16#06#,16#00#,16#00#,16#00#,16#00#,16#00#,
                 16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,
                 16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,
                 16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,
                 16#00#,16#00#,16#00#,16#00#,16#55#,16#B4#));

         Utility.Log.Put_Message ("UBLOX airborne configuration sent");

      end if;

   exception
      when E : Serial_Error =>
         Utility.Log.Put_Message (E, "error while connecting to serial device");
         return;
   end Setup_Serial;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Setups the stream buffer and data recording file
   --===========================================================================
   procedure Setup_Stream is
   begin

      Utility.Log.Put_Message ("setting up stream and recording...");

      -- Setup the buffer and data collection
      -----------------------------------------------------------------
      Store.Buffer := Data'Unrestricted_Access;

      Timing.Events.Register_Timer (Timer    => Timing.Time_Delta,
                                    Callback => Collect_Data'Access);

      -- Load the default map
      -----------------------------------------------------------------
      Maps.Loader.Load_Default_Dataset;

      -- Setup the replay file for data recording
      -----------------------------------------------------------------
      Timing.Events.Register_Timer (5.0, Flush_Recording'Access);

   end Setup_Stream;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Setups the stream buffer and data recording file
   --===========================================================================
   procedure Setup_Interface is
   begin

      Utility.Log.Put_Message ("setting up interface and recording...");

      Stream_Protocol := Protocol_Gnav; -- (for recording only)

      -- Load the default map
      -----------------------------------------------------------------
      Maps.Loader.Load_Default_Dataset;

      -- Setup the replay file for data recording
      -----------------------------------------------------------------
      Timing.Events.Register_Timer (5.0, Flush_Recording'Access);

   end Setup_Interface;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Init is

      use Ada.Text_IO;

      Reader          : Utility.Strings.String_Buffer (100);
      Selected_Source : Stream_Source_Kind := Stream_Source_None;
      File_Name       : String := Utility.Base_Directory & "setup.dat";
      File            : File_Type;

   begin

      -- Load the selected stream source
      --------------------------------------------------------------------------
      if Ada.Directories.Exists (File_Name) then

         Open (File, In_File, File_Name);

         while not End_Of_File (File) loop

            Reader.Load (Get_Line (File));

            if Reader.Current /= '#' then

               declare
                  Key   : String := Reader.Read_Next ('=');
                  Value : String := Reader.Read_Next ('=');
               begin

                  if Key = "FILE_STREAM" then

                     Replay_File_Name := +Value;

                     Selected_Source := Stream_Source_File;

                  elsif Key = "UDP_STREAM" then

                     Socket_Port := +Value;

                     Selected_Source := Stream_Source_Udp;

                  elsif Key = "SERIAL_STREAM" then

                     Serial_Reconnect := Timing.Events.Register_Timer (4.0, Setup_Serial'Access);

                     if Value'Length > 0 then
                        Serial_Name := +Value;
                     end if;

                     Selected_Source := Stream_Source_Serial;

                  elsif Key = "PROTOCOL" then

                     for P in Stream_Protocol_Kind loop

                        if Get_Protocol_Key (P) = Value then

                           Stream_Protocol := P;

                           exit;

                        end if;

                     end loop;

                  elsif Key = "UBLOX_AIRBORNE" then

                     Serial_Ublox := True;

                  end if;

               end;

            end if;

         end loop;

         case Selected_Source is

            when Stream_Source_File =>

               --NOTE: this will always override the protocol kind

               Setup_Replay;

            when Stream_Source_Udp =>

               Setup_Socket;

               Setup_Stream;

            when Stream_Source_Serial =>

               Setup_Serial;

               Setup_Stream;

            when Stream_Source_Interface =>

               Setup_Interface;

            when Stream_Source_None =>

               null;

         end case;

         Close (File);

      end if;

      -- Log resulting configuration
      --------------------------------------------------------------------------
      Utility.Log.Put_Message ("source set to "   & Stream_Source_Kind'Image   (Stream_Source));
      Utility.Log.Put_Message ("protocol set to " & Stream_Protocol_Kind'Image (Stream_Protocol));

      -- Start the traffic stack managment
      --------------------------------------------------------------------------
      Flight.Traffic.Init;

      -- Setup a timer to obtain the average data rate
      --------------------------------------------------------------------------
      Timing.Events.Register_Timer (5.0, Calculate_Rate'Access);

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Finalize is
   begin

      Utility.Log.Put_Message ("closing the data streams");

      if Is_Open (Replay_File) then

         if Record_Messages then

            Put_Line (Replay_File, "#END");

         end if;

         Close (Replay_File);

      end if;

      case Stream_Source is

         when Stream_Source_File =>

            null;

         when Stream_Source_Udp =>

            if Socket_Id /= No_Socket then

               Close_Socket (Socket_Id);

            end if;

         when Stream_Source_Serial =>

            Close (Serial_Id);

         when Stream_Source_Interface =>

            null;

         when Stream_Source_None =>

            null;

      end case;

   end Finalize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Is_Active return Boolean is
   begin

      return Stream_Active;

   end Is_Active;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Replay_Mode return Boolean is
   begin

      return Stream_Source = Stream_Source_File and Is_Open (Replay_File);

   end Replay_Mode;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Recording return Boolean is
   begin

      return Record_Messages;

   end Recording;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Decodes a generic data message
   --===========================================================================
   procedure Process_Message (Messages : String) is
   begin

      Flight.Data.Timestamp := Cached_Time;

      case Stream_Protocol is

         when Protocol_Gnav =>

            Parse_Gnav_Message (Messages);

         when Protocol_Nmea_Flarm =>

            Parse_Nmea_Message (Messages);

      end case;

      if Record_Messages then

         Put_Line (Replay_File, Messages);

      end if;

   end Process_Message; pragma Inline (Process_Message);
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Proceses the next valid line on the replay file
   --===========================================================================
   function Read_Replay_Step return Boolean is

      Line : String := Get_Line (Replay_File);

   begin

      if End_Of_File (Replay_File) then

         -- Reset the flight history and stop caching until receiving new data
         -----------------------------------------------------------------------

         Reset (Replay_File);

         Flight.Clear_History;

         Replay_Active := False;

         On_Replay_Reset.Trigger;

         Utility.Log.Put_Message ("replay reset");

      end if;

      if Line'Length = 0 then

         return True;

      else

         if Line (1) = '#' then

            return False;

         else

            Process_Message (Line);

            Replay_Active := True;

            return True;

         end if;

      end if;

   end Read_Replay_Step;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Collect_Data is

      use Utility.Strings;

   begin

      Cached_Time := Clock;

      for F in Data_Field_Kind loop

         Flight.Data.Origin (F) := Origin_None;

      end loop;

      case Stream_Source is

         when Stream_Source_File =>

            -- Case A: Replay data from file
            --------------------------------------------------------------------
            if Is_Open (Replay_File) then

               Utility.Log.Put_Message ("reading step");

               while Read_Replay_Step loop

                  null;

               end loop;

            end if;

            -- Only cache when the replay is active
            ---------------------------------------
            if Replay_Active then

               Flight.Cache_Data;

            end if;

         when Stream_Source_Udp =>

            -- Case B: Online socket data (from a simulator or middleware)
            --------------------------------------------------------------------

            loop

               Last := 0;

               Store.Move_Cursor (Data'First);

               begin
                  Receive_Socket (Socket_Id, Data, Last, From);
               exception
                  when E : Socket_Error =>
                     null;
                  when E : others =>
                     Utility.Log.Put_Message (E, "while reading from socket");
               end;

               if Last in Data'Range then

                  declare
                     Messages : String (1.. Positive (Last));
                  begin
                     String'Read (Store, Messages);
                     Process_Message (Messages);
                  end;

               else

                  Utility.Log.Put_Message ("warning: socket input is too large");

                  exit;

               end if;

            end loop;

            -- Load the data on the cache
            -------------------------------------
            Flight.Cache_Data;

         when Stream_Source_Serial =>

            -- Case C: serial streaming
            --------------------------------------------------------------------

            Last := 0;

            Store.Move_Cursor (Data'First);

            begin
               Read (Serial_Id, Data, Last);
            exception
               when E : Constraint_Error =>
                  Utility.Log.Put_Message (E, "error while reading from serial");
               when E : others =>
                  Serial_Reconnect.Resume;
            end;

            if Last in Data'Range then

               declare
                  Messages : String (1.. Positive (Last));
               begin
                  String'Read (Store, Messages);
                  Process_Message (Messages);
               end;

            end if;

            -- Load the data on the cache
            -------------------------------------
            Flight.Cache_Data;

         when Stream_Source_Interface =>

            null;

         when Stream_Source_None =>

            null;

      end case;

      -- Indicate end of recording step
      -------------------------------------
      if Record_Messages then

         Put_Line (Replay_File, "#");

      end if;

   exception

      when E : others =>

         Utility.Log.Put_Message ("error while aquiring data: "& Ada.Exceptions.Exception_Message (E));

   end Collect_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Data (Lat, Lon : Long_Float; Alt, Spd, Brg : Float) is
   begin

      if Stream_Source = Stream_Source_Interface then

         Cached_Time := Clock;

         for F in Data_Field_Kind loop

            Flight.Data.Origin (F) := Origin_None;

         end loop;

         Flight.Data.Timestamp := Cached_Time;

         Flight.Data.Origin (Field_Position) := Origin_External;

         Flight.Data.Ages   (Field_Position) := Cached_Time;

         Flight.Data.Position.Lat := Lat;

         Flight.Data.Position.Lon := Lon;

         --

         Flight.Data.Origin (Field_Altitude) := Origin_External;

         Flight.Data.Ages   (Field_Altitude) := Cached_Time;

         Flight.Data.Altitude := Alt;

         --

         Flight.Data.Origin (Field_Speed) := Origin_External;

         Flight.Data.Ages   (Field_Speed) := Cached_Time;

         Flight.Data.Speed := Spd;

         --

         Flight.Data.Origin (Field_Course) := Origin_External;

         Flight.Data.Ages   (Field_Course) := Cached_Time;

         Flight.Data.Course := Brg;

         -- Indicate end of recording step
         -------------------------------------
         if Record_Messages then

            -- TODO: write G-NAV message

            Put_Line (Replay_File, "#");

         end if;

      end if;

   end Set_Data;
   -----------------------------------------------------------------------------

end Flight.Stream;
--------------------------------------------------------------------------------
