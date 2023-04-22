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

--//////////////////////////////////////////////////////////////////////////////
-- This packages is where the raw flight data is collected.
-- There are three data sources:
--
-- FILE   -> used to replay recorded data
-- UDP    -> used either to connect a flight simulator or a middleware
-- SERIAL -> used to link a data acquisition serial port (like SoftRF USB dongles or FLARM devices).
--
-- The source is set using environmental variables at startup:
-- FILE   -> FILE_STREAM=<name of file located in the replay/ directory>
-- UDP    -> UDP_STREAM=<port number>
-- SERIAL -> SERIAL_STREAM=<serial port name (default is "/dev/ttyACM0")>
--
-- The data can be on two formats:
-- GNAV       -> comma separated set of named variables, like "X=10.0,Y=5.2,A=200,..."
--               This format is very suitable for linking a flight simulator.
-- NMEA/FLARM -> NMEA and FLARM standard
--
-- The protocol must be specified after the source in the startup variables:
-- PROTOCOL=<G-NAV,NMEA/FLARM>
--//////////////////////////////////////////////////////////////////////////////
package Flight.Stream is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The different kinds of data aquisition protocols
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stream_Protocol_Kind is (Protocol_Gnav,
                                 Protocol_Nmea_Flarm);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The different kinds of data sources
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stream_Source_Kind is (Stream_Source_None,
                               Stream_Source_File,
                               Stream_Source_Udp,
                               Stream_Source_Serial,
                               Stream_Source_Interface);

   --===========================================================================
   -- Opens the data stream as specified on the command line arguments
   --===========================================================================
   procedure Init;

   --===========================================================================
   -- Closes all data streams
   --===========================================================================
   procedure Finalize;

   --===========================================================================
   -- Indicates if the data streaming is running
   --===========================================================================
   function Is_Active return Boolean;

   --===========================================================================
   -- Returns the data rate (average number of packets received per minute)
   --===========================================================================
   function Get_Rate return Natural;

   --===========================================================================
   -- Returns the active stream kind
   --===========================================================================
   function Get_Source_Kind return Stream_Source_Kind;

   --===========================================================================
   -- Returns the active protocol kind
   --===========================================================================
   function Get_Protocol_Kind return Stream_Protocol_Kind;

   --===========================================================================
   -- Returns the string representation for a given protocol kind
   --===========================================================================
   function Get_Protocol_Key (Value : Stream_Protocol_Kind) return String;

   --===========================================================================
   -- Indicates if the stream is a replay
   --===========================================================================
   function Is_Replay return Boolean;

   --===========================================================================
   -- Indicates if there is an active recording
   --===========================================================================
   function Recording return Boolean;

   --===========================================================================
   -- When the interface streaming is used, this method accepts external data
   --===========================================================================
   procedure Set_Data (Lat, Lon : Long_Float; Alt, Spd, Brg : Float);

private

   --===========================================================================
   -- Collects the pending data on any of the streams
   --===========================================================================
   procedure Collect_Data;

end Flight.Stream;
--------------------------------------------------------------------------------
