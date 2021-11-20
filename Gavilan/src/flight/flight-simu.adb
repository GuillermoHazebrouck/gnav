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
with Ada.Exceptions;
with Ada.Streams;
use  Ada.Streams;
with Ada.Text_IO;
use  Ada.Text_IO;
with Gnat.Sockets;
use  Gnat.Sockets;
-- Gnav
with Maps.Loader;
with Maps.Terrain;
with Timing.Events;
with Utility.Streams;
use  Utility.Streams;
with Utility.Strings;
use  Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Simu is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The active simulation
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Simu_Kind : Simu_Kinds := Simu_Socket;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the incoming messges must be prompted
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Log_Messages : Boolean := True;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The server socket
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Server  : Socket_Type;
   Address : Sock_Addr_Type;
   From    : Sock_Addr_Type;
   Data    : Stream_Element_Array (1..512);
   Buffer  : aliased Stream_Buffer_Type;
   Store   : not null access Stream_Buffer_Type := Buffer'Access;
   Last    : Stream_Element_Offset;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The id of the simu file
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   File_Id : File_Type;

   --===========================================================================
   --
   --===========================================================================
   procedure Read_Step;

   --===========================================================================
   --
   --===========================================================================
   procedure Init is

      Command_Data : Utility.Strings.String_Buffer (100);

      Reader : String_Buffer (100);

      --========================================================================
      -- Registers the data aquisition timer
      --========================================================================
      procedure Register_Timer is
      begin

         Timing.Events.Register_Timer (Timer    => Timing.Time_Delta,
                                       Callback => Read_Step'Access);

      end Register_Timer;
      --------------------------------------------------------------------------

   begin

      for I in 1..Ada.Command_Line.Argument_Count loop

         Command_Data.Load (Ada.Command_Line.Argument (I));

         declare
            Key   : String := Command_Data.Read_Next ('=');
            Value : String := Command_Data.Read_Next ('=');
         begin

            if Key = "REPLAY" then

               Simu_Kind := Simu_File;

               if Ada.Directories.Exists ("replay/" & Value) then

                  Open (File_Id, In_File, "replay/" & Value);

                  Ada.Text_IO.Put_Line ("reading replay data");

                  -- Load the metadata
                  --------------------------------------------------------------

                  Reader.Load (Ada.Text_IO.Get_Line (File_Id));

                  while Reader.Current = '#' loop

                     if Reader.Read_Next ('=') = "#MAP" then

                        Maps.Loader.Load_Dataset (+Reader.Read_Next ('='));

                     end if;

                  end loop;

                  Reset (File_Id);

                  Register_Timer;

                  return;

               else

                  Ada.Text_IO.Put_Line ("WARNING: the provided simulation file dos not exist");

               end if;

            elsif Key = "PORT" then

               Simu_Kind := Simu_Socket;

               Ada.Text_IO.Put_Line ("setting up server socket for data aquisition");

               Address.Addr := Any_Inet_Addr; -- Addresses (Get_Host_By_Name ("localhost"), 1);
               Address.Port := Port_Type'Value (Value);

               Ada.Text_IO.Put_Line ("address: " & Image (Address.Addr));
               Ada.Text_IO.Put_Line ("port   :" & Port_Type'Image (Address.Port));

               Create_Socket     (Server, Family_Inet, Socket_Datagram);
               Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
               Set_Socket_Option (Server, Socket_Level, (Receive_Timeout, Timeout => 0.001));
               Bind_Socket       (Server, Address);

               Store.Buffer := Data'Unrestricted_Access;

               Register_Timer;

               return;

            end if;

         end;

      end loop;

   end Init;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Is_Active return Boolean is
   begin

      return Ada.Text_IO.Is_Open (File_Id);

   end Is_Active;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Parse_Message (Message : String) is

      use Maps.Terrain;

      Previous_Position : Position_Record := Flight.Get_Previous.Position;

      Reader : String_Buffer (Message'Size);

      Course_Set : Boolean   := False;
      Wind_Set   : Boolean   := False;
      Airspeed_Set : Boolean := False;

   begin

      if Log_Messages then

         Ada.Text_IO.Put_Line ("received " & Message);

      end if;

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

               when 'Y' =>
                  Flight.Data.Position.Lat := Long_Float'Value (Val);

               when 'A' =>
                  Flight.Data.Altitude     := Float'Value (Val);

               when 'S' =>
                  Flight.Data.Speed        := Float'Value (Val);

               when 'H' =>
                  Flight.Data.Heading      := Float'Value (Val);

               when 'C' =>
                  Flight.Data.Course       := Float'Value (Val);
                  Course_Set := True;

               when 'E' =>
                  Flight.Data.Wind.Set_X   (Long_Float'Value (Val));
                  Wind_Set := True;

               when 'N' =>
                  Flight.Data.Wind.Set_Y   (Long_Float'Value (Val));
                  Wind_Set := True;

               when 'V' =>
                  Flight.Data.Airspeed     := Float'Value (Val);

               when others =>
                  null;

            end case;

         end;

      end loop;

      -- Calculated data
      -----------------------------------------------

      Flight.Data.Timestamp := Clock;

      Flight.Data.Elevation := Flight.Data.Altitude - Get_Elevation (Flight.Data.Position);

      if not Course_Set and then Previous_Position /= No_Position_Record then

         Maps.Coordinates (Position_A => Previous_Position,
                           Position_B => Flight.Data.Position,
                           Distance   => Flight.Data.Step,
                           Bearing    => Flight.Data.Course);

      end if;

      Flight.Cache_Data;

   end Parse_Message;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Read_Step is

      use Utility.Strings;

      --========================================================================
      -- Proceses the next valid line on the replay file
      --========================================================================
      function Next_Replay_Step return Boolean is
         Line : String := Ada.Text_IO.Get_Line (File_Id);
      begin

         if End_Of_File (File_Id) then

            Reset (File_Id);

            Flight.Clear_History;

            Ada.Text_IO.Put_Line ("replay reset");

         end if;

         if Line'Length > 0 and then Line (1) /= '#' then

            Parse_Message (Line);

            return False;

         else

            return True;

         end if;

      end Next_Replay_Step;
      --------------------------------------------------------------------------

   begin

      -- Case A: Replay data from file
      --------------------------------------------------------------------------
      if Ada.Text_IO.Is_Open (File_Id) then

         while Next_Replay_Step loop

            null;

         end loop;

      -- Case B: Online socket data (from flight simulator or middleware)
      --------------------------------------------------------------------------
      else

         loop

            Last := 0;

            Store.Move_Cursor (Data'First);

            begin
               Receive_Socket (Server, Data, Last, From);
            exception
               when E : Socket_Error =>
                  null;
            end;

            if Last in Data'Range then

               declare
                  Message : String (1.. Positive (Last));
               begin
                  String'Read (Store, Message);
                  Parse_Message (Message);
               end;

            else

               exit;

            end if;

         end loop;

      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("error while aquiring data: "& Ada.Exceptions.Exception_Message (E));
   end Read_Step;
   -----------------------------------------------------------------------------

end Flight.Simu;
--------------------------------------------------------------------------------
