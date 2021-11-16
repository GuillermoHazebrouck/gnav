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
with Ada.Command_Line;
-- Gnav
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Gavilan_Help is

   --===========================================================================
   --
   --===========================================================================
   function Provide_Help return Boolean is

      use Ada.Command_Line;
      use Ada.Text_IO;

   begin

      for I in 1..Argument_Count loop

         if
           Utility.Strings.Get_Lower_Case (Argument (I)) = "--help" then

            Put_Line ("##############################");
            Put_Line ("## GAVILAN soaring computer ##");
            Put_Line ("##############################");

            Put_Line ("List of startup variables:");

            Put_Line ("SIMU=<file name>");
            Put_Line (" Starts the application in simulation mode instead of data aquisition.");
            Put_Line (" The simulation file must be located in the local 'simu/' folder");
            Put_Line (" Eg.: 'SIMU=example.dat' will load '<gavilan directory path>/simu/example.dat'.");

            Put_Line ("ON_BOARD");
            Put_Line (" Starts the application in maximized mode, without decoration and without mouse cursor.");

            Put_Line ("EDIT_WATER");
            Put_Line (" Provides graphic tools to edit the water (zero elevation) on the terrain grid.");

            return True;

         end if;

      end loop;

      return False;

   end Provide_Help;
   -----------------------------------------------------------------------------

end Gavilan_Help;
--------------------------------------------------------------------------------
