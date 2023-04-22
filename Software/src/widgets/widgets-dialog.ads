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

--//////////////////////////////////////////////////////////////////////////////
-- This widget represents a OK/CANCEL dialog that performs an external action
--//////////////////////////////////////////////////////////////////////////////
package Widgets.Dialog is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Dialog_Result_Kind is (Dialog_Ok, Dialog_Cancel);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Signature of the handing procedures
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Action_Handlers is access procedure (Result : Dialog_Result_Kind);

   --===========================================================================
   -- Initializes the widget
   --===========================================================================
   procedure Init;

   --===========================================================================
   -- Launches the dialog with a message and the action handler
   --===========================================================================
   procedure Confirm (Message : String; Handler : Action_Handlers);

   --===========================================================================
   -- Checks if the dialog is visible
   --===========================================================================
   function Is_Open return Boolean;

   --===========================================================================
   -- Draws the dialog when visible
   --===========================================================================
   procedure Draw;

   --===========================================================================
   -- Handles the action by touching the screen
   --===========================================================================
   procedure Handle_Action (X, Y : Float);

   --===========================================================================
   -- Preselects a given action (use this to toggle using the buttons)
   --===========================================================================
   procedure Preselect_Action (Value : Dialog_Result_Kind);

   --===========================================================================
   -- Excecutes the preselected action
   --===========================================================================
   procedure Expedite_Action;

end Widgets.Dialog;
--------------------------------------------------------------------------------
