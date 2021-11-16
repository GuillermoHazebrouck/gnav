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

-- External
with Ada.Finalization;
use  Ada.Finalization;
-- Gnav
with Gl;
use  Gl;
with Gl.Resources;
use  Gl.Resources;
with Utility.Colors;
use  Utility.Colors;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Widgets.Button is

   type Button_Record is new Widget_Record with private;

   --===========================================================================
   -- Represents the widget using the current open gl context
   --===========================================================================
   procedure Draw (This : in out Button_Record);

   --===========================================================================
   -- Initializes the object
   --===========================================================================
   overriding procedure Initialize (This : in out Button_Record);

   --===========================================================================
   -- Finalizes the object
   --===========================================================================
   overriding procedure Finalize (This : in out Button_Record);

   --===========================================================================
   -- Adjusts the object after assignments
   --===========================================================================
   overriding procedure Adjust (This : in out Button_Record);

   --===========================================================================
   -- Sets the text to be represented
   --===========================================================================
   procedure Set_Label (This : in out Button_Record; Text : String);

   --===========================================================================
   -- Sets the text color of the text
   --===========================================================================
   procedure Set_Label_Color (This : in out Button_Record;
                              Fore : Color_Record;
                              Glow : Color_Record := Color_Black);

   --===========================================================================
   -- Sets the size of the label font (from 0 to 1, relative to the height)
   --===========================================================================
   procedure Set_Font_Size (This        : in out Button_Record;
                            Value       : Ratio_Float;
                            Width_Ratio : Ratio_Float := 0.6;
                            Space_Ratio : Ratio_Float := 0.7);

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A bounded string used to label buttons
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Button_String is String (1..20);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The button content
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Button_Record is new Widget_Record with record

      Label       : Button_String;

      Length      : Natural;

      Font_Fore   : Color_Record;

      Font_Glow   : Color_Record;

      Font_Size   : Dimension_Float;

      Width_Ratio : Ratio_Float := 0.6;

      Space_Ratio : Ratio_Float := 0.7;

   end record;

end Widgets.Button;
--------------------------------------------------------------------------------
