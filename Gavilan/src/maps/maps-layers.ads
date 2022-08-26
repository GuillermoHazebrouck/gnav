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
with Ada.Finalization;
-- Gnav
with Gl;
use  Gl;
with Math.Vector2;
with Math.Vector2_List;
with Utility.Colors;
use  Utility.Colors;
with Stacks.Generic_List;
with Stacks.Linked;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Maps.Layers is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The type of layer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Layer_Types is (Layer_Unknown,
                        Layer_River,
                        Layer_Lake,
                        Layer_Rail,
                        Layer_Border,
                        Layer_Airspace);

   --===========================================================================
   -- Opens all Esri shape files and loads them on the GPU
   --===========================================================================
   procedure Load_Shape_Files;

   --===========================================================================
   -- Draws the map layers in geographic coordinates
   --===========================================================================
   procedure Draw (View : Map_View_Record);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Visible : array (Layer_Types) of Boolean := (others => True);

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The type of layer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Form_Types is (Form_Polygon, Form_Polyline);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Part_Names is String (1..20);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Part_Name : constant Part_Names := (others => ' ');

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A record containing aditional part information
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Part_Info_Record is new Ada.Finalization.Controlled with record

      Name   : Part_Names := No_Part_Name;

      Limits : Part_Names := No_Part_Name;

      Points : Math.Vector2_List.Stack_Access := new Math.Vector2_List.Stack;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Part_Info_Access is access all Part_Info_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A part in a layer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Part_Record is new Stacks.Linked.Linked_Record with record

      Id   : Gl_Uint  := 0;

      Size : Gl_Sizei := 0;

      Info : Part_Info_Access := null;

   end record;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Part_Access is access all Part_Record'Class;

   --===========================================================================
   -- Loads the resources using the cached nodes
   --===========================================================================
   procedure Load_Resources (This : in out Part_Record);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   package Part_List is new Stacks.Generic_List (T_Record => Part_Record,
                                                 T_Access => Part_Access,
                                                 Maximum  => 600);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A layer of map features
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Layer_Record is new Stacks.Linked.Linked_Record with record

      Name  : Part_Names := (others => ' ');

      Parts : Part_List.Stack_Access := new Part_List.Stack;

      Color : Color_Record := Color_Black;

      Glow  : Boolean := False;

      Form  : Form_Types := Form_Polyline;

      Kind  : Layer_Types := Layer_Unknown;

   end record;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Layer_Access is access all Layer_Record'Class;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   package Layer_List is new Stacks.Generic_List (T_Record => Layer_Record,
                                                  T_Access => Layer_Access,
                                                  Maximum  => 100);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The collection of layers
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Layers : Layer_List.Stack_Access := new Layer_List.Stack;

end Maps.Layers;
--------------------------------------------------------------------------------
