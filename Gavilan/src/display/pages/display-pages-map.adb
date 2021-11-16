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
-- Gnav
with Flight.Aircraft;
with Gl.Fonts;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Colors;
use  Utility.Colors;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Map is

   -- Map page widgets
   ---------------------------------
   
   Btn_Power_Off : Button_Record;
       
   Pnl_Terrain   : Panel_Record;

   Pnl_Layers    : Panel_Record;

   Pnl_Sectors   : Panel_Record;

   --===========================================================================
   --
   --===========================================================================
   procedure Init is

      Allocation : Allocation_Record;
      
   begin
        
      -- Screen panel
      ------------------------------------------------------

      Allocation.X := 0.010;
      Allocation.Y := 0.010;
      Allocation.W := 0.320;
      Allocation.H := 0.850;

      Pnl_Terrain.Set_Allocation (Allocation);

      Pnl_Terrain.Set_Background_Color (Color_Black);

      Pnl_Terrain.Set_Transparency (0.60);

      Pnl_Terrain.Set_Show_Border (True);

      Pnl_Terrain.Set_Label ("TERRAIN", Label_Left);

      Pnl_Terrain.Set_Font_Size (0.03, 0.25);

      Pnl_Terrain.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));
 
      -- Power panel
      ------------------------------------------------------

      Allocation.X := 0.340;
      Allocation.Y := 0.010;
      Allocation.W := 0.320;
      Allocation.H := 0.850;

      Pnl_Layers.Set_Allocation (Allocation);

      Pnl_Layers.Set_Background_Color (Color_Black);

      Pnl_Layers.Set_Transparency (0.60);

      Pnl_Layers.Set_Show_Border (True);

      Pnl_Layers.Set_Label ("LAYERS", Label_Left);

      Pnl_Layers.Set_Font_Size (0.03, 0.25);

      Pnl_Layers.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      -- Power panel
      ------------------------------------------------------

      Allocation.X := 0.670;
      Allocation.Y := 0.010;
      Allocation.W := 0.320;
      Allocation.H := 0.850;

      Pnl_Sectors.Set_Allocation (Allocation);

      Pnl_Sectors.Set_Background_Color (Color_Black);

      Pnl_Sectors.Set_Transparency (0.60);

      Pnl_Sectors.Set_Show_Border (True);

      Pnl_Sectors.Set_Label ("ATC SECTORS", Label_Left);

      Pnl_Sectors.Set_Font_Size (0.03, 0.25);

      Pnl_Sectors.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

   end Init;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw (Width, Height : Float) is
   begin
         
      Pnl_Terrain.Draw;

      Pnl_Layers.Draw;

      Pnl_Sectors.Draw;

   end Draw;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin
   
      null;
      
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Handles a key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin
      
      null;
      
   end Key_Changed;
   -----------------------------------------------------------------------------
          
     
     
end Display.Pages.Map;
--------------------------------------------------------------------------------
