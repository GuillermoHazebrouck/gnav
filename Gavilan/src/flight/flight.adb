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
-- Gnav
with Gl;
use  Gl;
with Gl.Resources;
with Gl.Shaders;
with Maps;
use  Maps;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Dots_Array is array (Dots_Range) of Flight_Data_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Cluster_Record is record

      Active  : Boolean;

      Loaded  : Boolean;

      Dots    : Dots_Array;

      Last    : Dots_Range;

      Line_Id : Gl_Uint;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Cluster_Record : constant Cluster_Record := (Active  => False,
                                                   Loaded  => False,
                                                   Dots    => (others => No_Flight_Data),
                                                   Last    => Dots_Range'First,
                                                   Line_Id => 0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Cluster_Array is array (Cluster_Range) of Cluster_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   History  : Cluster_Array := (others => No_Cluster_Record);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Current  : Cluster_Range := Cluster_Range'First;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Previous : Flight_Data_Record := No_Flight_Data;

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Cache_Data is
   begin

      -- Check if the current cluster is full
      ------------------------------------------------

      Previous := History (Current).Dots (History (Current).Last);

      if History (Current).Last = Dots_Range'Last then

         if Current = Cluster_Range'Last then

            Current := Cluster_Range'First;

         else

            Current := Current + 1;

         end if;

         -- Rest the next cluster
         --------------------------------------------

         History (Current).Active := False;
         History (Current).Last   := Dots_Range'First;
         History (Current).Dots   := (others => No_Flight_Data);

      end if;

      -- Advance the slot
      --------------------------------------------

      if History (Current).Active then

         History (Current).Last := History (Current).Last + 1;

      else

         History (Current).Active := True;

      end if;

      -- Load data in the next slot and mark for
      -- reloading on the GPU
      --------------------------------------------

      History (Current).Dots (History (Current).Last) := Data;

      History (Current).Loaded := False;

   end Cache_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Clear_History is
   begin

   	Current := Cluster_Range'First;

      for I in Cluster_Range loop

         History (I).Active := False;

         History (I).Loaded := False;

         History (I).Last   := Dots_Range'First;

         History (I).Dots   := (others => No_Flight_Data);

      end loop;

   	Previous := No_Flight_Data;

   end Clear_History;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Previous return Flight_Data_Record is
   begin

      return Previous;

   end Get_Previous;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw_Horizontal_Path (View : Map_View_Record) is

      M1 : Gl_Mat_4 := Gl.Shaders.Get_Active_Matrix;

      M2 : Gl_Mat_4 := View.Geographic_Matrix;

   begin

      Gl.Shaders.Load_Matrix (M2);

      for C in Cluster_Range loop

         if History (C).Active then

            if not History (C).Loaded then

               declare

                  Buffer : Gl_Float_Vec (1..2 * Positive (History (C).Last));
                  J      : Positive := 1;

               begin

                  for D in 1..History (C).Last loop

                     Buffer (J) := Gl_Float (History (C).Dots (D).Position.Lon);
                     J := J + 1;
                     Buffer (J) := Gl_Float (History (C).Dots (D).Position.Lat);
                     J := J + 1;

                  end loop;

                  Gl.Resources.Update_Resource (History (C).Line_Id, Buffer'Unrestricted_Access);

               end;

               History (C).Loaded := True;

            end if;

            Gl.Bind_Buffer (GL_ARRAY_BUFFER, History (C).Line_Id);

            Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

            Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

            Gl.Shaders.Load_Width (2.0);

            Gl.Shaders.Load_Color (0.2, 0.2, 0.2, 1.0);

            Gl.Draw_Arrays (GL_LINE_STRIP, 0, Gl_Sizei (History (C).Last));

         end if;

      end loop;

      Gl.Shaders.Load_Matrix (M1);

   end Draw_Horizontal_Path;
   -----------------------------------------------------------------------------


end Flight;
--------------------------------------------------------------------------------
