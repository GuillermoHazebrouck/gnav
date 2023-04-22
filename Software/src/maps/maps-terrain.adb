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
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Numerics.Short_Elementary_Functions;
-- Gnav
with Gl;
use  Gl;
with Gl.Colors;
with Gl.Resources;
with Gl.Shaders;
with Utility.Log;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Terrain is

   package Short_Math renames Ada.Numerics.Short_Elementary_Functions;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The terrain data for a grid node (in 4 bytes)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Terrain_Data_Record is record

      -- Terrain altitude
      --------------------------------
      Z : Short_Float;

      -- Terrain shadow
      --------------------------------
      S : Short_Float;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Default terrain data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Terrain_Data : Terrain_Data_Record := (0.0, 1.0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- 320MiB static buffer containing the terrain data grid
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Terrain : array (1..40000000) of Terrain_Data_Record := (others => No_Terrain_Data);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Map grid data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   -- Lower left corner
   --------------------------------
   Reference : Position_Record := No_Position_Record;

   -- The middle point of the chart
   --------------------------------
   Middle : Position_Record := No_Position_Record;

   -- The size of a cell (degrees)
   --------------------------------
   Cell_Size : Float;

   -- No data value
   --------------------------------
   No_Data : Float := 0.0;

   -- Lower vertical limit (meters)
   --------------------------------
   Z_Min_Global : Float := 0.0;

   -- Upper vertical limit (meters)
   --------------------------------
   Z_Max_Global : Float := 0.0;

   -- Number of rows (latitude)
   --------------------------------
   M : Natural := 0;

   -- Number of columns (longitude)
   --------------------------------
   N : Natural := 0;

   -- Buffer for the vertices
   --------------------------------
   Buffer_Id : Gl_Uint;

   -- Elements on screen (~2.3MiB)
   --------------------------------
   Elements : Gl_Ushort_Vec (1..3 * 480 * 800) := (others => 0);

   -- Elements
   --------------------------------
   Elements_Count : Gl_Sizei := 0;

   -- Permanent pointer to elements
   --------------------------------
   Elements_Pointer : access Gl_Ushort_Vec := Elements'Unrestricted_Access;

   -- The map has been loaded
   --------------------------------
   Loaded : Boolean := False;

   -- Previous view
   --------------------------------
   Old_View : Map_View_Record;

   -- Forces a map reload
   --------------------------------
   Reload : Boolean;

   -- Forces a map reload
   --------------------------------
   Terrain_Too_Large_Message : String := "unable to load the given terrain file, the size is too large";



   --===========================================================================
   -- Notifies that a reload is required
   --===========================================================================
   procedure Notify_Range_Changed is
   begin

      if Old_View.Cone_Active then

         Reload := True;

      end if;

   end Notify_Range_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Clears the grid
   --===========================================================================
   procedure Clear_Grid is
   begin

      Utility.Log.Put_Message ("clearing the terrain grid");

      N         := 0;

      M         := 0;

      Cell_Size := 0.0;

      Terrain   := (others => No_Terrain_Data);

      Reload    := True;

   end Clear_Grid;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Bin_File_Name return String is
      use  Utility.Strings;
   begin

      return -Dataset_Path & "terrain.bin";

   end Bin_File_Name;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Dat_File_Name return String is
      use  Utility.Strings;
   begin

      return -Dataset_Path & "terrain.asc";

   end Dat_File_Name;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Modify_Altitude (Position    : Position_Record;
                              Elevation   : Short_Float := 0.1;
                              Extent      : Integer     := 0;
                              Lower_Level : Short_Float := Short_Float'First;
                              Upper_Level : Short_Float := Short_Float'Last) is

      I, J, K : Integer;

   begin

      I := Integer (Float (Position.Lat - Reference.Lat) / Cell_Size);
      J := Integer (Float (Position.Lon - Reference.Lon) / Cell_Size);

      for R in I - Extent..I + Extent loop

         for C in J - Extent..J + Extent loop

            if R in 1..M and C in 1..N then

               K := (R - 1) * N + C;

               if Terrain (K).Z >= Lower_Level and Terrain (K).Z <= Upper_Level then

                  Terrain (K).Z := Elevation;

                  Reload := True;

               end if;

            end if;

         end loop;

      end loop;

   exception

      when others =>

         Utility.Log.Put_Message ("error when modifying altitude");

   end Modify_Altitude;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Computes the terrain shaddows for a given light incidence
   --===========================================================================
   procedure Compute_Shadows (Angle : Short_Float := 0.0) is

      use Short_Math;
      Uz, Vz : Short_Float;
      Ux, Vy : constant Short_Float := Short_Float (2.0 * 110580.0 * Cell_Size);
      Uy, Vx : constant Short_Float := 0.0;
      Nx, Ny, Nz, Nl, S : Short_Float;
      Light_X : Short_Float := Short_Math.Cos (Angle / 180.0 * 3.1415);
      Light_Y : Short_Float := Short_Math.Sin (Angle / 180.0 * 3.1415);

   begin

      Utility.Log.Put_Message ("computing terrain shadows");

      for I in 2..M-1 loop

         for J in 2..N-1 loop

            Uz := Terrain ((I - 1) * N + J + 1).Z - Terrain ((I - 1) * N + J - 1).Z;

            Vz := Terrain (I * N + J).Z - Terrain ((I - 2) * N + J).Z;

            Nx := Uy * Vz - Uz * Vy;

            Ny := Uz * Vx - Ux * Vz;

            Nz := Ux * Vy - Uy * Vx;

            Nl := Sqrt (Nx * Nx + Ny * Ny + Nz * Nz);

            S := (Light_X * Nx + Light_Y * Ny) / Nl;

            if S > 0.0 then

               Terrain ((I - 1) * N + J).S := (1.0 - S);

            else

               Terrain ((I - 1) * N + J).S := S;

            end if;

         end loop;

      end loop;

      Utility.Log.Put_Message ("done");

   end Compute_Shadows;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Grid_File is

      use Utility.Strings;

   begin

      Utility.Log.Put_Message ("reading terrain");

      if Ada.Directories.Exists (Bin_File_Name) then

         Load_Binary;

      elsif Ada.Directories.Exists (Dat_File_Name) then

         Load_Esri_Grid_File;

         Utility.Log.Put_Message ("saving binary terrain format");

         Save_Binary;

      else

         Utility.Log.Put_Message ("no terrain data available");

      end if;

      Utility.Log.Put_Message ("terrain buffer size:  " & Float_Image (Float (Terrain'Length * Terrain_Data_Record'Size) / 8_000_000.0, 0) & "MiB");

      Utility.Log.Put_Message ("terrain buffer usage: " & Float_Image (Float (N * M) / Float (Terrain'Length) * 100.0, 0) & "%");

      Compute_Shadows (0.0);

   end Load_Grid_File;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Opens an Esri ASCII grid file and loads the terrain data into the buffer
   --===========================================================================
   procedure Load_Esri_Grid_File is

      use Ada.Text_IO;
      use Utility.Strings;

      File_Id : File_Type;

      C, I, U : Natural := 0;

      Z : Float := 0.0;

      Reader : Utility.Strings.String_Buffer (100000);

   begin

      Utility.Log.Put_Message ("loading terrain grid");

      if Ada.Directories.Exists (Dat_File_Name) then

         -- Reset
         -----------------------------------------------------------------------
         Loaded := False;

         Reference := No_Position_Record;
         N         := 0;
         M         := 0;
         Cell_Size := 0.0;
         No_Data   := 0.0;

         -- Start reading
         -----------------------------------------------------------------------
         Open (File_Id, In_File, Dat_File_Name);

         while not End_Of_File (File_Id) loop

            C := C + 1;

            Reader.Load (Ada.Text_IO.Get_Line (File_Id));

            -- This is the header
            --------------------------------------------------------------------
            if C <= 6 then

               declare
                  Key   : String := Get_Lower_Case (Reader.Read_Next);
                  Value : String := Reader.Read_Next;
               begin

                  if Key = "ncols" then

                     N := Natural'Value (Value);

                     Utility.Log.Put_Message ("N =" & Natural'Image (N));

                  elsif Key = "nrows" then

                     M := Natural'Value (Value);

                     I := M - 1;

                     Utility.Log.Put_Message ("M =" & Natural'Image (M));

                  elsif Key = "xllcorner" then

                     Reference.Lon := Long_Float'Value (Value);

                  elsif Key = "yllcorner" then

                     Reference.Lat := Long_Float'Value (Value);

                  elsif Key = "cellsize" then

                     Cell_Size := Float'Value (Value);

                  elsif Key = "nodata_value" then

                     No_Data := Float'Value (Value);

                  end if;

               end;

               -- Check if the buffer is sufficiently large
               -----------------------------------------------------------------

               if N * M > Terrain'Length then

                  Utility.Log.Put_Message (Terrain_Too_Large_Message);

                  Clear_Grid;

                  return;

               end if;

            -- This is the data
            --------------------------------------------------------------------
            else

               -- Read a full row
               --------------------------------------------------------------
               for J in 1.. N loop

                  Z := Float'Value (Reader.Read_Next);

                  if Z = No_Data then

                     Z := 0.0;

                     U := U + 1;

                  end if;

                  Z_Min_Global := Float'Min (Z, Z_Min_Global);

                  Z_Max_Global := Float'Max (Z, Z_Max_Global);

                  -- Load a vertex
                  --------------------------------------------------------------
                  Terrain (I * N + J).Z := Short_Float (Z);

               end loop;

               if I > 0 then

                  I := I - 1;

               else

                  exit;

               end if;

            end if;

         end loop;

         Ada.Text_IO.Close (File_Id);

         Utility.Log.Put_Message ("terrain data loaded");

         -- Normalize altitude
         ----------------------------------------------
         Utility.Log.Put_Message ("normalizing altitude");

         Loaded := True;

         -- Report limits, size and unknown points
         ----------------------------------------------
         Middle.Lat := Reference.Lat + Long_Float (0.5 * Float (M) * Cell_Size);

         Middle.Lon := Reference.Lon + Long_Float (0.5 * Float (N) * Cell_Size);

         Utility.Log.Put_Message ("Lat: " & Long_Float'Image (Middle.Lat));

         Utility.Log.Put_Message ("Lon :" & Long_Float'Image (Middle.Lon));

         Utility.Log.Put_Message ("Zmin:" & Float'Image (Z_Min_Global));

         Utility.Log.Put_Message ("Zmax:" & Float'Image (Z_Max_Global));

         Utility.Log.Put_Message ("Size:" & Natural'Image (Float'Size * I / 8_000_000) & "MB");

         Utility.Log.Put_Message ("Unkn:" & Natural'Image (U));

         Utility.Log.Put_Message ("terrain grid loaded");

      else

         Utility.Log.Put_Message ("terrain data not found");

         Clear_Grid;

      end if;

   exception

      when E : others =>

         Utility.Log.Put_Message ("failed to load the terrain");

         Loaded := False;

         if Is_Open (File_Id) then
            Close (File_Id);
         end if;

   end Load_Esri_Grid_File;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Saves the chart in binary format
   --===========================================================================
   procedure Save_Binary is

      use Ada.Streams.Stream_IO;

      File_Id : File_Type;
      Stream  : Stream_Access;

   begin

      if N * M <= Terrain'Length then

         Utility.Log.Put_Message ("saving terrain data in binary");

         -- Start writing
         -----------------------------------------------------------------------

         Create (File => File_Id,
                 Mode => Out_File,
                 Name => Bin_File_Name);

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         Natural'Write (Stream, N);
         Natural'Write (Stream, M);

         Long_Float'Write (Stream, Reference.Lat);
         Long_Float'Write (Stream, Reference.Lon);
         Long_Float'Write (Stream, Middle.Lat);
         Long_Float'Write (Stream, Middle.Lon);
         Float'Write (Stream, Cell_Size);
         Float'Write (Stream, Z_Min_Global);
         Float'Write (Stream, Z_Max_Global);

         for I in 1 .. N * M loop

            Short_Float'Write (Stream, Terrain (I).Z);

         end loop;

         Close (File_Id);

      end if;

   end Save_Binary;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Loads the chart from a binary file
   --===========================================================================
   procedure Load_Binary  is

      use Ada.Streams.Stream_IO;

      File_Id : File_Type;

      Stream  : Stream_Access;

   begin

      if Ada.Directories.Exists (Bin_File_Name) then

         Utility.Log.Put_Message ("loading terrain data from binary");

         -- Start writing
         -----------------------------------------------------------------------

         Ada.Streams.Stream_IO.Open (File => File_Id,
                                     Mode => In_File,
                                     Name => Bin_File_Name);

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         Natural'Read (Stream, N);
         Natural'Read (Stream, M);

         if N * M > Terrain'Length then

            Utility.Log.Put_Message (Terrain_Too_Large_Message);

            Clear_Grid;

            return;

         end if;

         Long_Float'Read (Stream, Reference.Lat);
         Long_Float'Read (Stream, Reference.Lon);
         Long_Float'Read (Stream, Middle.Lat);
         Long_Float'Read (Stream, Middle.Lon);
         Float'Read (Stream, Cell_Size);
         Float'Read (Stream, Z_Min_Global);
         Float'Read (Stream, Z_Max_Global);

         for I in 1 .. N * M loop

            Short_Float'Read (Stream, Terrain (I).Z);

         end loop;

         Close (File_Id);

         Loaded := True;

      else

         Clear_Grid;

      end if;

   exception
      when others =>

         if Is_Open (File_Id) then
            Close (File_Id);
         end if;

   end Load_Binary;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- Returns the middle for the entire terrain chart.
   --===========================================================================
   function Get_Middle return Position_Record is
   begin

      return Middle;

   end Get_Middle;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (View : Map_View_Record) is

      use Gl;
      use type Gl_Uint;
      use type Gl_Sizei;
      use type Gl_Ushort;

      -- Target tile size in pixels
      -----------------------------------------------
      Resolution : constant Float := 3.0;

      -- The zoom levels, in number of degrees/pixel
      -----------------------------------------------
      Zoom_X : Float := View.Zoom * View.Shrink;
      Zoom_Y : Float := View.Zoom;

      -- The intended size of a tile in degrees
      -----------------------------------------------
      Tile_Size_X : Float;
      Tile_Size_Y : Float;

      -- The intended corner
      -----------------------------------------------
      Corner : Position_Record := No_Position_Record;

      -- The required number of cells to cover a tile
      -----------------------------------------------
      Tile_Step_X : Positive := 1;
      Tile_Step_Y : Positive := 1;

      -- Screen limits
      -----------------------------------------------
      Screen_T : Float;
      Screen_R : Float;
      Screen_L : Float;
      Screen_B : Float;

      -- Drawing units per degree
      -----------------------------------------------
      Scale_X : Float;
      Scale_Y : Float;

      -- Screen limits
      -----------------------------------------------
      Chart_T  : Float;
      Chart_R  : Float;
      Chart_L  : Float;
      Chart_B  : Float;

      -- Grid boundary indices
      -----------------------------------------------
      Grid_T   : Integer;
      Grid_R   : Integer;
      Grid_L   : Integer;
      Grid_B   : Integer;

      -- Number of vertical and horizontal tiles
      -----------------------------------------------
      Tiles_W   : Natural;
      Tiles_H   : Natural;

      -- Lower left corner of tiles
      -----------------------------------------------
      Tiles_L   : Float;
      Tiles_B   : Float;

      -- Colors
      -----------------------------------------------
      R, G, B : Float;

   begin

      if not Loaded or View.Height = 0.0 or View.Width = 0.0 or Z_Max_Global < View.Zero then

         return;

      end if;

      -- Reload data on GPU if necessary
      --------------------------------------------------------------------------

      if View /= Old_View or Reload then

         Reload := False;

         Old_View := View;

         -- Compute the size of a tile in degrees
         -----------------------------------------------------------------------
         Tile_Size_X := Zoom_X * Resolution;

         Tile_Step_X := Positive (Float'Ceiling (Tile_Size_X / Cell_Size));

         Tile_Size_X := Cell_Size * Float (Tile_Step_X);

         Tile_Size_Y := Zoom_Y * Resolution;

         Tile_Step_Y := Positive (Float'Ceiling (Tile_Size_Y / Cell_Size));

         Tile_Size_Y := Cell_Size * Float (Tile_Step_Y);

         -- Compute screen limits
         -----------------------------------------------------------------------
         Screen_B := Float (View.Center.Lat) - 0.5 * Zoom_Y * View.Height;

         Screen_L := Float (View.Center.Lon) - 0.5 * Zoom_X * View.Width;

         Screen_T := Float (View.Center.Lat) + 0.5 * Zoom_Y * View.Height;

         Screen_R := Float (View.Center.Lon) + 0.5 * Zoom_X * View.Width;

         -- Compute chart limits
         -----------------------------------------------------------------------
         Chart_B  := Float (Reference.Lat);

         Chart_L  := Float (Reference.Lon);

         Chart_T  := Float (Reference.Lat) + Float (M) * Cell_Size;

         Chart_R  := Float (Reference.Lon) + Float (N) * Cell_Size;

         -- Check if the chart is outside the screen boundary (nothing to draw)
         -----------------------------------------------------------------------
         if Chart_L >= Screen_R or Chart_R <= Screen_L or Chart_T <= Screen_B or Chart_B >= Screen_T then

            -- Draw arrows indicating where the chart is and exit

            Utility.Log.Put_Message ("out of terrain");

            declare
               Garbage : aliased Gl_Uint_Vec := (1 => Buffer_Id);
            begin
               Gl.Delete_Buffers (1, Garbage'Access);
               Buffer_Id := 0;
            end;

            return;

         end if;

         -- Compute grid bounds
         -----------------------------------------------------------------------
         Grid_B := Integer'Max (1, Integer'Min (M, Integer (Float'Floor   ((Screen_B - Chart_B) / Cell_Size))));

         Grid_L := Integer'Max (1, Integer'Min (N, Integer (Float'Floor   ((Screen_L - Chart_L) / Cell_Size))));

         Grid_T := Integer'Min (M, Integer'Max (1, Integer (Float'Ceiling ((Screen_T - Chart_B) / Cell_Size)) + Tile_Step_Y));

         Grid_R := Integer'Min (N, Integer'Max (1, Integer (Float'Ceiling ((Screen_R - Chart_L) / Cell_Size)) + Tile_Step_X));

         -- Calculate number of tiles (trimmed to a multiple of the tile step)
         -----------------------------------------------------------------------
         Tiles_W := (Grid_R - Grid_L) / Tile_Step_X;

         Tiles_H := (Grid_T - Grid_B) / Tile_Step_Y;

         -- Check that there is something to draw
         -----------------------------------------------------------------------
         if Tiles_W = 0 or Tiles_H = 0 then

            Utility.Log.Put_Message ("out of terrain");

            declare
               Garbage : aliased Gl_Uint_Vec := (1 => Buffer_Id);
            begin
               Gl.Delete_Buffers (1, Garbage'Access);
               Buffer_Id := 0;
            end;

            return;

         end if;

         Scale_X := 1.0 / (View.Width  * Zoom_X);

         Scale_Y := 1.0 / (View.Height * Zoom_Y);

         Tiles_B  := Chart_B + Float (Grid_B - 1) * Cell_Size;

         Tiles_L  := Chart_L + Float (Grid_L - 1) * Cell_Size;

         declare

            P       : Position_Record;
            C, E    : Natural := 0;
            Nodes   : Gl_Float_Vec (1 .. 5 * (Tiles_H + 1) * (Tiles_W + 1));
            X, Y, Z : Float;
            I, J    : Natural;
            I1, J1  : Gl_Ushort;
            I2, J2  : Gl_Ushort;
            S       : Float;

         begin

            -- Vertices
            --------------------

            P.Lat := Long_Float (Tiles_B); -- + 0.5 * Long_Float (Tile_Size_Y);

            Y := (Tiles_B - Screen_B) * Scale_Y;

            I := (Grid_B - 1) * N;

            for Count_H in 0..Tiles_H loop

               P.Lon := Long_Float (Tiles_L); -- + 0.5 * Long_Float (Tile_Size_X);

               X := (Tiles_L - Screen_L) * Scale_X;

               J := Grid_L;

               for Count_W in 0..Tiles_W loop

                  Z := Float (Terrain (J + I).Z);

                  if View.Shadow then

                     S := Float (Terrain (J + I).S);

                  else

                     S := 1.0;

                  end if;

                  View.Find_Color (P, Z, S, Z_Min_Global, Z_Max_Global, R, G ,B);

                  C := C + 1;
                  Nodes (C) := X;

                  C := C + 1;
                  Nodes (C) := Y;

                  C := C + 1;
                  Nodes (C) := R;

                  C := C + 1;
                  Nodes (C) := G;

                  C := C + 1;
                  Nodes (C) := B;

                  P.Lon := P.Lon + Long_Float (Tile_Size_X);

                  X := X + Tile_Size_X * Scale_X;

                  J := J + Tile_Step_X;

               end loop;

               P.Lat := P.Lat + Long_Float (Tile_Size_Y);

               Y := Y + Tile_Size_Y * Scale_Y;

               I := I + N * Tile_Step_Y;

            end loop;

            -- Elements
            --------------------

            E  := 0;
            I1 := 0;
            I2 := 1;
            J1 := Gl_Ushort (Tiles_W + 1);
            J2 := Gl_Ushort (Tiles_W + 2);

            for Count_H in 1..Tiles_H loop

               for Count_W in 1..Tiles_W loop

                  if
                    E + 6 > Elements'Length or
                    J2 > Gl_Ushort'Last
                  then
                     Utility.Log.Put_Message ("insuficient terrain elements buffer");
                     goto Finished_Loading;
                  end if;

                  Elements (E + 1) := I1;

                  Elements (E + 2) := I2;

                  Elements (E + 3) := J2;

                  Elements (E + 4) := J2;

                  Elements (E + 5) := J1;

                  Elements (E + 6) := I1;

                  E := E + 6;

                  I1 := I1 + 1;
                  I2 := I2 + 1;
                  J1 := J1 + 1;
                  J2 := J2 + 1;

               end loop;

               I1 := I1 + 1;
               I2 := I2 + 1;
               J1 := J1 + 1;
               J2 := J1 + 1;

            end loop;

            <<Finished_Loading>>

            Elements_Count := Gl_Sizei (E);

            Gl.Resources.Update_Resource (Buffer_Id, Nodes'Unrestricted_Access, C);

         end;

         Utility.Log.Put_Message ("terrain reloaded");

      end if;

      -- Render
      --------------------------------------------------------------------------

      if Buffer_Id > 0 then

         Gl.Shaders.Bind_Shader (Shader => Gl.Shaders.Colormap_2D);

         Gl.Bind_Buffer (GL_ARRAY_BUFFER, Buffer_Id);

         Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 5 * Gl_Float_Size, 0);

         Gl.Vertex_Attrib_Pointer (1, 3, GL_TYPE_FLOAT, GL_FALSE, 5 * Gl_Float_Size, 2 * Gl_Float_Size);

         Gl.Draw_Elements (GL_TRIANGLES, Elements_Count, GL_TYPE_UNSIGNED_SHORT, Elements_Pointer);

      end if;

   exception
      when E : others =>
         Utility.Log.Put_Message ("error: " & Ada.Exceptions.Exception_Message (E));

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the approximate elevation at the given position
   --===========================================================================
   function Get_Elevation (Position : Position_Record) return Float is

      I, J : Integer := 0;

   begin

      if Cell_Size > 0.0 then

         I := Integer ((Position.Lon - Reference.Lon) / Long_Float (Cell_Size));

         J := Integer ((Position.Lat - Reference.Lat) / Long_Float (Cell_Size));

         if I in 1..M and J in 1..N then

            return Float (Terrain ((J - 1) * N + I).Z);

         end if;

      end if;

      return 0.0;

   end Get_Elevation;


end Maps.Terrain;
--------------------------------------------------------------------------------
