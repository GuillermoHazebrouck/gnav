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
with Ada.Streams;
use  Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
-- Gnav
with Gl.Resources;
with Gl.Shaders;
with Utility.Streams;
use  Utility.Streams;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Layers is

   --===========================================================================
   -- Loads the resources using the cached nodes
   --===========================================================================
   procedure Load_Resources (This : in out Part_Record) is

      use Math.Vector2;

      I : Natural := 0;

      Buffer : Gl_Float_Vec (1..2 * This.Info.Points.Get_Count);

      Point  : Vector2_Access := This.Info.Points.Get_First_Item;

   begin

      while Point /= null loop

         I := I + 1;
         Buffer (I) := Gl_Float (Point.Get_X);

         I := I + 1;
         Buffer (I) := Gl_Float (Point.Get_Y);

         This.Info.Points.Get_Next_Item (Point);

      end loop;

      Gl.Resources.Update_Resource (This.Id, Buffer'Unrestricted_Access);

      This.Size := Gl_Sizei (This.Info.Points.Get_Count);

   exception

      when others =>

         Ada.Text_IO.Put_Line ("unable to load resources for layer part");

   end Load_Resources;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Reads and loads the content of the shape file
   --===========================================================================
   procedure Load_Geometry (Layer        : Layer_Access;
                            Source       : not null access Stream_Buffer_Type;
                            Parts_Count  : Integer;
                            Points_Count : Integer;
                            Load_Info    : Boolean) is

      Parts  : array (1..Parts_Count) of Integer;

      Points : Position_Record_Array (1..Points_Count);

      First  : Positive := 1;

      Last   : Positive := 1;

      --========================================================================
      procedure Load_Buffer (Sub_Set : Position_Record_Array) is

         use Math.Vector2;

         Buffer : Gl_Float_Vec (1..2 * Sub_Set'Length);

         P      : Natural := 0;

         Part   : Part_Access := null;

         Point  : Vector2_Access;

      begin

         Layer.Parts.Add_Item (Part);

         if Part = null then

            return;

         end if;

         if Load_Info then

            Part.Info := new Part_Info_Record;

         end if;

         for K in Sub_Set'Range loop

            P := P + 1;

            Buffer (P) := Float (Sub_Set (K).Lon);

            P := P + 1;

            Buffer (P) := Float (Sub_Set (K).Lat);

            if Load_Info then

               Part.Info.Points.Add_Item (Point);

               Point.Set (Sub_Set (K).Lon, Sub_Set (K).Lat);

            end if;

         end loop;

         Gl.Resources.Update_Resource (Part.Id, Buffer'Unrestricted_Access);

         Part.Size := Sub_Set'Length;

      end Load_Buffer;
      --------------------------------------------------------------------------

   begin

      for I in 1..Parts_Count loop

         Integer'Read (Source, Parts (I));

         -- Ada.Text_IO.Put_Line ("part at" & Integer'Image (Parts (I)));

      end loop;

      for J in 1..Points_Count loop

         Long_Float'Read (Source, Points (J).Lon);

         Long_Float'Read (Source, Points (J).Lat);

      end loop;

      -- Ada.Text_IO.Put_Line ("loading layer");

      -- Ada.Text_IO.Put_Line (Image (Points (Points'First)));

      -- Ada.Text_IO.Put_Line (Image (Points (Points'Last)));

      for I in 1..Parts_Count loop

         First := Parts (I) + 1;

         if I = Parts_Count then

            Last  := Points'Last;

         else

            Last  := Parts (I + 1);

         end if;

         Load_Buffer (Points (First..Last));

      end loop;

   exception

      when E : others =>

         Ada.Text_IO.Put_Line ("error while reading shape parts");

         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));

   end Load_Geometry;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Reads and loads the content of the shape file
   --===========================================================================
   procedure Process_Shape_File (File : Ada.Directories.Directory_Entry_Type) is

      use Ada.Directories;
      use Ada.Streams.Stream_IO;
      use Utility.Strings;

      Layer   : Layer_Access := null;

      Path    : String := Full_Name (File);

      Name    : String := Ada.Directories.Base_Name (Path);

      Buffer  : Stream_Element_Array (1..Stream_Element_Offset (Size (File)));

      Stream  : aliased Stream_Buffer_Type;

      Source  : not null access Stream_Buffer_Type := Stream'Access;

      File_Id : File_Type;

      Last    : Stream_Element_Offset;

      Ident   : Integer;

      Version : Integer;

      Shape   : Integer := 0;

      BL, UR  : Position_Record;

      Number  : Integer;

      Length  : Integer;

      Parts   : Integer;

      Points  : Integer;

      Load_Info : Boolean := False;

   begin

      Ada.Text_IO.Put_Line ("reading shape file: " & Path);

      Layers.Add_Item (Layer);

      if Layer = null then

         return;

      end if;

      Override (Layer.Name, Name);

      if
        Contains (Name, "rivers")
      then

         Layer.Color := Color_Water;

      elsif
        Contains (Name, "lakes")
      then

         Layer.Color := Color_Water;

      elsif
        Contains (Name, "rails")
      then

         Layer.Color := Color_Rails;

      elsif
        Contains (Name, "border")
      then

         Layer.Color := Color_Black;

         Load_Info := True;

      else

         return;

      end if;

      Open (File_Id, In_File, Path);

      Read (File_Id, Buffer, Last);

      Stream.Buffer := Buffer'Unrestricted_Access;

      Stream.Cursor := Buffer'First;

      Stream.Endianness_Type := Big_Endian;

      Integer'Read (Source, Ident);

      if Ident /= 9994 then

         Ada.Text_IO.Put_Line ("shape file code not found" & Stream_Element_Offset'Image (Last));

         goto Close_And_Leave;

      end if;

      Stream.Endianness_Type := Little_Endian;

      Stream.Move_Cursor (29);

      Integer'Read (Source, Version);

      if Version /= 1000 then

         Ada.Text_IO.Put_Line ("shape file version not supported");

         goto Close_And_Leave;

      end if;

      Integer'Read (Source, Shape);

      Ada.Text_IO.Put_Line ("shape type:" & Integer'Image (Shape));

      if Shape = 3 then

         Layer.Form := Form_Polyline;

      elsif Shape = 5 then

         Layer.Form := Form_Polygon;

      else

         Ada.Text_IO.Put_Line ("shaper type not supported");

         goto Close_And_Leave;

      end if;

      Long_Float'Read (Source, BL.Lon);

      Long_Float'Read (Source, BL.Lat);

      Long_Float'Read (Source, UR.Lon);

      Long_Float'Read (Source, UR.Lat);

      Ada.Text_IO.Put_Line ("Bottom-Left:" & Image (BL));

      Ada.Text_IO.Put_Line ("Upper-Right:" & Image (UR));

      Stream.Move_Cursor (101);

      while not Stream.End_Off_Buffer loop

         -- Record header
         ------------------------------------

         Stream.Endianness_Type := Big_Endian;

         Integer'Read (Source, Number);

         Integer'Read (Source, Length);

         -- Record content
         ------------------------------------

         Stream.Endianness_Type := Little_Endian;

         Integer'Read (Source, Shape);

         Long_Float'Read (Source, BL.Lon);

         Long_Float'Read (Source, BL.Lat);

         Long_Float'Read (Source, UR.Lon);

         Long_Float'Read (Source, UR.Lat);

         Integer'Read (Source, Parts);

         Integer'Read (Source, Points);

         -- Parts and points
         ------------------------------------

         Load_Geometry (Layer, Source, Parts, Points, Load_Info);

      end loop;

      Ada.Text_IO.Put_Line ("loaded parts:" & Natural'Image (Layer.Parts.Get_Count));

      <<Close_And_Leave>>

      Close (File_Id);

   exception
      when E : others =>

         Ada.Text_IO.Put_Line ("error while reading shape file '" & Name & "'");

         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));

         if Is_Open (File_Id) then
            Close (File_Id);
         end if;

   end Process_Shape_File;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Shape_Files is

      use Ada.Directories;

   begin

      Layers.Clear;

      Search (Directory =>-Database_Path,
              Pattern   => "*.shp",
              Process   => Process_Shape_File'Access);

   end Load_Shape_Files;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (View : Map_View_Record) is

      use  Utility.Colors;

      use type Gl_Uint;

      Layer : Layer_Access := Layers.Get_First_Item;

      Part  : Part_Access  := null;

      M1    : Gl_Mat_4 := Gl.Shaders.Get_Active_Matrix;

      M2    : Gl_Mat_4 := View.Geographic_Matrix;

   begin

      if View.Height = 0.0 or View.Width = 0.0 then

         return;

      end if;

      Gl.Shaders.Bind_Shader (Gl.Shaders.Lines_2D);

      Gl.Shaders.Load_Matrix (M2);

      while Layer /= null loop

         Part := Layer.Parts.Get_First_Item;

         if Layer.Glow then

            while Part /= null loop

               Gl.Bind_Buffer (GL_ARRAY_BUFFER, Part.Id);

               Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

               Gl.Shaders.Load_Color (1.0,1.0,1.0,1.0);

               Gl.Shaders.Load_Width (3.5, 0.8);

               case Layer.Form is

                  when Form_Polyline =>

                     Gl.Draw_Arrays (GL_LINE_STRIP, 0, Part.Size);

                     Gl.Shaders.Load_Width (1.5, 0.8);

                     Gl.Shaders.Load_Color (Layer.Color.R,
                                            Layer.Color.G,
                                            Layer.Color.B,
                                            Layer.Color.A);

                     Gl.Draw_Arrays (GL_LINE_STRIP, 0, Part.Size);

                  when Form_Polygon =>

                     Gl.Draw_Arrays (GL_LINE_LOOP, 0, Part.Size);

                     Gl.Shaders.Load_Width (1.5, 0.8);

                     Gl.Shaders.Load_Color (Layer.Color.R,
                                            Layer.Color.G,
                                            Layer.Color.B,
                                            Layer.Color.A);

                     Gl.Draw_Arrays (GL_LINE_LOOP, 0, Part.Size);

               end case;

               Layer.Parts.Get_Next_Item (Part);

            end loop;

         else

            Gl.Shaders.Load_Width (1.0);

            Gl.Shaders.Load_Color (Layer.Color.R,
                                   Layer.Color.G,
                                   Layer.Color.B,
                                   Layer.Color.A);

            while Part /= null loop

               Gl.Bind_Buffer (GL_ARRAY_BUFFER, Part.Id);

               Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

               case Layer.Form is

                  when Form_Polyline =>

                     Gl.Draw_Arrays (GL_LINE_STRIP, 0, Part.Size);

                  when Form_Polygon =>

                     Gl.Draw_Arrays (GL_LINE_LOOP, 0, Part.Size);

               end case;

               Layer.Parts.Get_Next_Item (Part);

            end loop;

         end if;

         Layers.Get_Next_Item (Layer);

      end loop;

      Gl.Shaders.Load_Matrix (M1);

   end Draw;
   -----------------------------------------------------------------------------

end Maps.Layers;
--------------------------------------------------------------------------------
