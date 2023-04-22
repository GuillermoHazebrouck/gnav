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
with Ada.Directories;
with Ada.Text_Io;
use  Ada.Text_Io;
-- Gnav
with Gl.Fonts;
with Math.Vector2;
use  Math.Vector2;
with Utility.Log;
with Utility.Strings;
use  Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Reference is

   --===========================================================================
   -- (See referencec file)
   --===========================================================================
   procedure Load_Reference_File is

      use Ada.Directories;

      File_Name  : String := -Dataset_Path & "reference.dat";

      File_Id    : File_Type;

      Reader     : Utility.Strings.String_Buffer (200);

      Kind       : Reference_Types := Reference_City;

      R          : Natural := 0;

   begin

      Utility.Log.Put_Message ("loading reference points");

      References := (others => No_Reference_Record);

      if Ada.Directories.Exists (File_Name) then

         -- Start reading
         -----------------------------------------------------------------------
         Open (File_Id, In_File, File_Name);

         while not End_Of_File (File_Id) and R < References'Last loop

            declare
               Line : String := Trim (Ada.Text_IO.Get_Line (File_Id));
            begin

               Utility.Log.Put_Message ("trying " & Line);

               if Line'Length > 0 then

                  if Line (Line'First) = '#' then

                     if Line = "#AIRFIELDS" then

                        Kind := Reference_Airfield;

                     elsif Line = "#LANDMARKS" then

                        Kind := Reference_Landmark;

                     elsif Line = "#WATER" then

                        Kind := Reference_Water;

                     elsif Line = "#CITIES" then

                        Kind := Reference_City;

                     elsif Line = "#VILLAGES" then

                        Kind := Reference_Village;

                     elsif Line = "#WOODS" then

                        Kind := Reference_Woods;

                     elsif Line = "#TURBINES" then

                        Kind := Reference_Turbine;

                     end if;

                  else

                     Reader.Load (Line);

                     R := R + 1;

                     References (R).Kind := Kind;

                     Override (References (R).Id,   Trim (Reader.Read_Next ('@')));

                     Override (References (R).Name, Trim (Reader.Read_Next ('@')));

                     References (R).Position := Maps.Value (Trim (Reader.Read_Next ('@')));

                     References (R).Is_Loaded := True;

                     Utility.Log.Put_Message (References (R).Id & "=>" & Maps.Image (References (R).Position));

                  end if;

               end if;

            end;

         end loop;

         Close (File_Id);

      end if;

      if R > 0 then

         Utility.Log.Put_Message ("loaded" & Natural'Image (R) & " reference locations");

      else

         Utility.Log.Put_Message ("no reference locations loaded");

      end if;

   exception
      when E : others =>
         Utility.Log.Put_Message (E, "while reading reference file");

         Close (File_Id);

   end Load_Reference_File;
   -----------------------------------------------------------------------------




   -- Fonts
   ---------------------------------
   Fonts : constant array (Reference_Types) of Gl.Fonts.Font_Style_Record := (Reference_City =>     (Width     => 0.008,
                                                                                                     Height    => 0.018,
                                                                                                     Space     => 0.006,
                                                                                                     Rendering => Gl.Fonts.Font_Glow,
                                                                                                     Thickness => Gl.Fonts.Font_Thin),
                                                                              Reference_Village =>  (Width     => 0.008,
                                                                                                     Height    => 0.018,
                                                                                                     Space     => 0.006,
                                                                                                     Rendering => Gl.Fonts.Font_Glow,
                                                                                                     Thickness => Gl.Fonts.Font_Thin),
                                                                              Reference_Woods =>    (Width     => 0.008,
                                                                                                     Height    => 0.018,
                                                                                                     Space     => 0.005,
                                                                                                     Rendering => Gl.Fonts.Font_Glow,
                                                                                                     Thickness => Gl.Fonts.Font_Regular),
                                                                              Reference_Airfield => (Width     => 0.008,
                                                                                                     Height    => 0.018,
                                                                                                     Space     => 0.006,
                                                                                                     Rendering => Gl.Fonts.Font_Glow,
                                                                                                     Thickness => Gl.Fonts.Font_Thin),
                                                                              Reference_Turbine =>  (Width     => 0.012,
                                                                                                     Height    => 0.022,
                                                                                                     Space     => 0.006,
                                                                                                     Rendering => Gl.Fonts.Font_Glow,
                                                                                                     Thickness => Gl.Fonts.Font_Thin),
                                                                              Reference_Landmark => (Width     => 0.012,
                                                                                                     Height    => 0.022,
                                                                                                     Space     => 0.006,
                                                                                                     Rendering => Gl.Fonts.Font_Glow,
                                                                                                     Thickness => Gl.Fonts.Font_Thin),
                                                                              Reference_Water    => (Width     => 0.008,
                                                                                                     Height    => 0.018,
                                                                                                     Space     => 0.006,
                                                                                                     Rendering => Gl.Fonts.Font_Glow,
                                                                                                     Thickness => Gl.Fonts.Font_Thin));

   -- Colors
   ---------------------------------
   Lines : constant array (Reference_Types) of Gl.Colors.Line_Color_Record := (Reference_City =>    (Fore => (0.80, 0.80, 0.80, 1.00),
                                                                                                     Glow => (0.35, 0.35, 0.35, 1.00)),
                                                                              Reference_Village =>  (Fore => (0.85, 0.85, 0.25, 1.00),
                                                                                                     Glow => (0.55, 0.55, 0.55, 1.00)),
                                                                              Reference_Woods =>    (Fore => (0.60, 0.80, 0.60, 1.00),
                                                                                                     Glow => (0.30, 0.50, 0.30, 1.00)),
                                                                              Reference_Airfield => (Fore => (0.80, 0.80, 0.80, 1.00),
                                                                                                     Glow => (0.30, 0.30, 1.00, 1.00)),
                                                                              Reference_Turbine =>  (Fore => (0.90, 0.90, 0.90, 1.00),
                                                                                                     Glow => (0.30, 0.30, 0.30, 1.00)),
                                                                              Reference_Landmark => (Fore => (0.60, 0.60, 0.00, 1.00),
                                                                                                     Glow => (1.00, 1.00, 0.60, 1.00)),
                                                                              Reference_Water    => (Fore => (0.00, 1.00, 1.00, 1.00),
                                                                                                     Glow => (0.50, 0.60, 1.00, 1.00)));

   --===========================================================================
   --
   --===========================================================================
   procedure Draw (View : Map_View_Record) is

      use Gl;
      use Gl.Fonts;
      use Utility.Strings;

      Point        : Point_Record;
      Top_Left     : Position_Record;
      Bottom_Right : Position_Record;

   begin

      View.Get_Limits (Top_Left, Bottom_Right);

      for R in References'Range loop

         exit when not References (R).Is_Loaded;

         if
           References (R).Position.Lat > Bottom_Right.Lat and then
           References (R).Position.Lon < Bottom_Right.Lon and then
           References (R).Position.Lat < Top_Left.Lat     and then
           References (R).Position.Lon > Top_Left.Lon
         then

            Point := View.To_Screen_Coordinates (References (R).Position);

            case References (R).Kind is

               when Reference_Woods =>

                  if View.Zoom < 0.0002 then

                     Gl.Fonts.Draw ("}}}",
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y),
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_CC);

                     Gl.Fonts.Draw (Trim (References (R).Name),
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y) - 0.03,
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_TC);

                  elsif View.Zoom < 0.0004 then

                     Gl.Fonts.Draw ("}}",
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y),
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_CC);

                     Gl.Fonts.Draw (Trim (References (R).Id),
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y) - 0.03,
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_TC);

                  else

                     Gl.Fonts.Draw ("}",
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y),
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_CC);

                  end if;

               when Reference_Turbine =>

                  if View.Zoom < 0.0015 then

                     Gl.Fonts.Draw ("Y",
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y),
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_CC);

                  end if;

               when Reference_Landmark =>

                  if View.Zoom < 0.0015 then

                     Gl.Fonts.Draw ("}",
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y),
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_CC);

                  end if;

               when Reference_Water =>

                  if View.Zoom < 0.0002 then

                     Gl.Fonts.Draw ("~~~",
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y),
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_CC);

                     Gl.Fonts.Draw (Trim (References (R).Name),
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y) - 0.03,
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_TC);

                  elsif View.Zoom < 0.0004 then

                     Gl.Fonts.Draw ("~~",
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y),
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_CC);

                     Gl.Fonts.Draw (Trim (References (R).Id),
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y) - 0.03,
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_TC);

                  else

                     Gl.Fonts.Draw ("~",
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y),
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_CC);

                  end if;

               when Reference_Airfield =>

                  Gl.Fonts.Draw (Trim (References (R).Id),
                                 Float (Point.Get_X),
                                 Float (Point.Get_Y),
                                 Fonts (References (R).Kind),
                                 Lines (References (R).Kind),
                                 Alignment_CC);

               when Reference_City |
                    Reference_Village =>

                  if View.Zoom > 0.0005 then

                     if
                       References (R).Kind = Reference_City or else
                       View.Zoom < 0.0015
                     then

                        Gl.Fonts.Draw (Trim (References (R).Id),
                                       Float (Point.Get_X),
                                       Float (Point.Get_Y),
                                       Fonts (References (R).Kind),
                                       Lines (References (R).Kind),
                                       Alignment_CC);

                     end if;

                  else

                     Gl.Fonts.Draw (Trim (References (R).Name),
                                    Float (Point.Get_X),
                                    Float (Point.Get_Y),
                                    Fonts (References (R).Kind),
                                    Lines (References (R).Kind),
                                    Alignment_CC);
                  end if;

            end case;

         end if;

      end loop;

   end Draw;
   -----------------------------------------------------------------------------

end Maps.Reference;
