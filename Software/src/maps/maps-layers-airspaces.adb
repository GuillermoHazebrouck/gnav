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
with Ada.Text_IO;
-- Gnav
with Gl.Fonts;
with Math.Tools;
with Math.Vector2;
with Math.Vector2_List;
with Utility.Atmosphere;
use  Utility.Atmosphere;
with Utility.Log;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Layers.Airspaces is


   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Invalid_Lat_Long : exception;

   --===========================================================================
   -- Specific function for airspace position data (Eg. 505130N 0031110E)
   --===========================================================================
   function Sexagecimal_Value (Image : String) return Long_Float is

      Sign   : Long_Float := 1.0;
      Result : Long_Float := 0.0;
      F      : constant Natural := Image'First - 1;

   begin

      if Image'Length > 0 then

         case Image (Image'Last) is

         when 'N' | 'S' =>

            Result := Long_Float'Value (Image (F + 1..F + 2)) +
                      Long_Float'Value (Image (F + 3..F + 4)) / 60.0 +
                      Long_Float'Value (Image (F + 5..F + 6)) / 3600.0;

            if Image (Image'Last) = 'S' then
               Sign := -1.0;
            end if;

         when 'E' | 'O' =>

            Result := Long_Float'Value (Image (F + 1..F + 3)) +
                      Long_Float'Value (Image (F + 4..F + 5)) / 60.0 +
                      Long_Float'Value (Image (F + 6..F + 7)) / 3600.0;

            if Image (Image'Last) = 'O' then
               Sign := -1.0;
            end if;

         when others =>

            case Image (Image'First) is

               when 'N' | 'S' =>

                  Result := Long_Float'Value (Image (F + 2..F + 3)) +
                            Long_Float'Value (Image (F + 4..F + 5)) / 60.0 +
                            Long_Float'Value (Image (F + 6..F + 7)) / 3600.0;

                  if Image (Image'First) = 'S' then
                     Sign := -1.0;
                  end if;

               when 'E' | 'O' =>

                  Result := Long_Float'Value (Image (F + 2..F + 4)) +
                            Long_Float'Value (Image (F + 5..F + 6)) / 60.0 +
                            Long_Float'Value (Image (F + 7..F + 8)) / 3600.0;

                  if Image (Image'First) = 'O' then
                     Sign := -1.0;
                  end if;

               when others =>

                  null;

            end case;

         end case;

         return Sign * Result;

      else

         Utility.Log.Put_Message ("warning: invalid lat/long");

         raise Invalid_Lat_Long;

      end if;

   end Sexagecimal_Value;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Class_Names is String (1..1);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Label_Record is tagged record

      Name      : Part_Names;

      Class     : Class_Names;

      Point     : Position_Record;

      Color     : Color_Record;

      Upper_Org : Altitude_Strings;

      Lower_Org : Altitude_Strings;

      Lower_Lbl : Altitude_Strings;

      Upper_Lbl : Altitude_Strings;

   end record;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Recomputes the altitude string based on the current units and QNH
   --===========================================================================
   procedure Recompute (Label : in out Label_Record) is
   begin

      Label.Lower_Lbl := To_Altitude (Label.Lower_Org);
      Label.Upper_Lbl := To_Altitude (Label.Upper_Org);

   end Recompute;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Parse (Label : in out Label_Record; Info : String) is

      use Utility.Strings;

      Reader : String_Buffer (40);

   begin

      Reader.Load (Info);

      Override (Label.Class,     Reader.Read_Next ('/'));
      Override (Label.Lower_Org, Reader.Read_Next ('/'));
      Override (Label.Upper_Org, Reader.Read_Next ('/'));
      Label.Point.Lat := Sexagecimal_Value (Reader.Read_Next (' '));
      Label.Point.Lon := Sexagecimal_Value (Reader.Read_Next (' '));

      -- Complete text

      Label.Recompute;

   end Parse;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Default value for label data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Label_Record : constant Label_Record := (No_Part_Name,
                                               "A",
                                               No_Position_Record,
                                               Color_Black,
                                               No_Altitude_String,
                                               No_Altitude_String,
                                               No_Altitude_String,
                                               No_Altitude_String);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Label_Array is array (1..100) of Label_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The static stack of sector labels
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Labels : Label_Array := (others => (No_Label_Record));

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The actual number of labels in use
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Label_Count : Natural := 0;




   --===========================================================================
   -- Updates the vertical sector limits based on the current QNH and unit
   --===========================================================================
   procedure Recompute_Vertical_Limits is
   begin

      for I in 1..Label_Count loop

         Labels (I).Recompute;

      end loop;

   end Recompute_Vertical_Limits;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Reads a text file containing AIP namespaces and converts them into
   -- shape files and metadata.
   --===========================================================================
   procedure Generate_Airspaces is

      use Ada.Text_IO;
      use Math.Vector2;
      use Utility.Strings;

      type Data_Kinds is (Invalid_Data,       -- Line skipper
                          Airspace_Name,      -- 1st line
                          Airspace_Info,      -- 2nd line
                          Airspace_Boundary); -- 3th line and on

      File_Id    : File_Type;

      Reader     : Utility.Strings.String_Buffer (100);

      Points     : Math.Vector2_List.Stack;

      Data_Kind  : Data_Kinds := Airspace_Name;

      Border     : Layer_Access := null;

      Layer      : Layer_Access := null;

      Other      : Layer_Access := null;

      Airspace   : Part_Access  := null;

      File_Name  : String := -Dataset_Path & "airspaces.dat";

      Skip_Block : Boolean := False;

      P          : Vector2_Access := null;

      Label      : Label_Record   := No_Label_Record;

      --========================================================================
      -- Announces an error and invalidates the current airspace
      --========================================================================
      procedure Announce_Error (Message : String) is
      begin

         Utility.Log.Put_Message ("error: " & Message);

         Utility.Log.Put_Message ("airspace '" &  Trim (Airspace.Info.Name) & "' won't be included");

         Layer.Parts.Remove_Item (Airspace);

         Airspace := null;

         Data_Kind := Invalid_Data;

      end Announce_Error;
      --------------------------------------------------------------------------

      --========================================================================
      -- Indicates if the line can be used to extract data
      -- Note: the '#' character is used for comments
      --========================================================================
      function Valid_Line (Text : String) return Boolean is
      begin

         if Text'Length > 0 then

            if Text (Text'First) /= '#' then

               -- Make sure there is always a layer

               if Layer = null then

                  if Other = null then

                     Layers.Add_Item (Other);

                     Other.Color := Color_Gray_5;

                     Other.Form  := Form_Polygon;

                     Other.Glow  := True;

                     Override (Layer.Name, "UNK");

                  end if;

                  Layer := Other;

               end if;

               return True;

            else

               if Text = "#FIR" then

                  Layers.Add_Item (Layer);

                  Layer.Color := Color_Black;

                  Layer.Form  := Form_Polygon;

                  Layer.Glow  := True;

                  Override (Layer.Name, "TMA");

               elsif Text = "#TMZ" then

                  Layers.Add_Item (Layer);

                  Layer.Color := Color_Violet;

                  Layer.Form  := Form_Polygon;

                  Layer.Glow  := True;

                  Override (Layer.Name, "TMZ");

               elsif Text = "#TMA" then

                  Layers.Add_Item (Layer);

                  Layer.Color := Color_Blue;

                  Layer.Form  := Form_Polygon;

                  Layer.Glow  := True;

                  Override (Layer.Name, "TMA");

               elsif Text = "#CTR" then

                  Layers.Add_Item (Layer);

                  Layer.Color := Color_Purple;

                  Layer.Form  := Form_Polygon;

                  Layer.Glow  := True;

                  Override (Layer.Name, "CTR");

               elsif Text = "#DRP" then

                  Layers.Add_Item (Layer);

                  Layer.Color := Color_Red;

                  Layer.Form  := Form_Polygon;

                  Layer.Glow  := True;

                  Override (Layer.Name, "DRP");

               end if;

            end if;

            return False;

         else

            return False;

         end if;

      end Valid_Line;
      --------------------------------------------------------------------------

   begin

      Label_Count := 0;

      if Ada.Directories.Exists (File_Name) then

         -- Start reading
         -----------------------------------------------------------------------
         Open (File_Id, In_File, File_Name);

         while not End_Of_File (File_Id) loop

            declare
               Line : String := Trim (Ada.Text_IO.Get_Line (File_Id));
            begin

               if Line'Length = 0 then

                  -- Finish the active airspace (if not deleted due to error)
                  --------------------------------------------------------------

                  if Airspace /= null then

                     -- NOTE: we consider the list ends where it started, so
                     -- the last node is removed

                     P := Airspace.Info.Points.Get_First_Item;

                     Airspace.Info.Points.Remove_Item (P);

                     Utility.Log.Put_Message ("loading airspace resources");

                     Airspace.Load_Resources;

                     Airspace := null;

                  end if;

                  -- Expect a new airspace on the next line
                  --------------------------------------------------------------

                  Data_Kind := Airspace_Name;

               elsif Valid_Line (Line) then

                  -- Keep building active airspace
                  --------------------------------------------------------------

                  case Data_Kind is

                     when Invalid_Data =>

                        null;

                     when Airspace_Name =>

                        Layer.Parts.Add_Item (Airspace);

                        Airspace.Info := new Part_Info_Record;

                        Override (Airspace.Info.Name, Line);

                        Utility.Log.Put_Message ("adding new airspace " & Line);

                        Data_Kind := Airspace_Info;

                     when Airspace_Info =>

                        Label_Count := Natural'Min (Labels'Last, Label_Count + 1);
                        Labels (Label_Count).Parse (Line);
                        Labels (Label_Count).Color := Layer.Color;
                        Labels (Label_Count).Name := Airspace.Info.Name;

                        Data_Kind := Airspace_Boundary;

                     when Airspace_Boundary =>

                        Reader.Load (Line);

                        declare
                           Flag : String := Trim (Reader.Read_Next (' '));
                        begin

                           if
                             Flag = "BORDER"
                           then

                              if Airspace.Info.Points.Get_Count = 0 then

                                 Announce_Error ("expected point before border");

                                 goto Next_Line;

                              end if;

                              -- Search for the matching border line
                              --------------------------------------------------

                              declare
                                 Border_Name : String  := Trim (Reader.Read_Next (' '));
                                 F           : Natural := Border_Name'First - 1;
                                 Border_1    : Part_Names := No_Part_Name;
                                 Border_2    : Part_Names := No_Part_Name;
                              begin

                                 if Border_Name'Length = 5 then

                                    Override (Border_1, Border_Name (F + 1..F + 2) & "-" & Border_Name (F + 4..F + 5) & "_border");

                                    Override (Border_2, Border_Name (F + 4..F + 5) & "-" & Border_Name (F + 1..F + 2) & "_border");

                                    Border := Layers.Get_First_Item;

                                    while Border /= null loop

                                       if
                                          Border.Name = Border_1 or
                                          Border.Name = Border_2
                                       then

                                          Utility.Log.Put_Message ("border " & Trim (Border.Name) & " found");

                                          exit;

                                       end if;

                                       Layers.Get_Next_Item (Border);

                                    end loop;

                                    if Border = null then

                                       Utility.Log.Put_Message ("borders " & Trim (Border_1) & " or " & Trim (Border_2) & " not found");

                                    end if;

                                 else

                                    Utility.Log.Put_Message ("invalid border line name");

                                 end if;

                              end;

                           elsif
                             Flag = "ARC"
                           then

                              if Border /= null then

                                 Announce_Error ("expected point after border line");

                                 goto Next_Line;

                              end if;

                              declare
                                 Radius : String := Reader.Read_Next (' ');
                                 Unit   : String := Reader.Read_Next (' ');
                                 PC_Lat : String := Reader.Read_Next (' ');
                                 PC_Lon : String := Reader.Read_Next (' ');
                                 Sense  : String := Reader.Read_Next (' ');
                                 P2_Lat : String := Reader.Read_Next (' ');
                                 P2_Lon : String := Reader.Read_Next (' ');
                                 P1     : Position_Record;
                                 P2     : Position_Record;
                                 PC     : Position_Record;
                                 V1     : Vector2_Record;
                                 V2     : Vector2_Record;
                                 R      : Position_Record;
                                 A1     : Long_Float := 0.0;
                                 A2     : Long_Float := 0.0;
                                 A      : Long_Float := 0.0;
                                 N      : Natural    := 0;
                              begin

                                 P := Airspace.Info.Points.Get_Last_Item;

                                 P1.Lat := P.Get_Y;

                                 P1.Lon := P.Get_X;

                                 P2.Lat := Sexagecimal_Value (P2_Lat);

                                 P2.Lon := Sexagecimal_Value (P2_Lon);

                                 PC.Lat := Sexagecimal_Value (PC_Lat);

                                 PC.Lon := Sexagecimal_Value (PC_Lon);

                                 V1 := Maps.Vector (PC, P1);

                                 V2 := Maps.Vector (PC, P2);

                                 A1 := V1.Bearing;

                                 A2 := V2.Bearing;

                                 if Sense = ">" then

                                    if A1 >= A2 then

                                       A := A2 - A1;

                                    else

                                       A := A2 - A1 - Math.TwoPi;

                                    end if;

                                 else

                                    if A1 <= A2 then

                                       A := A2 - A1;

                                    else

                                       A := A2 - A1 + Math.TwoPi;

                                    end if;

                                 end if;

                                 N := Natural (abs A / (Math.Pi / 180.0));

                                 A := A / Long_Float (N);

                                 for I in 1..N loop

                                    V1.Rotate (A);

                                    R := Maps.Position (PC, V1);

                                    Airspace.Info.Points.Add_Item (P);

                                    P.Set (R.Lon, R.Lat);

                                 end loop;

                              end;

                           elsif
                             Flag = "CIRCLE"
                           then

                              if Border /= null then

                                 Announce_Error ("expected point after border line");

                                 goto Next_Line;

                              end if;

                              declare
                                 Radius : String := Reader.Read_Next (' ');
                                 Unit   : String := Reader.Read_Next (' ');
                                 C_Lat  : String := Reader.Read_Next (' ');
                                 C_Lon  : String := Reader.Read_Next (' ');
                                 C      : Position_Record;
                                 V      : Vector2_Record;
                                 Q      : Position_Record;
                                 R      : Long_Float := 0.0;
                                 A      : Long_Float := 0.0;
                                 N      : Natural    := 0;
                              begin

                                 C.Lat := Sexagecimal_Value (C_Lat);

                                 C.Lon := Sexagecimal_Value (C_Lon);

                                 R     := Long_Float'Value (Radius);

                                 V.Set (R, 0.0);

                                 N := Natural'Max (300, Natural'Min (8, Natural ((2.0* Math.Pi * R) / 0.2)));

                                 A := 2.0* Math.Pi / Long_Float (N);

                                 for I in 1..N loop

                                    V.Rotate (A);

                                    Q := Maps.Position (C, V);

                                    Airspace.Info.Points.Add_Item (P);

                                    P.Set (Q.Lon, Q.Lat);

                                 end loop;

                              end;

                           elsif Line'Length = 16 then

                              if Border /= null then

                                 -- Complete the pending border line
                                 -----------------------------------------------

                                 declare
                                    P1 : Vector2_Record := No_Vector2_Record;
                                    P2 : Vector2_Record := No_Vector2_Record;
                                    C1 : Vector2_Record := No_Vector2_Record;
                                    C2 : Vector2_Record := No_Vector2_Record;
                                    Bd : Part_Access    := null;
                                    S1 : Natural := 0;
                                    S2 : Natural := 0;
                                 begin

                                    P := Airspace.Info.Points.Get_Last_Item;

                                    P1.Copy_From_Access (P);

                                    Airspace.Info.Points.Remove_Item (P);

                                    P2.Set (Sexagecimal_Value (Line (9..16)),
                                            Sexagecimal_Value (Line (1.. 7)));

                                    declare
                                       Bl : Part_Access    := null;
                                       R1 : Vector2_Record := No_Vector2_Record;
                                       R2 : Vector2_Record := No_Vector2_Record;
                                       O1 : Long_Float     := Long_Float'Last;
                                       O2 : Long_Float     := Long_Float'Last;
                                       A1 : Natural        := 0;
                                       A2 : Natural        := 0;
                                       D1 : Long_Float     := Long_Float'Last;
                                       D2 : Long_Float     := Long_Float'Last;
                                    begin

                                       Bl := Border.Parts.Get_First_Item;

                                       while Bl /= null loop

                                          if Bl.Info /= null then

                                             Math.Tools.Get_Closest_Point (Bl.Info.Points, P1, False, R1, O1, A1);
                                             Math.Tools.Get_Closest_Point (Bl.Info.Points, P2, False, R2, O2, A2);

                                             if O1 < D1 and O2 < D2 then

                                                Bd := Bl;
                                                D1 := O1;
                                                D2 := O2;
                                                S1 := A1;
                                                S2 := A2;
                                                C1 := R1;
                                                C2 := R2;

                                             end if;

                                          else

                                             Utility.Log.Put_Message ("warning: no points loaded for border line");

                                          end if;

                                          Border.Parts.Get_Next_Item (Bl);

                                       end loop;

                                    end;

                                    if Bd /= null then

                                       Airspace.Info.Points.Add_Item (P);

                                       P.Copy_From_Record (C1);

                                       if S1 < S2 then

                                          for I in S1+1..S2 loop

                                             Airspace.Info.Points.Add_Item (P);

                                             P.Copy_From_Access (Bd.Info.Points.Get_Item (I));

                                          end loop;

                                       elsif S2 < S1 then

                                          for I in reverse S2+1..S1 loop

                                             Airspace.Info.Points.Add_Item (P);

                                             P.Copy_From_Access (Bd.Info.Points.Get_Item (I));

                                          end loop;

                                       end if;

                                       Airspace.Info.Points.Add_Item (P);

                                       P.Copy_From_Record (C2);

                                    else

                                       Announce_Error ("the border line could not be completed");

                                       goto Next_Line;

                                    end if;

                                 end;

                                 Border := null;

                              else

                                 -- Load the point
                                 -----------------------------------------------

                                 Airspace.Info.Points.Add_Item (P);

                                 P.Set (Sexagecimal_Value (Line (9..16)),
                                        Sexagecimal_Value (Line (1.. 7)));

                              end if;

                           else

                              Utility.Log.Put_Message ("unrecognized line '" & Line & "'");

                           end if;

                        end;

                  end case;

               end if;

            exception
               when E : others =>
                  Utility.Log.Put_Message (E, "error while decoding line """ & Line & """");

            end;

            <<Next_Line>>

         end loop;

      end if;

      -- Synchronize the sector labels with future changes in QNH and/or unit
      --------------------------------------------------------------------------

      Utility.Atmosphere.On_Qnh_Changed.Connect           (Recompute_Vertical_Limits'Access);
      Utility.Atmosphere.On_Altitude_Unit_Changed.Connect (Recompute_Vertical_Limits'Access);

   exception
      when E : others =>

         Utility.Log.Put_Message (E, "while generating airspace");

         if Is_Open (File_Id) then
            Close (File_Id);
         end if;

   end Generate_Airspaces;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw_Labels (View : Map_View_Record) is

      use Gl.Fonts;

      P    : Point_Record;
      TL   : Position_Record;
      BR   : Position_Record;
      Font : Font_Style_Record := (Width     => 0.010,
                                   Height    => 0.022,
                                   Space     => 0.008,
                                   Rendering => Gl.Fonts.Font_Glow,
                                   Thickness => Gl.Fonts.Font_Regular);

      Color : Line_Color_Record := (Fore => Color_White,
                                    Glow => Color_Gray_8);

      begin

      View.Get_Limits (TL, BR);

      for I in 1..Label_Count loop

         if
           Labels (I).Point.Lon < TL.Lon or else
           Labels (I).Point.Lon > BR.Lon or else
           Labels (I).Point.Lat > TL.Lat or else
           Labels (I).Point.Lat < BR.Lat
         then

            -- (out of screen)

            null;

         else

            P := View.To_Screen_Coordinates (Labels (I).Point);

            Color.Glow := Labels (I).Color;

            Gl.Fonts.Draw (Trim (Labels (I).Upper_Lbl),
                           X         => Float (P.Get_X),
                           Y         => Float (P.Get_Y) + 0.01,
                           Style     => Font,
                           Color     => Color,
                           Alignment => Alignment_LC);

            Gl.Fonts.Draw (Trim (Labels (I).Lower_Lbl),
                           X         => Float (P.Get_X),
                           Y         => Float (P.Get_Y) - 0.01,
                           Style     => Font,
                           Color     => Color,
                           Alignment => Alignment_TC);

            if View.Zoom < 0.0005 then

               Gl.Fonts.Draw (Trim (Labels (I).Name),
                              X         => Float (P.Get_X),
                              Y         => Float (P.Get_Y) + 0.05,
                              Style     => Font,
                              Color     => Color,
                              Alignment => Alignment_LC);

            end if;

         end if;

      end loop;

   end Draw_Labels;
   -----------------------------------------------------------------------------

end Maps.Layers.Airspaces;
--------------------------------------------------------------------------------
