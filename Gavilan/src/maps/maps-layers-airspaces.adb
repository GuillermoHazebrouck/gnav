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
with Math.Tools;
with Math.Vector2;
with Math.Vector2_List;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Layers.Airspaces is

   --===========================================================================
   --
   --===========================================================================
   function Sexagecimal_Value (Image : String) return Long_Float is

      Sign   : Long_Float := 1.0;
      Result : Long_Float := 0.0;
      F      : constant Natural := Image'First - 1;

   begin

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

      --Ada.Text_IO.Put_Line ("Sexagecimal_Value " & Image & " -> " & Long_Float'Image (Result));

      return Sign * Result;

   end Sexagecimal_Value;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Altitude (Value : String) return Float is
   begin

      if Value'Length > 2 then

         if Value (Value'First..Value'First + 1) = "FL" then

            return Float'Value (Value (Value'First + 2..Value'Last)) * 100.0;

         elsif Value (Value'Last - 1..Value'Last) = "FT" then

            return Float'Value (Value (Value'First..Value'Last - 2));

         end if;

      end if;

      return 0.0;

   end Get_Altitude;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- Reads a text file containing AIP namespaces and converts them into
   -- shape files and metadata.
   --===========================================================================
   procedure Generate_Airspaces is

      use Ada.Text_IO;
      use Math.Vector2;
      use Utility.Strings;

      type Data_Kinds is (Invalid_Data,             -- Line skipper
                          Airspace_Name,            -- 1st line
                          Airspace_Class,           -- 2nd line
                          Airspace_Vertical_Limits, -- 3rd line
                          Airspace_Lateral_Limits); -- 4th line and on

      File_Id    : File_Type;

      Reader     : Utility.Strings.String_Buffer (100);

      Points     : Math.Vector2_List.Stack;

      Data_Kind  : Data_Kinds := Airspace_Name;

      Border     : Layer_Access := null;

      Layer      : Layer_Access := null;

      Airspace   : Part_Access  := null;

      File_Name  : String := -Database_Path & "airspaces.dat";

      Skip_Block : Boolean := False;

      P          : Vector2_Access := null;

      --========================================================================
      -- Announces an error and invalidates the current airspace
      --========================================================================
      procedure Announce_Error (Message : String) is
      begin

         Ada.Text_IO.Put_Line ("error: " & Message);

         Ada.Text_IO.Put_Line ("airspace '" &  Trim (Airspace.Info.Name) & "' won't be included");

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

         return Text'Length > 0 and then Text (Text'First) /= '#';

      end Valid_Line;
      --------------------------------------------------------------------------

   begin

      if Ada.Directories.Exists (File_Name) then

         -- Start reading
         -----------------------------------------------------------------------
         Open (File_Id, In_File, File_Name);

         Layers.Add_Item (Layer);

         Layer.Color := Color_Brown;

         Layer.Form  := Form_Polygon;

         Layer.Glow  := True;

         Override (Layer.Name, "airspaces");

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

                     Ada.Text_IO.Put_Line ("loading airspace resources");

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

                        Ada.Text_IO.Put_Line ("adding new airspace " & Line);

                        Data_Kind := Airspace_Class;

                     when Airspace_Class =>

                        Data_Kind := Airspace_Vertical_Limits;

                     when Airspace_Vertical_Limits =>

                        if Airspace /= null then

                           Override (Airspace.Info.Limits, Line);

                        end if;

                        Data_Kind := Airspace_Lateral_Limits;

                     when Airspace_Lateral_Limits =>

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

                                          Ada.Text_IO.Put_Line ("border " & Trim (Border.Name) & " found");

                                          exit;

                                       end if;

                                       Layers.Get_Next_Item (Border);

                                    end loop;

                                    if Border = null then

                                       Ada.Text_IO.Put_Line ("borders " & Trim (Border_1) & " or " & Trim (Border_2) & " not found");

                                    end if;

                                 else

                                    Ada.Text_IO.Put_Line ("invalid border line name");

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

                                             Ada.Text_IO.Put_Line ("warning: no points loaded for border line");

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

                              Ada.Text_IO.Put_Line ("unrecognized line '" & Line & "'");

                           end if;

                        end;

                  end case;

               end if;

            end;

            <<Next_Line>>

         end loop;

      end if;

   exception
      when others =>

         if Is_Open (File_Id) then
            Close (File_Id);
         end if;

   end Generate_Airspaces;
   -----------------------------------------------------------------------------

end Maps.Layers.Airspaces;
--------------------------------------------------------------------------------
