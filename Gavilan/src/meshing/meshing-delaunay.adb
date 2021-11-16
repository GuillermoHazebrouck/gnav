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
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;
with Ada.Text_IO;
-- Gnav
with Math.Tools;
with Math.Vector2;
use  Math.Vector2;
with Math.Vector2_List;
with Math.Vector2_Vector;
with Meshing.Triangle;
use  Meshing.Triangle;
with Meshing.Triangle_List;
with Meshing.Triangle_Vector;
with Meshing.Segment;
use  Meshing.Segment;
with Meshing.Segment_List;
use  Meshing.Segment_List;

--//////////////////////////////////////////////////////////////////////////////
-- Provides methods to generate a Delaunay triangulation over a polygon defined
-- by a concatenation of consecutive vertices.
--//////////////////////////////////////////////////////////////////////////////
package body Meshing.Delaunay is


   -----------------------------------------------------------------------------
   -- A local reference to the actual polygon nodes.
   -----------------------------------------------------------------------------
   Polygon : Math.Vector2_List.Stack_Access;

   -----------------------------------------------------------------------------
   -- A local reference to the target vertices stack.
   -----------------------------------------------------------------------------
   Vertices : Math.Vector2_List.Stack;

   -----------------------------------------------------------------------------
   -- A local definition of the boundary.
   -----------------------------------------------------------------------------
   Segments : Meshing.Segment_List.Stack;

   -----------------------------------------------------------------------------
   -- A local reference to the mesh.
   -----------------------------------------------------------------------------
   Triangles : Meshing.Triangle_List.Stack;

   -----------------------------------------------------------------------------
   -- Starting vertex 1 (not part of the final triangulation).
   -----------------------------------------------------------------------------
   Point_1 : Math.Vector2.Vector2_Access := new Math.Vector2.Vector2_Record;

   -----------------------------------------------------------------------------
   -- Starting vertex 2 (not part of the final triangulation).
   -----------------------------------------------------------------------------
   Point_2 : Math.Vector2.Vector2_Access := new Math.Vector2.Vector2_Record;

   -----------------------------------------------------------------------------
   -- Starting vertex 3 (not part of the final triangulation).
   -----------------------------------------------------------------------------
   Point_3 : Math.Vector2.Vector2_Access := new Math.Vector2.Vector2_Record;

   -----------------------------------------------------------------------------
   -- The level of recursion for the pseudo polygon completion.
   -----------------------------------------------------------------------------
   Recursion_Level : Natural := 0;

   -----------------------------------------------------------------------------
   -- The level of recursion for the pseudo polygon completion.
   -----------------------------------------------------------------------------
   Recursion_Limit : Natural := 0;

   -----------------------------------------------------------------------------
   -- The center of the polygon.
   -----------------------------------------------------------------------------
   Center : Math.Vector2.Vector2_Record;

   -----------------------------------------------------------------------------
   -- The scale of the polygon.
   -----------------------------------------------------------------------------
   Scale : Long_Float;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   procedure Generate_Mesh (Nodes  : Math.Vector2_List.Stack_Access;
                            Mesh   : Meshing.Triangle_List.Stack_Access;
                            Cloud  : Math.Vector2_List.Stack_Access := null) is

      use type Math.Vector2_List.Stack_Access;

      use type Meshing.Triangle_List.Stack_Access;

      Vertex : Math.Vector2.Vector2_Access;

      procedure Load_Mesh (Source : Math.Vector2_List.Stack_Access) is

         use type Math.Vector2.Vector2_Access;

         Vertex_A, Vertex_B, Vertex_C : Math.Vector2.Vector2_Access;

         Local_Triangle : Triangle_Access;

         Outer_Triangle : Meshing.Triangle.Triangle_Access;

      begin

         Local_Triangle := Triangles.Get_First_Item;

         while Local_Triangle /= null loop

            if Local_Triangle.Get_Initialized then

               Vertex_A := Source.Get_Item (Positive (Local_Triangle.Get_Vertex_A.Get_Id));

               Vertex_B := Source.Get_Item (Positive (Local_Triangle.Get_Vertex_B.Get_Id));

               Vertex_C := Source.Get_Item (Positive (Local_Triangle.Get_Vertex_C.Get_Id));

               if Vertex_A /= null and Vertex_B /= null and Vertex_C /= null then

                  Mesh.Add_Item (Outer_Triangle);

                  Outer_Triangle.Set_Vertices (Vertex_A, Vertex_B, Vertex_C);

               end if;

            end if;

            Triangles.Get_Next_Item (Local_Triangle);

         end loop;

      end;

   begin

      if (Nodes /= null and Mesh /= null) and then Nodes.Get_Count > 2 then

         if Verbosity_Level > 2 then

            Point_1.Set_Id (9991);

            Point_2.Set_Id (9992);

            Point_3.Set_Id (9993);

         end if;

         Abort_Meshing := False;

         Publish_Start;

         Polygon := Nodes;

         Triangles.Clear;

         Segments.Clear;

         Vertices.Clear;

         -----------------------------------------------------------------------
         -- Load the nodes
         -----------------------------------------------------------------------

         Load_Nodes;

         -----------------------------------------------------------------------
         -- Generate boundary
         -----------------------------------------------------------------------

         if Cloud = null then

            Set_Status_Message ("Generating boundary on nodes...");

            Generate_Boundary_1;

         else

            Set_Status_Message ("Generating boundary on back buffer...");

            Generate_Boundary_2;

         end if;

         Indexate_Vertices;

         -----------------------------------------------------------------------
         -- Generating start triangle
         -----------------------------------------------------------------------

         Generate_Starting_Triangle;

         -----------------------------------------------------------------------
         -- Adding vertices
         -----------------------------------------------------------------------

         Set_Status_Message ("Adding vertices...");

         Reset_Progress (Vertices.Get_Count);

         Vertex := Vertices.Get_First_Item;

         while Vertex /= null and not Abort_Meshing loop

            Insert_Vertex (Vertex);

            Vertices.Get_Next_Item (Vertex);

            Push_Progress;

         end loop;

         -----------------------------------------------------------------------
         -- Insert missing segments
         -----------------------------------------------------------------------

         if not Abort_Meshing then

            Set_Status_Message ("Adding missing segments...");

            Insert_Missing_Segments;

         end if;

         -----------------------------------------------------------------------
         -- Cleaning mesh
         -----------------------------------------------------------------------

         Mesh.Clear;

         if Cloud /= null then

            Cloud.Clear;

         end if;

         if not Abort_Meshing then

            Set_Status_Message ("Cleaning mesh...");

            Clean_Mesh;

            if Cloud = null then

               Load_Mesh (Nodes);

            else

               Cloud.Clear;

               declare

                  Point : Math.Vector2.Vector2_Access;

               begin

                  Vertex := Vertices.Get_First_Item;

                  while Vertex /= null and not Abort_Meshing loop

                     Cloud.Add_Item (Point);

                     Point.Set (Vertex.Get_X, Vertex.Get_Y);

                     Point.Set_Id (Point_Identifier (Cloud.Get_Count));

                     Vertices.Get_Next_Item (Vertex);

                  end loop;

               end;

               Load_Mesh (Cloud);

            end if;

         end if;

         if Publish_Progress then

            if Abort_Meshing then

               Set_Status_Message ("Meshing aborted due to inconsistent geometry.");

            else

               Set_Status_Message (Natural'Image (Mesh.Get_Count) & " triangles generated.");

            end if;

            Publish_Done;

         end if;

         Polygon := null;

         Triangles.Clear;

         Segments.Clear;

         Vertices.Clear;

      else

         Abort_Meshing := True;

         Set_Status_Message ("Invalid stacks passed. Meshing aborted.");

      end if;

   exception

      when others =>

         Polygon := null;

         Triangles.Clear;

         Segments.Clear;

         Vertices.Clear;

         Set_Status_Message ("Mesher crashed.");

         Publish_Done;

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   procedure Load_Nodes is

      use type Math.Vector2.Vector2_Access;

      Node : Math.Vector2.Vector2_Access := null;

      Vertex : Math.Vector2.Vector2_Access := null;

      X_Min, X_Max, Y_Min, Y_Max : Long_Float;

      Dx, Dy : Long_Float;

      Counter : Positive := 1;

   begin

      if Verbosity_Level > 2 then

         Ada.Text_IO.Put_Line ("Normalizing coordinates...");

      end if;

      Node := Polygon.Get_First_Item;

      X_Min := Node.Get_X;

      X_Max := Node.Get_X;

      Y_Min := Node.Get_Y;

      Y_Max := Node.Get_Y;

      while Node /= null loop

         Node.Set_Id (Point_Identifier (Counter));

         X_Min := Long_Float'Min (X_Min, Node.Get_X);

         X_Max := Long_Float'Max (X_Max, Node.Get_X);

         Y_Min := Long_Float'Min (Y_Min, Node.Get_Y);

         Y_Max := Long_Float'Max (Y_Max, Node.Get_Y);

         Polygon.Get_Next_Item (Node);

         Counter := Counter + 1;

      end loop;

      Dx := Long_Float (X_Max - X_Min);

      Dy := Long_Float (Y_Max - Y_Min);

      Scale := Long_Float'Max (Dx, Dy);

      if Scale > 0.0 then

         Center.Set (Long_Float (X_Min) + 0.5 * Dx, Long_Float (Y_Min) + 0.5 * Dy);

         Node := Polygon.Get_First_Item;

         while Node /= null loop

            Vertices.Add_Item (Vertex);

            Vertex.Set ((Long_Float (Node.Get_X) - Center.Get_X) / Scale, (Long_Float (Node.Get_Y) - Center.Get_Y) / Scale);

            Polygon.Get_Next_Item (Node);

         end loop;

      end if;

   end;

   -----------------------------------------------------------------------------
   -- Loads the segments using the current vertex stack
   -----------------------------------------------------------------------------
   procedure Load_Segments is

      Point_A, Point_B : Vector2_Access;

      Segment : Segment_Access;

   begin

      if Verbosity_Level > 2 then

         Ada.Text_IO.Put_Line ("Adding segments...");

      end if;

      Segments.Clear;

      Point_B := Vertices.Get_First_Item;

      while Point_B /= null loop

         if Point_A /= null then

            Segments.Add_Item (Segment);

            Segment.Set_Vertices (Point_A, Point_B);

         end if;

         Point_A := Point_B;

         Vertices.Get_Next_Item (Point_B);

      end loop;

      Point_A := Vertices.Get_Last_Item;

      Point_B := Vertices.Get_First_Item;

      Segments.Add_Item (Segment);

      Segment.Set_Vertices (Point_A, Point_B);

   end;

   -----------------------------------------------------------------------------
   -- Removes nodes too close to eachoter (and associated segments)
   -----------------------------------------------------------------------------
   procedure Merge_Points (Offset : Long_Float) is

      Point_A, Point_B : Vector2_Access;

      Segment : Segment_Access;

   begin

      if Verbosity_Level > 2 then

         Ada.Text_IO.Put_Line ("Merging points...");

      end if;

      Point_A := Vertices.Get_First_Item;

      while Point_A /= null loop

         Point_B := Point_A;

         Vertices.Get_Next_Item (Point_B);

         while Point_B /= null loop

            if (Point_A /= Point_B) and then Point_A.Distance (Point_B.all) < Offset then

               Segment := Segments.Get_First_Item;

               while Segment /= null loop

                  if
                    (Segment.Get_Vertex_A = Point_A and Segment.Get_Vertex_B = Point_B) or
                    (Segment.Get_Vertex_A = Point_B and Segment.Get_Vertex_B = Point_A)
                  then

                     declare

                        Old_Segment : Segment_Access := Segment;

                     begin

                        Segments.Get_Next_Item (Segment);

                        Segments.Remove_Item (Old_Segment);

                     end;

                  elsif
                    Segment.Get_Vertex_A = Point_B
                  then

                     Segment.Set_Vertices (Point_A, Segment.Get_Vertex_B);

                     Segments.Get_Next_Item (Segment);

                  elsif
                    Segment.Get_Vertex_B = Point_B
                  then

                     Segment.Set_Vertices (Segment.Get_Vertex_A, Point_A);

                     Segments.Get_Next_Item (Segment);

                  else

                     Segments.Get_Next_Item (Segment);

                  end if;

               end loop;

               declare

                  Old_Point : Vector2_Access := Point_B;

               begin

                  Vertices.Get_Next_Item (Point_B);

                  Vertices.Remove_Item (Old_Point);

               end;

            else

               Vertices.Get_Next_Item (Point_B);

            end if;

         end loop;

         Vertices.Get_Next_Item (Point_A);

      end loop;

   end;

   -----------------------------------------------------------------------------
   -- Breaks segments pierced by a node (without adding any node)
   -----------------------------------------------------------------------------
   procedure Split_Pierced_Segments (Offset : Long_Float) is

      Point : Vector2_Access;

      Segment, New_Segment : Segment_Access;

      Count, Limit : Natural;

      Offset_2 : Long_Float := 2.0 * Offset;

   begin

      if Verbosity_Level > 2 then

         Ada.Text_IO.Put_Line ("Splitting pierced segments...");

      end if;

      Count := 0;

      Limit := Segments.Get_Count * Vertices.Get_Count;

      Segment := Segments.Get_First_Item;

      while Segment /= null loop

         if Segment.Get_Length > Offset_2 then

            Point := Vertices.Get_First_Item;

            while Point /= null and Count < Limit loop

               if (Segment.Get_Vertex_A /= Point and Segment.Get_Vertex_B /= Point) and then Segment.Contains (Point, Offset) then

                  Count := Count + 1;

                  Segments.Add_Item (New_Segment);

                  New_Segment.Set_Vertices (Point, Segment.Get_Vertex_B);

                  Segment.Set_Vertices (Segment.Get_Vertex_A, Point);

               end if;

               Vertices.Get_Next_Item (Point);

            end loop;

         end if;

         Segments.Get_Next_Item (Segment);

      end loop;

      if Count = Limit then

         if Verbosity_Level > 0 then

           Ada.Text_IO.Put_Line ("Error: the segment splitting algorithm could not resolve the geometry");

         end if;

         Abort_Meshing := True;

      end if;

   end;

   -----------------------------------------------------------------------------
   -- Removes segments having same extreme nodes
   -----------------------------------------------------------------------------
   procedure Remove_Duplicated_Segments is

      Segment_A, Segment_B, Old_Segment : Segment_Access;

   begin

      if Verbosity_Level > 2 then

         Ada.Text_IO.Put_Line ("Removing duplicated segments...");

      end if;

      Segment_A := Segments.Get_First_Item;

      while Segment_A /= null loop

         Segment_B := Segments.Get_First_Item;

         while Segment_B /= null loop

            if Segment_A /= Segment_B then

               if
                 (Segment_A.Get_Vertex_A = Segment_B.Get_Vertex_A and Segment_A.Get_Vertex_B = Segment_B.Get_Vertex_B) or
                 (Segment_A.Get_Vertex_A = Segment_B.Get_Vertex_B and Segment_A.Get_Vertex_B = Segment_B.Get_Vertex_A)
               then

                  if Verbosity_Level > 1 then

                     Ada.Text_IO.Put_Line ("Warning: duplicated segment removed");

                     Segment_B.Publish;

                  end if;

                  Old_Segment := Segment_B;

                  Segments.Get_Next_Item (Segment_B);

                  Segments.Remove_Item (Old_Segment);

               else

                  Segments.Get_Next_Item (Segment_B);

               end if;

            else

               Segments.Get_Next_Item (Segment_B);

            end if;

         end loop;

         Segments.Get_Next_Item (Segment_A);

      end loop;

   end;

   -----------------------------------------------------------------------------
   -- Assignes in Id to each vertex
   -----------------------------------------------------------------------------
   procedure Indexate_Vertices is

      Vertex : Vector2_Access;

      Counter : Positive := 1;

   begin

      Vertex := Vertices.Get_First_Item;

      while Vertex /= null loop

         Vertex.Set_Id (Point_Identifier (Counter));

         Counter := Counter + 1;

         Vertices.Get_Next_Item (Vertex);

      end loop;

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   procedure Generate_Boundary_1 is
   begin

      --------------------------------------------------------------------------
      -- A) Build the boundary
      --------------------------------------------------------------------------

      Load_Segments;

      --------------------------------------------------------------------------
      -- B) Split pierced segments
      --------------------------------------------------------------------------

      Split_Pierced_Segments (Proximity_Threshold);

      --------------------------------------------------------------------------
      -- C) Remove doubled segment
      --------------------------------------------------------------------------

      Remove_Duplicated_Segments;

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   procedure Generate_Boundary_2 is

      Vertex : Vector2_Access;

      Segment, New_Segment, Other_Segment : Segment_Access;

      Intersected, Changed : Boolean;

      Intersection : Vector2_Record;

      Count : Natural := 0;

      Limit : Natural := 0;

   begin

      --------------------------------------------------------------------------
      -- This algorithm will do a very good effort to simplify geometric details at very
      -- low scale, so that nodes and segments mantain a minimum spacing,
      -- sufficiently higher than the proximity threshold. This will avoid
      -- inconsistent situations during the meshing.
      -- However, the resulting plannar graph is not guranteed to be meshable,
      -- and under some complex configurations the mesher could be forced to abort.
      --------------------------------------------------------------------------

      -----------------------------------------------------------------------
      -- A) Load the boundary segments based on the current nodes
      -----------------------------------------------------------------------

      Load_Segments;

      -----------------------------------------------------------------------
      -- B) Remove nodes too close to eachoter (and associated segments)
      -----------------------------------------------------------------------

      Merge_Points (Fusion_Threshold);

      -----------------------------------------------------------------------
      -- C) Split segments pierced by a node (not recursive)
      -----------------------------------------------------------------------

      Split_Pierced_Segments (Fusion_Threshold);

      -----------------------------------------------------------------------
      -- D) Split segments crossed by another segment
      -----------------------------------------------------------------------

      if Verbosity_Level > 2 then

         Ada.Text_IO.Put_Line ("Adding crossing points...");

      end if;

      Limit := Segments.Get_Count * Segments.Get_Count;

      Changed := False;

      Segment := Segments.Get_First_Item;

      while Segment /= null and Count < Limit loop

         Count := Count + 1;

         Other_Segment := Segments.Get_First_Item;

         while Other_Segment /= null loop

            if
              (Segment.Get_Vertex_A = Other_Segment.Get_Vertex_A) or else
              (Segment.Get_Vertex_A = Other_Segment.Get_Vertex_B) or else
              (Segment.Get_Vertex_B = Other_Segment.Get_Vertex_A) or else
              (Segment.Get_Vertex_B = Other_Segment.Get_Vertex_B)
            then

               null;

            else

               if Verbosity_Level > 2 then

                  Ada.Text_IO.Put_Line ("CHECKING BREAK");

                  Ada.Text_IO.Put_Line ("Segment 1 -> ");

                  Segment.Publish;

                  Ada.Text_IO.Put_Line ("Segment 2 -> ");

                  Other_Segment.Publish;

               end if;

               Segment.Intersection (Other_Segment, Intersected, Intersection);

               if Intersected then

                  Vertex := Vertices.Get_First_Item;

                  -----------------------------------------------------------
                  -- Check if there is a point in the vecinity of the
                  -- intersection
                  -----------------------------------------------------------

                  while Vertex /= null loop

                     if Vertex.Distance (Intersection) < Fusion_Threshold then

                        exit;

                     else

                        Vertices.Get_Next_Item (Vertex);

                     end if;

                  end loop;

                  -----------------------------------------------------------
                  -- Add a new point if there is nothing in the neigbourhood
                  -----------------------------------------------------------

                  if Vertex = null then

                     Vertices.Add_Item (Vertex);

                     Vertex.Set (Intersection.Get_X, Intersection.Get_Y);

                     if Verbosity_Level > 2 then

                        Ada.Text_IO.Put_Line ("Intersection at -> ");

                        Vertex.Publish;

                     end if;

                  end if;

                  -----------------------------------------------------------
                  -- Split the segment, and add a new segment
                  -----------------------------------------------------------

                  if Vertex /= Segment.Get_Vertex_A and Vertex /= Segment.Get_Vertex_B then

                     if Verbosity_Level > 2 then

                        Ada.Text_IO.Put_Line ("Breaking...");

                     end if;

                     Segments.Add_Item (New_Segment);

                     New_Segment.Set_Vertices (Vertex, Segment.Get_Vertex_B);

                     Segment.Set_Vertices (Segment.Get_Vertex_A, Vertex);

                     if Verbosity_Level > 2 then

                        Segment.Publish;

                        New_Segment.Publish;

                     end if;

                     Changed := True;

                  end if;

                  if Vertex /= Other_Segment.Get_Vertex_A and Vertex /= Other_Segment.Get_Vertex_B then

                     if Verbosity_Level > 2 then

                        Ada.Text_IO.Put_Line ("Breaking ...");

                     end if;

                     Segments.Add_Item (New_Segment);

                     New_Segment.Set_Vertices (Vertex, Other_Segment.Get_Vertex_B);

                     Other_Segment.Set_Vertices (Other_Segment.Get_Vertex_A, Vertex);

                     if Verbosity_Level > 2 then

                        Other_Segment.Publish;

                        New_Segment.Publish;

                     end if;

                     Changed := True;

                  end if;

               end if;

            end if;

            Segments.Get_Next_Item (Other_Segment);

         end loop;

         Segments.Get_Next_Item (Segment);

      end loop;

      if Count = Limit then

         if Verbosity_Level > 0 then

            Ada.Text_IO.Put_Line ("Error: blocked segment intersection loop");

         end if;

         Abort_Meshing := True;

         return;

      end if;

      -----------------------------------------------------------------------
      -- E) Split segments pierced by new intersections (not recursive)
      -----------------------------------------------------------------------

      if Changed then

         Split_Pierced_Segments (Fusion_Threshold);

      end if;

      -----------------------------------------------------------------------
      -- F) Remove duplicated segments
      -----------------------------------------------------------------------

      Remove_Duplicated_Segments;

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   procedure Generate_Starting_Triangle is

      X_Min, Y_Min, X_Max, Y_Max : Long_Float;

      Width, Height, Offset, Delta_X, Delta_Y : Long_Float;

      Vertex : Vector2_Access;

      Starting_Triangle : Triangle_Access;

   begin

      Vertex := Vertices.Get_First_Item;

      if Vertex /= null then

         X_Min := Vertex.Get_X;

         X_Max := Vertex.Get_X;

         Y_Min := Vertex.Get_Y;

         Y_Max := Vertex.Get_Y;

      end if;

      while Vertex /= null loop

         if Vertex.Get_X > X_Max then

            X_Max := Vertex.Get_X;

         end if;

         if Vertex.Get_Y > Y_Max then

            Y_Max := Vertex.Get_Y;

         end if;

         if Vertex.Get_X < X_Min then

            X_Min := Vertex.Get_X;

         end if;

         if Vertex.Get_Y < Y_Min then

            Y_Min := Vertex.Get_Y;

         end if;

         Vertices.Get_Next_Item (Vertex);

      end loop;

      Width  := X_Max - X_Min;

      Height := Y_Max - Y_Min;

      if Width > 0.0 and Height > 0.0 then

         Offset := 0.1 * Long_Float'Max (Height, Width);

         Delta_X := Offset / Ada.Numerics.Long_Elementary_Functions.Sqrt (Width * Width / Height / Height + 1.0) + Offset * Width / Height;

         Delta_Y := Offset / Ada.Numerics.Long_Elementary_Functions.Sqrt (Height * Height / Width / Width + 1.0) + Offset * Height / Width;

         Point_1.Set (X_Min - Offset, Y_Min - Offset);

         Point_2.Set (2.0 * X_Max - X_Min + Offset + Delta_X, Y_Min - Offset);

         Point_3.Set (X_Min - Offset, 2.0 * Y_Max - Y_Min + Offset + Delta_Y);

         Triangles.Add_Item (Starting_Triangle);

         Starting_Triangle.Set_Vertices (Point_1, Point_2, Point_3);

      else

         if Verbosity_Level > 0 then

            Ada.Text_IO.Put_Line ("Error: could not generate the starting triangle");

         end if;

         Abort_Meshing := True;

      end if;

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   procedure Insert_Vertex (Vertex_To_Add : Vector2_Access) is

      Host_Triangle : Triangle_Access;

      Location_In_Host : Point_Location_Kinds := Outside;

      Adjacent_Triangle : Triangle_Access;

      Side_In_Adjacent : Triangle_Side_Kinds := AB_Side;

      Revision_Pool : Triangle_Vector.Vector;

      Triangle_1, Triangle_2 : Triangle_Access;

   begin

      if Meshing.Verbosity_Level > 2 then

         Ada.Text_IO.Put_Line ("Vertex insertion -> ");

         Vertex_To_Add.Publish;

      end if;

      Host_Triangle := Triangles.Get_First_Item;

      while Host_Triangle /= null and Location_In_Host = Outside loop

         Location_In_Host := Host_Triangle.Locate_Point (Vertex_To_Add);

         if Location_In_Host /= Outside then

            if Meshing.Verbosity_Level > 2 then

               Ada.Text_IO.Put_Line ("Host found:");

               Host_Triangle.Publish;

               Ada.Text_IO.Put_Line (Point_Location_Kinds'Image (Location_In_Host));

            end if;

         else

            Triangles.Get_Next_Item (Host_Triangle);

         end if;

      end loop;

      if Host_Triangle /= null then

         case Location_In_Host is

            when Inside =>

               -----------------------------------------------------------------
               -- Split the host triangle in three
               -----------------------------------------------------------------

               --------------------------------------------------------------
               -- Add new triangle joining BCP
               --------------------------------------------------------------

               Triangles.Add_Item (Triangle_1);

               Triangle_1.Set_Vertices (Host_Triangle.Get_Vertex_B,
                                        Host_Triangle.Get_Vertex_C,
                                        Vertex_To_Add);

               Revision_Pool.Append (Triangle_1);

               --------------------------------------------------------------
               -- Add new triangle joining CAP
               --------------------------------------------------------------

               Triangles.Add_Item (Triangle_2);

               Triangle_2.Set_Vertices (Host_Triangle.Get_Vertex_C,
                                        Host_Triangle.Get_Vertex_A,
                                        Vertex_To_Add);

               Revision_Pool.Append (Triangle_2);

               --------------------------------------------------------------
               -- Redefine the host triangle as ABP
               --------------------------------------------------------------

               Host_Triangle.Set_Vertices (Host_Triangle.Get_Vertex_A,
                                           Host_Triangle.Get_Vertex_B,
                                           Vertex_To_Add);

               Revision_Pool.Append (Host_Triangle);

            when Close_To_AB =>

               --------------------------------------------------------------
               -- Find the neighbour to AB
               --------------------------------------------------------------

               Get_Adjacent_Triangle (Host_Triangle, AB_Side, Adjacent_Triangle, Side_In_Adjacent);

               --------------------------------------------------------------
               -- Split both in two
               --------------------------------------------------------------

               Split_In_Two (Vertex_To_Add, Host_Triangle, AB_Side, Triangle_1);

               Revision_Pool.Append (Host_Triangle);

               Revision_Pool.Append (Triangle_1);

               if Adjacent_Triangle /= null then

                  Split_In_Two (Vertex_To_Add, Adjacent_Triangle, Side_In_Adjacent, Triangle_2);

                  Revision_Pool.Append (Adjacent_Triangle);

                  Revision_Pool.Append (Triangle_2);

               end if;

            when Close_To_BC =>

               --------------------------------------------------------------
               -- Find the neighbour to BC
               --------------------------------------------------------------

               Get_Adjacent_Triangle (Host_Triangle, BC_Side, Adjacent_Triangle, Side_In_Adjacent);

               --------------------------------------------------------------
               -- Split both in two
               --------------------------------------------------------------

               Split_In_Two (Vertex_To_Add, Host_Triangle, BC_Side, Triangle_1);

               Revision_Pool.Append (Host_Triangle);

               Revision_Pool.Append (Triangle_1);

               if Adjacent_Triangle /= null then

                  Split_In_Two (Vertex_To_Add, Adjacent_Triangle, Side_In_Adjacent, Triangle_2);

                  Revision_Pool.Append (Adjacent_Triangle);

                  Revision_Pool.Append (Triangle_2);

               end if;

            when Close_To_CA =>

               --------------------------------------------------------------
               -- Find the neighbour to CA
               --------------------------------------------------------------

               Get_Adjacent_Triangle (Host_Triangle, CA_Side, Adjacent_Triangle, Side_In_Adjacent);

               --------------------------------------------------------------
               -- Split both in two
               --------------------------------------------------------------

               Split_In_Two (Vertex_To_Add, Host_Triangle, CA_Side, Triangle_1);

               Revision_Pool.Append (Host_Triangle);

               Revision_Pool.Append (Triangle_1);

               if Adjacent_Triangle /= null then

                  Split_In_Two (Vertex_To_Add, Adjacent_Triangle, Side_In_Adjacent, Triangle_2);

                  Revision_Pool.Append (Adjacent_Triangle);

                  Revision_Pool.Append (Triangle_2);

               end if;

            when others =>

               -- Note that "Outside" has already been rouled out

               null;

         end case;

         --------------------------------------------------------------------------
         -- Swap sides of non-Delaynay triangles
         --------------------------------------------------------------------------

         declare

            Pooled_Triangle : Triangle_Access;

            Side_In_Pooled  : Triangle_Side_Kinds;

            Cointains_Vertex_To_Add : Boolean;

            Vertex_1, Vertex_2, Vertex_3 : Vector2_Access;

            Count : Natural := 0;

            Limit : constant Natural := Triangles.Get_Count * Triangles.Get_Count;

         begin

            while not Revision_Pool.Is_Empty and Count < Limit loop

               Count := Count + 1;

               Pooled_Triangle := Revision_Pool.First_Element;

               Revision_Pool.Delete_First;

               Cointains_Vertex_To_Add := True;

               if Pooled_Triangle.Get_Vertex_A = Vertex_To_Add then

                  Vertex_1 := Pooled_Triangle.Get_Vertex_B;

                  Vertex_2 := Pooled_Triangle.Get_Vertex_C;

                  Side_In_Pooled := BC_Side;

               elsif Pooled_Triangle.Get_Vertex_B = Vertex_To_Add then

                  Vertex_1 := Pooled_Triangle.Get_Vertex_C;

                  Vertex_2 := Pooled_Triangle.Get_Vertex_A;

                  Side_In_Pooled := CA_Side;

               elsif Pooled_Triangle.Get_Vertex_C = Vertex_To_Add then

                  Vertex_1 := Pooled_Triangle.Get_Vertex_A;

                  Vertex_2 := Pooled_Triangle.Get_Vertex_B;

                  Side_In_Pooled := AB_Side;

               else

                  Cointains_Vertex_To_Add := false;

                  if Verbosity_Level > 1 then

                     Ada.Text_IO.Put_Line ("Warning: the revision pool contains a wrong triangle");

                  end if;

               end if;

               if Cointains_Vertex_To_Add and then not Is_At_Boundary (Vertex_1, Vertex_2) then

                  Get_Adjacent_Triangle (Pooled_Triangle, Side_In_Pooled, Adjacent_Triangle, Side_In_Adjacent);

                  if Adjacent_Triangle /= null and then Adjacent_Triangle.In_Circumcircle (Vertex_To_Add) then

                     case Side_In_Adjacent is

                     when AB_Side =>

                        Vertex_3 := Adjacent_Triangle.Get_Vertex_C;

                     when BC_Side =>

                        Vertex_3 := Adjacent_Triangle.Get_Vertex_A;

                     when CA_Side =>

                        Vertex_3 := Adjacent_Triangle.Get_Vertex_B;

                     end case;

                     if Verbosity_Level > 3 then

                        Ada.Text_IO.Put_Line ("Swapping...");

                        Ada.Text_IO.Put_Line ("Pooled:");

                        Pooled_Triangle.Publish;

                        Ada.Text_IO.Put_Line ("Adjacent:");

                        Adjacent_Triangle.Publish;

                     end if;

                     --------------------------------------------------------------
                     -- Perform the swapping
                     --------------------------------------------------------------

                     Pooled_Triangle.Set_Vertices (Vertex_To_Add, Vertex_1, Vertex_3);

                     Adjacent_Triangle.Set_Vertices (Vertex_To_Add, Vertex_2, Vertex_3);

                     --------------------------------------------------------------
                     -- Add both triangles for further revision
                     --------------------------------------------------------------

                     Revision_Pool.Append (Pooled_Triangle);

                     Revision_Pool.Append (Adjacent_Triangle);

                  end if;

               end if;

            end loop;

            if Count = Limit then

               Revision_Pool.Clear;

               if Verbosity_Level > 0 then

                  Ada.Text_IO.Put_Line ("Error: the side swapping has been blocked");

               end if;

               Meshing.Abort_Meshing := True;

            end if;

         end;

      else

         if Verbosity_Level > 1 then

            Ada.Text_IO.Put_Line ("Error: host triangle not found");

            Vertex_To_Add.Publish;

         end if;

         Abort_Meshing := True;

      end if;

   exception

      when E : others =>

         Abort_Meshing := True;

         Ada.Text_IO.Put_Line ("the mesher crashed while inserting vertices");

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   procedure Split_In_Two (Piercing_Vertex   : Vector2_Access;
                           Triangle_To_Split : Triangle_Access;
                           Side              : Triangle_Side_Kinds;
                           New_Triangle      : out Triangle_Access)
   is

   begin

      case Side is

         when AB_Side =>

            --------------------------------------------------------------
            -- Add new triangle joining BCP:
            --------------------------------------------------------------

            Triangles.Add_Item (New_Triangle);

            New_Triangle.Set_Vertices (Triangle_To_Split.Get_Vertex_B,
                                       Triangle_To_Split.Get_Vertex_C,
                                       Piercing_Vertex);

            --------------------------------------------------------------
            -- Redefine the host triangle as CAP
            --------------------------------------------------------------

            Triangle_To_Split.Set_Vertices (Triangle_To_Split.Get_Vertex_C,
                                            Triangle_To_Split.Get_Vertex_A,
                                            Piercing_Vertex);

         when BC_Side =>

            --------------------------------------------------------------
            -- Add new triangle joining ABP:
            --------------------------------------------------------------

            Triangles.Add_Item (New_Triangle);

            New_Triangle.Set_Vertices (Triangle_To_Split.Get_Vertex_A,
                                       Triangle_To_Split.Get_Vertex_B,
                                       Piercing_Vertex);

            --------------------------------------------------------------
            -- Redefine the host triangle as CAP
            --------------------------------------------------------------

            Triangle_To_Split.Set_Vertices (Triangle_To_Split.Get_Vertex_C,
                                            Triangle_To_Split.Get_Vertex_A,
                                            Piercing_Vertex);

         when CA_Side =>

            --------------------------------------------------------------
            -- Add new triangle joining ABP:
            --------------------------------------------------------------

            Triangles.Add_Item (New_Triangle);

            New_Triangle.Set_Vertices (Triangle_To_Split.Get_Vertex_A,
                                       Triangle_To_Split.Get_Vertex_B,
                                       Piercing_Vertex);

            --------------------------------------------------------------
            -- Redefine the host triangle as CAP
            --------------------------------------------------------------

            Triangle_To_Split.Set_Vertices (Triangle_To_Split.Get_Vertex_B,
                                            Triangle_To_Split.Get_Vertex_C,
                                            Piercing_Vertex);

      end case;

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   procedure Get_Adjacent_Triangle (Host          : Triangle_Access;
                                    Host_Side     : Triangle_Side_Kinds;
                                    Adjacent      : out Triangle_Access;
                                    Adjacent_Side : out Triangle_Side_Kinds) is

      Vertex_1, Vertex_2, Vertex_3 : Vector2_Access;

      Triangle : Triangle_Access;

   begin

      Adjacent := null;

      Adjacent_Side := AB_Side;

      case Host_Side is

         when AB_Side =>

            Vertex_1 := Host.Get_Vertex_A;

            Vertex_2 := Host.Get_Vertex_B;

            Vertex_3 := Host.Get_Vertex_C;

         when BC_Side =>

            Vertex_1 := Host.Get_Vertex_B;

            Vertex_2 := Host.Get_Vertex_C;

            Vertex_3 := Host.Get_Vertex_A;

         when CA_Side =>

            Vertex_1 := Host.Get_Vertex_C;

            Vertex_2 := Host.Get_Vertex_A;

            Vertex_3 := Host.Get_Vertex_B;

         when others =>

            -- (Outside and inside must have been rouled out)

            null;

      end case;

      Triangle := Triangles.Get_First_Item;

      while Triangle /= null loop

         if Triangle /= Host then

            if ((Triangle.Get_Vertex_A = Vertex_1 and Triangle.Get_Vertex_B = Vertex_2) or
                (Triangle.Get_Vertex_B = Vertex_1 and Triangle.Get_Vertex_A = Vertex_2)) and
                (Triangle.Get_Vertex_C /= Vertex_3)
            then

               Adjacent := Triangle;

               Adjacent_Side := AB_Side;

               exit;

            end if;

            if ((Triangle.Get_Vertex_B = Vertex_1 and Triangle.Get_Vertex_C = Vertex_2) or
                (Triangle.Get_Vertex_C = Vertex_1 and Triangle.Get_Vertex_B = Vertex_2)) and
                (Triangle.Get_Vertex_A /= Vertex_3)
            then

               Adjacent := Triangle;

               Adjacent_Side := BC_Side;

               exit;

            end if;

            if ((Triangle.Get_Vertex_C = Vertex_1 and Triangle.Get_Vertex_A = Vertex_2) or
                (Triangle.Get_Vertex_A = Vertex_1 and Triangle.Get_Vertex_C = Vertex_2)) and
                (Triangle.Get_Vertex_B /= Vertex_3)
            then

               Adjacent := Triangle;

               Adjacent_Side := CA_Side;

               exit;

            end if;

         end if;

         Triangles.Get_Next_Item (Triangle);

      end loop;

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   function Segment_Is_Missing (Segment : Segment_Access) return Boolean is

      Triangle : Triangle_Access;

   begin

      Triangle := Triangles.Get_First_Item;

      while Triangle /= null loop

         if
           (Triangle.Get_Vertex_A = Segment.Get_Vertex_A and Triangle.Get_Vertex_B = Segment.Get_Vertex_B) or
           (Triangle.Get_Vertex_A = Segment.Get_Vertex_B and Triangle.Get_Vertex_B = Segment.Get_Vertex_A) or

           (Triangle.Get_Vertex_B = Segment.Get_Vertex_A and Triangle.Get_Vertex_C = Segment.Get_Vertex_B) or
           (Triangle.Get_Vertex_B = Segment.Get_Vertex_B and Triangle.Get_Vertex_C = Segment.Get_Vertex_A) or

           (Triangle.Get_Vertex_C = Segment.Get_Vertex_A and Triangle.Get_Vertex_A = Segment.Get_Vertex_B) or
           (Triangle.Get_Vertex_C = Segment.Get_Vertex_B and Triangle.Get_Vertex_A = Segment.Get_Vertex_A)
         then

            -- At least one side of one triangle is covering this segment.

            return False;

         end if;

         Triangles.Get_Next_Item (Triangle);

      end loop;

      return True;

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   procedure Fix_Segment (Segment : Segment_Access) is

      Triangle : Triangle_Access := null;

      Adjacent : Triangle_Access := null;

      Crossed : Boolean;

      Crossing_Side, Adjacent_Side : Triangle_Side_Kinds;

      Starting_Vertex : Vector2_Access := null;

      Final_Vertex : Vector2_Access := null;

      Bad_Triangles : Triangle_Vector.Vector;

      Reached_End : Boolean := False;

      Upper_Path : Math.Vector2_Vector.Vector;

      Lower_Path : Math.Vector2_Vector.Vector;

      Count, Limit : Natural;

   begin

      --------------------------------------------------------------------------
      -- Modifies the mesh until the segment fits in the triangulation
      --------------------------------------------------------------------------

      Crossed := False;

      Triangle := Triangles.Get_First_Item;

      while Triangle /= null and not Crossed loop

         Triangle.Crosses_Segment (Segment, Crossed, Crossing_Side, Starting_Vertex);

         if Crossed then

            Bad_Triangles.Append (Triangle);

            if Starting_Vertex = Segment.Get_Vertex_A then

               Final_Vertex := Segment.Get_Vertex_B;

            else

               Final_Vertex := Segment.Get_Vertex_A;

            end if;

            Get_Adjacent_Triangle (Triangle, Crossing_Side, Adjacent, Adjacent_Side);

         else

            Triangles.Get_Next_Item (Triangle);

         end if;

      end loop;

      if not Crossed then

         if Verbosity_Level > 0 then

            Ada.Text_IO.Put_Line ("Error: a boundary segment is not crossed by any triangle");

            Segment.Publish;

         end if;

         Abort_Meshing := True;

         return;

      end if;

      --------------------------------------------------------------------------
      -- Follow the propagation edge up to the other side of the segment,
      -- collecting all of the triangles crossing the way.
      --------------------------------------------------------------------------

      Count := 0;

      Limit := Triangles.Get_Count;

      while
        Adjacent /= null and
        not Reached_End and
        Count < Limit
      loop

         Count := Count + 1;

         Bad_Triangles.Append (Adjacent);

         declare

            --------------------------------------------------------------------
            -- Note: Vertex_3 is always the oposite vertex to the adjacent side.
            --------------------------------------------------------------------

            Vertex_1, Vertex_2, Vertex_3 : Vector2_Access;

            Direction_To_Vertex_1 : Vector2_Record;

            Side_13, Side_23 : Triangle_Side_Kinds;

         begin

            case Adjacent_Side is

               when AB_Side =>

                  Vertex_1 := Adjacent.Get_Vertex_A;

                  Vertex_2 := Adjacent.Get_Vertex_B;

                  Vertex_3 := Adjacent.Get_Vertex_C;

                  Side_13 := CA_Side;

                  Side_23 := BC_Side;

               when BC_Side =>

                  Vertex_1 := Adjacent.Get_Vertex_B;

                  Vertex_2 := Adjacent.Get_Vertex_C;

                  Vertex_3 := Adjacent.Get_Vertex_A;

                  Side_13 := AB_Side;

                  Side_23 := CA_Side;

               when CA_Side =>

                  Vertex_1 := Adjacent.Get_Vertex_C;

                  Vertex_2 := Adjacent.Get_Vertex_A;

                  Vertex_3 := Adjacent.Get_Vertex_B;

                  Side_13 := BC_Side;

                  Side_23 := AB_Side;

            end case;

            Direction_To_Vertex_1.Set_X (Vertex_1.Get_X - Segment.Get_Vertex_A.Get_X);

            Direction_To_Vertex_1.Set_Y (Vertex_1.Get_Y - Segment.Get_Vertex_A.Get_Y);

            if Segment.Get_Direction.Cross_Product (Direction_To_Vertex_1) > 0.0 then

               if not Upper_Path.Contains (Vertex_1) then

                  Upper_Path.Append (Vertex_1);

               end if;

               if not Lower_Path.Contains (Vertex_2) then

                  Lower_Path.Append (Vertex_2);

               end if;

            else

               if not Upper_Path.Contains (Vertex_2) then

                  Upper_Path.Append (Vertex_2);

               end if;

               if not Lower_Path.Contains (Vertex_1) then

                  Lower_Path.Append (Vertex_1);

               end if;

            end if;

            if Vertex_3 = Final_Vertex then

               --
               -- Whe have reached the other side of the segment. We can stop
               -- searching.
               --

               Reached_End := true;

            else

               Triangle := Adjacent;

               if Segment.Crosses (Vertex_1, Vertex_3) then

                  Get_Adjacent_Triangle (Triangle, Side_13, Adjacent, Adjacent_Side);

               elsif Segment.Crosses (Vertex_2, Vertex_3) then

                  Get_Adjacent_Triangle (Triangle, Side_23, Adjacent, Adjacent_Side);

               else

                  if Verbosity_Level > 0 then

                     Ada.Text_IO.Put_Line ("Error: no crossing found in propagation path");

                     Segment.Publish;

                  end if;

                  Abort_Meshing := True;

                  return;

               end if;

            end if;

         end;

      end loop;

      if not Reached_End then

         if Verbosity_Level > 0 then

            Ada.Text_IO.Put_Line ("Error: could not reach the end of a segment");

            if Count = Limit then

               Ada.Text_IO.Put_Line (" after reaching the limit");

            end if;

            Segment.Publish;

         end if;

         Abort_Meshing := True;

         return;

      end if;

      -----------------------------------------------------------------------
      -- Remove the bad triangles
      -----------------------------------------------------------------------

      for i in Bad_Triangles.First_Index .. Bad_Triangles.Last_Index loop

         Triangle := Bad_Triangles.Element (i);

         Triangles.Remove_Item (Triangle);

      end loop;

      Bad_Triangles.Clear;

      -----------------------------------------------------------------------
      -- Generate upper triangulations
      -----------------------------------------------------------------------

      Recursion_Level := 0;

      Recursion_Limit := Natural (Upper_Path.Length);

      Complete_Pseudo_Polygon (Upper_Path, Starting_Vertex, Final_Vertex);

      -----------------------------------------------------------------------
      -- Generate lower triangulations
      -----------------------------------------------------------------------

      Recursion_Level := 0;

      Recursion_Limit := Natural (Lower_Path.Length);

      Complete_Pseudo_Polygon (Lower_Path, Starting_Vertex, Final_Vertex);

   exception

      when E : others =>

         Abort_Meshing := True;

         Ada.Text_IO.Put_Line ("the mesher crashed while fixing a boundary segment");

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   procedure Complete_Pseudo_Polygon (Polygon : in out Math.Vector2_Vector.Vector; Vertex_A, Vertex_B : Vector2_Access) is

      Triangle : Triangle_Access;

      Vertex : Vector2_Access;

      Split : Boolean;

      Split_Index : Positive := 1;

   begin

      --------------------------------------------------------------------------
      -- Note: normally the recursion end is inherent to the algorithm, but for
      -- a safety reason, we count the level of recursion and force a stop if
      -- reached the absolute limit.
      --------------------------------------------------------------------------

      Recursion_Level := Recursion_Level + 1;

      if Recursion_Level > Recursion_Limit then

         if Verbosity_Level > 0 then

            Ada.Text_IO.Put_Line ("Error: reached the recursion limit for pseudo polygon completion");

         end if;

         Abort_Meshing := True;

         return;

      end if;

      if Polygon.Last_Index > 1 then

         Triangles.Add_Item (Triangle);

         Triangle.Set_Vertices (Vertex_A, Vertex_B, Polygon.First_Element);

         Split := False;

         for i in Polygon.First_Index .. Polygon.Last_Index loop

            Vertex := Polygon.Element (i);

            if i > 1 and then Triangle.In_Circumcircle (Vertex) then

               Split := True;

               Split_Index := i;

               Triangle.Set_Vertices (Vertex_A, Vertex_B, Vertex);

            end if;

         end loop;

         if Split then

            declare

               First_Polygon : Math.Vector2_Vector.Vector;

               Index : Positive := Polygon.First_Index;

            begin

               for i in Index .. Split_Index - 1 loop

                  First_Polygon.Append (Polygon.First_Element);

                  Polygon.Delete_First;

               end loop;

               Polygon.Delete_First;

               -----------------------------------------------------------------
               -- Recursive calls
               -----------------------------------------------------------------

               Complete_Pseudo_Polygon (First_Polygon, Vertex_A, Triangle.Get_Vertex_C);

               if Polygon.Last_Index > 0 then

                  Complete_Pseudo_Polygon (Polygon, Triangle.Get_Vertex_C, Vertex_B);

               end if;

            end;

         else

            Polygon.Delete_First;

            Complete_Pseudo_Polygon (Polygon, Triangle.Get_Vertex_C, Vertex_B);

         end if;

      elsif Polygon.Last_Index = 1 then

         Triangles.Add_Item (Triangle);

         Triangle.Set_Vertices (Vertex_A, Vertex_B, Polygon.First_Element);

      end if;

   exception

      when E : others =>

         Abort_Meshing := True;

         Ada.Text_IO.Put_Line ("the mesher crashed while completing a pseudo polygon");

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   procedure Insert_Missing_Segments is

      Segment : Segment_Access;

      Triangle : Triangle_Access;

   begin

      if Verbosity_Level > 3 then

         Ada.Text_IO.Put_Line ("Current status:");

         Triangle := Triangles.Get_First_Item;

         while Triangle /= null loop

            Triangle.Publish;

            Ada.Text_IO.Put_Line ("");

            Triangles.Get_Next_Item (Triangle);

         end loop;

      end if;

      Reset_Progress (Segments.Get_Count);

      Segment := Segments.Get_First_Item;

      while Segment /= null and not Abort_Meshing loop

         if Segment_Is_Missing (Segment) then

            Fix_Segment (Segment);

         end if;

         Push_Progress;

         Segments.Get_Next_Item (Segment);

      end loop;

   exception

      when E : others =>

         Abort_Meshing := True;

         Ada.Text_IO.Put_Line ("the mesher crashed while inserting missing segments");

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   function Is_At_Boundary (Vertex_1, Vertex_2 : Vector2_Access) return Boolean is

      Segment : Segment_Access := Segments.Get_First_Item;

   begin

      while Segment /= null loop

         if
           (Segment.Get_Vertex_A = Vertex_1 and Segment.Get_Vertex_B = Vertex_2) or
           (Segment.Get_Vertex_B = Vertex_1 and Segment.Get_Vertex_A = Vertex_2)
         then

            return True;

         end if;

         Segments.Get_Next_Item (Segment);

      end loop;

      return False;

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   function Is_Outside_Polygon (Triangle : Triangle_Access) return Boolean is

      Adjacent : Triangle_Access;

      Adjacent_Side : Triangle_Side_Kinds;

   begin

      Get_Adjacent_Triangle (Triangle, AB_Side, Adjacent, Adjacent_Side);

      if Adjacent = null and then not Is_At_Boundary (Triangle.Get_Vertex_A, Triangle.Get_Vertex_B) then

         return True;

      end if;

      Get_Adjacent_Triangle (Triangle, BC_Side, Adjacent, Adjacent_Side);

      if Adjacent = null and then not Is_At_Boundary (Triangle.Get_Vertex_B, Triangle.Get_Vertex_C) then

         return True;

      end if;

      Get_Adjacent_Triangle (Triangle, CA_Side, Adjacent, Adjacent_Side);

      if Adjacent = null and then not Is_At_Boundary (Triangle.Get_Vertex_C, Triangle.Get_Vertex_A) then

         return True;

      end if;

      return False;

   end;

   -----------------------------------------------------------------------------
   -- (See specification file).
   -----------------------------------------------------------------------------
   procedure Clean_Mesh is

      Found : Boolean := True;

      Triangle : Triangle_Access;

      Next : Triangle_Access;

      Circumcenter : Vector2_Record;

   begin

      if Verbosity_Level > 2 then

         Ada.Text_IO.Put_Line ("Restoring natural coordinates...");

      end if;

      declare

         Point : Vector2_Access := Vertices.Get_First_Item;

      begin

         while Point /= null loop

            Point.Set (Point.Get_X * Scale + Center.Get_X, Point.Get_Y * Scale + Center.Get_Y);

            Vertices.Get_Next_Item (Point);

         end loop;

      end;

      if Verbosity_Level > 2 then

         Ada.Text_IO.Put_Line ("Removing triangles...");

      end if;

     Reset_Progress (Triangles.Get_Count);

      --------------------------------------------------------------------------
      -- Remove the triangles joined to the initial vertices
      --------------------------------------------------------------------------

      while Found loop

         Found := False;

         Triangle := Triangles.Get_First_Item;

         while Triangle /= null and not Abort_Meshing loop

            if
              Triangle.Get_Vertex_A = Point_1 or
              Triangle.Get_Vertex_B = Point_1 or
              Triangle.Get_Vertex_C = Point_1 or
              Triangle.Get_Vertex_A = Point_2 or
              Triangle.Get_Vertex_B = Point_2 or
              Triangle.Get_Vertex_C = Point_2 or
              Triangle.Get_Vertex_A = Point_3 or
              Triangle.Get_Vertex_B = Point_3 or
              Triangle.Get_Vertex_C = Point_3
            then

               Found := True;

               Triangles.Remove_Item (Triangle);

            else

               Triangles.Get_Next_Item (Triangle);

            end if;

         end loop;

         Push_Progress;

      end loop;

      Point_1.Set (0.0, 0.0);

      Point_2.Set (0.0, 0.0);

      Point_3.Set (0.0, 0.0);

      --------------------------------------------------------------------------
      -- Remove the triangles in the concavities of the polygon
      --------------------------------------------------------------------------

      Reset_Progress (Triangles.Get_Count);

      case Cleaning_Method is

         when Adjacency_Base =>

            -- Method A: Will always work, but it consumes a lot of processor.

            Found := True;

            while Found and not Abort_Meshing loop

               Found := False;

               Triangle := Triangles.Get_First_Item;

               while Triangle /= null loop

                  if Is_Outside_Polygon (Triangle) then

                     Found := True;

                     Triangles.Remove_Item (Triangle);

                  else

                     Triangles.Get_Next_Item (Triangle);

                  end if;

               end loop;

               Push_Progress;

            end loop;

         when Polygon_Based =>

            -- Method B: Depends on a second algorithm to check if the center of the
            --           triangle belogs to the polygon. It runs much faster, but it could
            --           fail if the topology of the polygon is ill.

            Triangle := Triangles.Get_First_Item;

            while Triangle /= null and not Abort_Meshing loop

               if not Math.Tools.Contains_Point (Polygon, Triangle.Get_Center.Get_X, Triangle.Get_Center.Get_Y) then

                  Found := True;

                  if Verbosity_Level > 3 then

                     Ada.Text_IO.Put_Line ("Outer found:");

                     Triangle.Publish;

                  end if;

                  Next := Triangle;

                  Triangles.Get_Next_Item (Next);

                  Triangles.Remove_Item (Triangle);

                  Triangle := Next;

               else

                  Triangles.Get_Next_Item (Triangle);

               end if;

            end loop;

      end case;

   exception

      when E : others =>

         Abort_Meshing := True;

         Ada.Text_IO.Put_Line ("the mesher crashed while clearing the triangulation");

   end;

end Meshing.Delaunay;
--------------------------------------------------------------------------------
