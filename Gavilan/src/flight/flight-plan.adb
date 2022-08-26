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
with Flight.Aircraft;
with Math.Vector2;
use  Math.Vector2;
with Maps.Terrain;
with Timing.Events;
with Utility.Log;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Plan is

   --===========================================================================
   --
   --===========================================================================
   procedure Init is
   begin

      Read_Flight_Plans;

      Timing.Events.Register_Timer (Timer    => Timing.Time_Delta,
                                    Callback => Update_Flight_Plan'Access);

      Timing.Events.Register_Timer (Timer    => 5.0,
                                    Callback => Save_If_Modified'Access);

   end Init;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The file storing the flights
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   File_Name : constant String := "data/flights.dat";

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Bearing_Image (This : Waypoint_Record) return String is
   begin

      return Utility.Strings.Float_Image (This.Bearing, 0) & "*";

   end Get_Bearing_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Distance_Image (This : Waypoint_Record) return String is
   begin

      if This.Distance < 10.0 then

         return Utility.Strings.Float_Image (This.Distance, 1);

      else

         return Utility.Strings.Float_Image (This.Distance, 0);

      end if;

   end Get_Distance_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Vector_Image (This : Waypoint_Record) return String is
   begin

      if This.Distance < 10.0 then

         return Utility.Strings.Float_Image (This.Distance, 1) & " " & Utility.Strings.Float_Image (This.Bearing, 0) & "*";

      else

         return Utility.Strings.Float_Image (This.Distance, 0) & " " & Utility.Strings.Float_Image (This.Bearing, 0) & "*";

      end if;

   end Get_Vector_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Margin_Image (This : Waypoint_Record) return String is

      use Utility.Strings;

   begin

      if This.Margin = No_Altitude then

         return "N.A.";

      elsif This.Margin > 0.0 then

         return "}" & Float_Image (abs This.Margin, 0);

      else

         return "{" & Float_Image (abs This.Margin, 0);

      end if;


   end Get_Margin_Image;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Read_Flight_Plans is

      use Ada.Text_IO;
      use Utility.Strings;

      File_Id      : File_Type;

      Line_Reader  : String_Buffer  (1000);

      Value_Reader : String_Buffer (1000);

      First        : Boolean := True;

      I : Flight_Plan_Range  := Flight_Plan_Range'First;

      W : Waypoint_Range     := Waypoint_Range'First;

   begin

      if Ada.Directories.Exists (File_Name) then

         Open (File_Id, In_File, File_Name);

         Utility.Log.Put_Message ("reading flight plan data");

         Flight_Plans := (others => No_Flight_Plan_Record);

         while not End_Of_File (File_Id) loop

            Line_Reader.Load (Ada.Text_IO.Get_Line (File_Id));

            declare
               Key   : String := Line_Reader.Read_Next ('=');
               Value : String := Line_Reader.Read_Next ('=');
            begin

               if Key = "FLIGHT" then

                  W := Waypoint_Range'First;

                  if not First and I < Flight_Plan_Range'Last then

                     I := I + 1;

                  end if;

                  First := False;

                  Flight_Plans (I).Initialize;

                  Override (Flight_Plans (I).Name, Value);

                  Utility.Log.Put_Message ("FLIGHT PLAN");

               elsif Key = "WAYPOINT" then

                  Value_Reader.Load (Value);

                  Override (Flight_Plans (I).Waypoints (W).Name, Value_Reader.Read_Next ('@'));

                  Flight_Plans (I).Waypoints (W).Position := Maps.Value (Trim (Value_Reader.Read_Next ('@')));

                  Flight_Plans (I).Waypoints (W).Elevation := Maps.Terrain.Get_Elevation (Flight_Plans (I).Waypoints (W).Position);

                  Flight_Plans (I).Waypoints (W).Is_Loaded := True;

                  Utility.Log.Put_Message ("WAYPOINT @ " & Image (Flight_Plans (I).Waypoints (W).Position));

                  if W < Waypoint_Range'Last then

                     W := W + 1;

                  end if;

               end if;

            end;

         end loop;

         Close (File_Id);

      end if;

      for F in Flight_Plan_Range loop

         Flight_Plans (F).Recompute_Tasks;

      end loop;

   end Read_Flight_Plans;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Write_Flight_Plans is

      use Ada.Text_IO;
      use Utility.Strings;

      File_Id   : File_Type;

   begin

      Create (File_Id, Out_File, File_Name);

      Utility.Log.Put_Message ("writing flight plan data");

      for Plan of Flight.Plan.Flight_Plans loop

         if Plan.Is_Loaded then

            Put_Line (File_Id, "FLIGHT=" & Plan.Name);

            for Waypoint of Plan.Waypoints loop

               if Waypoint.Is_Loaded then

                  Put_Line (File_Id, "WAYPOINT=" & Waypoint.Name & " @ " & Maps.Image (Waypoint.Position));

               end if;

            end loop;

         end if;

      end loop;

      Close (File_Id);

   end Write_Flight_Plans;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Save_If_Modified is
   begin

      if Modified then

         Write_Flight_Plans;

         Modified := False;

      end if;

   end Save_If_Modified;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Update_Flight_Plan is

      Plan   : access Flight_Plan_Record := Flight_Plan;
      Wpt    : access Waypoint_Record    := null;
      Dist   : Float;

   begin

      -- Calculate distance and bearing
      --------------------------------------------------------------------------

      for W in Waypoint_Range loop

         exit when not Plan.Waypoints (W).Is_Loaded;

         Wpt := Plan.Waypoints (W)'Access;

         -- Vector (distance/bearing)
         -----------------------------------------------------------------------

         Maps.Coordinates (Position_A => Flight.Data.Position,
                           Position_B => Wpt.Position,
                           Distance   => Wpt.Distance,
                           Bearing    => Wpt.Bearing);

         -- Altitude margin
         -----------------------------------------------------------------------

         Wpt.Margin := Flight.Aircraft.Get_Final_Altitude (Wpt.Position) - Wpt.Elevation;

         if Wpt.Margin = No_Altitude then

            Wpt.In_Range := False;

         else

            Wpt.In_Range := Wpt.Margin > 0.0;

         end if;

         -- Status flags
         -----------------------------------------------------------------------

         Wpt.In_Proximity := Maps.Distance (Flight.Data.Position, Wpt.Position) < Proximity_Threshold;

         Wpt.Visited      := Plan.Waypoints (W).Visited or else Wpt.In_Proximity;

         Wpt.Is_Active    := W = Plan.Target;

         -- Task update
         -----------------------------------------------------------------------

         if Plan.Tasks (W).Is_Loaded then

            Plan.Tasks (W).Is_Active := Plan.Tasks (W).Point_B.Is_Active;

            Dist := Maps.Distance (Position_A => Flight.Data.Position,
                                   Position_B => Plan.Tasks (W).Point_B.Position);

            if Dist < Plan.Tasks (W).Length then

               Plan.Tasks (W).Progress := 1.0 - Dist / Plan.Tasks (W).Length;

            else

               Plan.Tasks (W).Progress := 0.0;

            end if;

         end if;

      end loop;

      -- Jump to the next waypoint when the active waypoint is reached
      --------------------------------------------------------------------------

      if Jump_In_Proximity and Plan.Waypoints (Plan.Target).In_Proximity then

         if not Plan.Go_Back then

            Goto_Next_Waypoint;

         else

            Goto_Previous_Waypoint;

         end if;

      end if;

   end Update_Flight_Plan;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Flight_Plan return access Flight_Plan_Record is
   begin

      return Flight_Plans (Active_Flight_Plan)'Access;

   end Flight_Plan;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Activates the next flight plan
   --===========================================================================
   function Next_Flight_Plan return Boolean is

      F : Flight_Plan_Range renames Active_Flight_Plan;

   begin

      if F < Flight_Plan_Range'Last and then Flight_Plans (F + 1).Is_Loaded then

         F := F + 1;

         return True;

      end if;

         return False;

   end Next_Flight_Plan;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Activates the previous flight plan
   --===========================================================================
   function Previous_Flight_Plan return Boolean is

      F : Flight_Plan_Range renames Active_Flight_Plan;

   begin

      if F > Flight_Plan_Range'First then

         F := F - 1;

         return True;

      end if;

      return False;

   end Previous_Flight_Plan;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Initialize (This : in out Flight_Plan_Record) is

      use Utility.Strings;

   begin

      This := No_Flight_Plan_Record;

      Override (This.Name, "PLAN");

      This.Is_Loaded := True;

      Override (This.Waypoints (1).Name, "HOME");

      This.Waypoints (1).Is_Loaded := True;

      This.Waypoints (1).Position := Flight.Data.Position;

      This.Recompute_Tasks;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Recompute_Tasks (This : in out Flight_Plan_Record) is
   begin

      This.Tasks := (others => No_Task_Record);

      for W in Waypoint_Range loop

         -- Exit when reaching the last waypoit or when the next is not loaded
         -----------------------------------------------------------------------

         if W = Waypoint_Range'Last or else not This.Waypoints (W + 1).Is_Loaded then
            exit;
         end if;

         -- Load properties
         -----------------------------------------------------------------------

         if This.Go_Back then

            This.Tasks (W).Point_A := This.Waypoints (W+1)'unrestricted_access;
            This.Tasks (W).Point_B := This.Waypoints (W  )'unrestricted_access;

         else

            This.Tasks (W).Point_A := This.Waypoints (W  )'unrestricted_access;
            This.Tasks (W).Point_B := This.Waypoints (W+1)'unrestricted_access;

         end if;

         Maps.Coordinates (Position_A => This.Tasks (W).Point_A.Position,
                           Position_B => This.Tasks (W).Point_B.Position,
                           Distance   => This.Tasks (W).Length,
                           Bearing    => This.Tasks (W).Course);

         This.Tasks (W).Vector := Maps.Vector (Position_A => This.Tasks (W).Point_B.Position,
                                               Position_B => This.Tasks (W).Point_A.Position);

         This.Tasks (W).Vector.Normalize;

         This.Tasks (W).Is_Loaded := True;

         This.Tasks (W).Is_Active := This.Tasks (W).Point_B.Is_Active;

      end loop;

   end Recompute_Tasks;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Go_Back (This : in out Flight_Plan_Record; Value : Boolean) is
   begin

      if This.Go_Back /= Value then

         This.Go_Back := Value;

         This.Recompute_Tasks;

      end if;

   end Set_Go_Back;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Append_Flight_Plan return Boolean is
   begin

      for F in Flight_Plan_Range loop

         if not Flight_Plans (F).Is_Loaded then

            Flight_Plans (F).Initialize;

            Active_Flight_Plan := F;

            Modified := True;

            return True;

         end if;

      end loop;

      return False;

   end Append_Flight_Plan;
   -----------------------------------------------------------------------------




   --===========================================================================
   --  (See specification file)
   --===========================================================================
   function Remove_Flight_Plan return Boolean is

      F : Flight_Plan_Range renames Active_Flight_Plan;

   begin

      -- Note: it is not allowed to remove all flight plans, at least one must
      -- remain as reference.
      --------------------------------------------------------------------------

      if
        (F = Flight_Plan_Range'First and Flight_Plans (F+1).Is_Loaded) or
        (F > Flight_Plan_Range'First and Flight_Plans (F  ).Is_Loaded)
      then

         -- Move all items backwards
         -----------------------------------------------------------------------
         for I in F..Flight_Plan_Range'Last-1 loop

            Flight_Plans (I) := Flight_Plans (I+1);

            exit when not Flight_Plans (I).Is_Loaded;

         end loop;

         -- The last waypoint is always vacant after the removal
         -----------------------------------------------------------------------
         Flight_Plans (Flight_Plan_Range'Last) := No_Flight_Plan_Record;

         -- If the new target is not loaded, move it to the new last one
         -----------------------------------------------------------------------
         if not Flight_Plans (F).Is_Loaded then

            for I in reverse Flight_Plan_Range loop

               F := I;

               exit when Flight_Plans (I).Is_Loaded;

            end loop;

         end if;

         Modified := True;

         return True;

      end if;

      return False;

   end Remove_Flight_Plan;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Next_Waypoint return access Waypoint_Record is
   begin

      return Flight_Plan.Waypoints (Flight_Plan.Target)'Access;

   end Next_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Remove_Active_Waypoint return Boolean is

      W : Waypoint_Range renames Flight_Plan.Target;

   begin

      -- Note: it is not allowed to remove all waypoints, at least one must
      -- remain as reference.
      --------------------------------------------------------------------------

      if
        (W = Waypoint_Range'First and Flight_Plan.Waypoints (W+1).Is_Loaded) or
        (W > Waypoint_Range'First and Flight_Plan.Waypoints (W  ).Is_Loaded)
      then

         -- Move all items backwards
         -----------------------------------------------------------------------
         for I in W..Waypoint_Range'Last-1 loop

            Flight_Plan.Waypoints (I) := Flight_Plan.Waypoints (I+1);

            exit when not Flight_Plan.Waypoints (I).Is_Loaded;

         end loop;

         -- The last waypoint is always vacant after the removal
         -----------------------------------------------------------------------
         Flight_Plan.Waypoints (Waypoint_Range'Last) := No_Waypoint_Record;

         -- If the new target is not loaded, move it to the new last one
         -----------------------------------------------------------------------
         if not Flight_Plan.Waypoints (W).Is_Loaded then

            for I in reverse Waypoint_Range loop

               W := I;

               exit when Flight_Plan.Waypoints (I).Is_Loaded;

            end loop;

         end if;

         Flight_Plan.Recompute_Tasks;

         Modified := True;

         return True;

      end if;

      return False;

   end Remove_Active_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Append_Waypoint return Boolean is

      use Utility.Strings;

      W : Waypoint_Range renames Flight_Plan.Target;

   begin

      if W < Waypoint_Range'Last then

         -- Move all items forward
         -----------------------------------------------------------------------
         for I in reverse W..Waypoint_Range'Last-1 loop

            Flight_Plan.Waypoints (I+1).Name      := Flight_Plan.Waypoints (I).Name;

            Flight_Plan.Waypoints (I+1).Position  := Flight_Plan.Waypoints (I).Position;

            Flight_Plan.Waypoints (I+1).Is_Loaded := Flight_Plan.Waypoints (I).Is_Loaded;

         end loop;

         -- Advance to the copy
         -----------------------------------------------------------------------
         W := W + 1;

         -- Make sure the position is valid
         -----------------------------------------------------------------------
         if Flight_Plan.Waypoints (W).Position = No_Position_Record then

            Flight_Plan.Waypoints (W).Position := Flight.Data.Position;

         end if;

         Override (Flight_Plan.Waypoints (W).Name, "WPT");

         Flight_Plan.Waypoints (W).Is_Loaded := True;

         Flight_Plan.Recompute_Tasks;

         Modified := True;

         return True;

      end if;

      return False;

   end Append_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Prepend_Waypoint return Boolean is

      use Utility.Strings;

      W : Waypoint_Range renames Flight_Plan.Target;

   begin

      if W < Waypoint_Range'Last then

         -- Move all items forward
         -----------------------------------------------------------------------
         for I in reverse W..Waypoint_Range'Last-1 loop

            Flight_Plan.Waypoints (I+1).Name      := Flight_Plan.Waypoints (I).Name;

            Flight_Plan.Waypoints (I+1).Position  := Flight_Plan.Waypoints (I).Position;

            Flight_Plan.Waypoints (I+1).Is_Loaded := Flight_Plan.Waypoints (I).Is_Loaded;

         end loop;

         -- Stay with the copy
         -----------------------------------------------------------------------
         Override (Flight_Plan.Waypoints (W).Name, "WPT");

         Flight_Plan.Recompute_Tasks;

         Modified := True;

         return True;

      end if;

      return False;

   end Prepend_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Home_Waypoint return access Waypoint_Record is
   begin

      return Flight_Plan.Waypoints (1)'Access;

   end Home_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Goto_Previous_Waypoint is
   begin

      if Flight_Plan.Target > Waypoint_Range'First then

         for W in reverse Waypoint_Range'First..Flight_Plan.Target - 1 loop

            if
              Flight_Plan.Waypoints (W).Is_Loaded and then
              (not Jump_In_Proximity or not Flight_Plan.Waypoints (W).In_Proximity)
            then

               Flight_Plan.Waypoints (Flight_Plan.Target).Is_Active := False;

               Flight_Plan.Target := W;

               Flight_Plan.Waypoints (Flight_Plan.Target).Is_Active := True;

               return;

            end if;

         end loop;

      end if;

   end Goto_Previous_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Activates the next waypoint
   --===========================================================================
   procedure Goto_Next_Waypoint is
   begin

      if Flight_Plan.Target < Waypoint_Range'Last then

         for W in Flight_Plan.Target + 1..Waypoint_Range'Last loop

            if
              Flight_Plan.Waypoints (W).Is_Loaded and then
              (not Jump_In_Proximity or not Flight_Plan.Waypoints (W).In_Proximity)
            then

               Flight_Plan.Waypoints (Flight_Plan.Target).Is_Active := False;

               Flight_Plan.Target := W;

               Flight_Plan.Waypoints (Flight_Plan.Target).Is_Active := True;

               return;

            end if;

         end loop;

      end if;

   end Goto_Next_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Toggle_Go_Back is
   begin

      Flight_Plan.Go_Back := not Flight_Plan.Go_Back;

      Flight_Plan.Recompute_Tasks;

   end Toggle_Go_Back;
   -----------------------------------------------------------------------------

end Flight.Plan;
--------------------------------------------------------------------------------
