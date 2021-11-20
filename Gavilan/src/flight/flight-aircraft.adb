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
use  Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;
-- Gnav
with Maps.Terrain;
with Utility.Strings;
use  Utility.Strings;
with Timing.Events;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Aircraft is
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents the aerodynamic force coefficients of an equilibrium state
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Polar_Point_Record is record
      
      Cl : Float;
      
      Cd : Float;
      
   end record;
     
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Polar_Range is Natural range 1..40;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The actual number of points in the polar curve
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Polar_Count : Natural := 0;
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Polar_Point_Array is array (Polar_Range) of Polar_Point_Record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The aerodynamic force coefficients at equilibrium states
   -- NOTE: this is a characteristic of the aircraft.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Polar : Polar_Point_Array := (others => (0.0, 0.0));
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a steady gliding equilibrium state
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Gliding_State_Record is record
      
      -- Aerodynamic speed
      V  : Float;
      
      -- Horizontal component
      Vh : Float;
      
      -- Vertical component (sink rate)
      Vv : Float;
      
   end record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a range of steady gliding equilibrium state
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Gliding_State_Array is array (Polar_Range) of Gliding_State_Record;
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The sink rates at different speeds in steady gliding states
   -- NOTE: this varies with the altitude (density) and mass.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Gliding_States : Gliding_State_Array := (others => (0.0, 0.0, 0.0));
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a steady gliding equilibrium state
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Best_Gliding_Record is record
      
      -- Aerodynamic speed
      Airspeed : Float;
      
      -- Ground speed
      Ground_Speed : Float;
      
      -- Sink rate
      Sink_Rate : Float;
      
      -- Gliding slope
      Gliding_Ratio : Float;
      
   end record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Best_Gliding_Record : constant Best_Gliding_Record := (Airspeed      => 0.0,
                                                             Ground_Speed  => 0.0,
                                                             Sink_Rate     => 0.0,
                                                             Gliding_Ratio => 0.0);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The spectrum of best gliding slopes (from 0 to 180 degrees)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Gliding_Spectrum : array (0..180) of Best_Gliding_Record := (others => No_Best_Gliding_Record);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a gliding range ceiling cone that can be used to intersect the
   -- terrain.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Range_Cone_Record is tagged record

      Center   : Position_Record := No_Position_Record; -- reference position
      Altitude : Float           := 1000.0;             -- reference altitude
      Wind     : Vector2_Record  := No_Vector2_Record;  -- reference wind
   
   end record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The variable properties of the range cone that determine when to update it
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Range_Cone : Range_Cone_Record;
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The reference area of the airplane
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Area : Float := 0.0;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The mass of the airplane
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Empty_Mass : Float := 0.0;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The mass of the airplane
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Total_Mass : Float := 0.0;
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Recalculate_Mass is
   begin
      
      Total_Mass := Empty_Mass;
      
      for I in Mass_Point_Range loop
         
         if Mass_Points (I).Active then
            
            Total_Mass := Total_Mass + Mass_Points (I).Mass;
            
         end if;
         
      end loop;
            
      Ada.Text_IO.Put_Line ("total mass:" & Float'Image (Total_Mass));

   end Recalculate_Mass;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Read_Aircraft_Data is
      
      File_Name    : constant String := "data/aircraft.dat";

      File_Id      : File_Type;
 
      Line_Reader  : String_Buffer (1000);

      Value_Reader : String_Buffer (100);
      
      Mass_Index   : Mass_Point_Range := Mass_Point_Range'First;
      
   begin
      
      if Ada.Directories.Exists (File_Name) then

         Open (File_Id, In_File, File_Name);

         Ada.Text_IO.Put_Line ("reading aircraft data");

         while not End_Of_File (File_Id) loop
            
            Line_Reader.Load (Ada.Text_IO.Get_Line (File_Id));

            declare
               Key   : String := Line_Reader.Read_Next ('=');
               Value : String := Line_Reader.Read_Next ('=');
            begin
                           
               if Key = "MODEL" then
                  
                  Override (Model, Value);  
                            
               elsif Key = "REGISTRATION" then
                  
            	  Override (Registration, Value);  
                            
               elsif Key = "WING_AREA" then
                  
                  Area := Float'Value (Value);
                           
               elsif Key = "EMPTY_MASS" then
                  
                  Empty_Mass := Float'Value (Value);
                  
               elsif Key = "MAXIMUM_AIRSPEED" then
                  
                  Maximum_Airspeed := Float'Value (Value);
                  
               elsif Key = "MASS_POINT" then
                  
                  Value_Reader.Load (Value);
                  
                  Mass_Points (Mass_Index).Active := True;
                  
                  Override (Mass_Points (Mass_Index).Label, Value_Reader.Read_Next ('@'));
                  
                  Mass_Points (Mass_Index).Arm := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                  Mass_Points (Mass_Index).Mass := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                  if Mass_Index < Mass_Point_Range'Last then
                     
                     Mass_Index := Mass_Index + 1;
                     
                  end if;
                  
               elsif Key = "POLAR_POINT" then
                  
                  if Polar_Count < Polar'Last then
                     
                     Polar_Count := Polar_Count + 1;
                     
                     Value_Reader.Load (Value);
                  
                     Polar (Polar_Count).Cl := Float'Value (Trim (Value_Reader.Read_Next (';')));
                     
                     Polar (Polar_Count).Cd := Float'Value (Trim (Value_Reader.Read_Next (';')));
                     
                  end if;
                  
               end if;
               
            end;
                                   
         end loop;
         
         Close (File_Id);
         
      end if;
      
      Recalculate_Mass;
      
      Calculate_Gliding_States;

   end Read_Aircraft_Data;
   -----------------------------------------------------------------------------

   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Calculate_Gliding_States is
      
      M : Float renames Total_Mass;
      S : Float renames Area;
      
      G : Float := 0.0;   -- Gliding angle (in radians)
      V : Float := 25.0;  -- Aerodynamic speed
      R : Float := 1.225; -- Air density (make variable)
   
   begin
    
      for I in 1..Polar_Count loop
             
         G := Arctan (Polar (I).Cd / Polar (I).Cl);
            
         V := Sqrt (Cos (G) * 2.0 * M * 9.8 / (S * R * Polar (I).Cl));
         
         Gliding_States (I).V  := V;
         
         Gliding_States (I).Vh := V * Cos (G);
         
         Gliding_States (I).Vv := V * Sin (G);
         
      end loop;
            
      Calculate_Gliding_Spectrum;
      
   exception
      when others =>         
         Put_Line ("error while computing sink rates");               
      
   end Calculate_Gliding_States;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Calculate_Gliding_Spectrum is
      
      Psi   : Float := 0.0;
      G     : Float := 0.0;
      G_Max : Float := 0.0;
      W     : Float := 0.0;
      W_Cos : Float := 0.0;
      W_Sin : Float := 0.0;
      Vh    : Float := 0.0;
      Uh    : Float := 0.0;
      Uv    : Float := 0.0;
      
   begin

      Gliding_Spectrum := (others => No_Best_Gliding_Record);
      
      W := Float (Range_Cone.Wind.Norm2);
      
      for I in Gliding_Spectrum'Range loop
      
         Psi := Float (I) * Float (Math.Pi) / Float (Gliding_Spectrum'Length);
         
         G_Max := 0.0;
         
         for J in 1..Polar_Count loop
            
            if Gliding_States (J).V > 0.0 and Gliding_States (J).V < Maximum_Airspeed then
            
               Uv := Gliding_States (J).Vv;
              
               Vh := Gliding_States (J).Vh;
            
               W_Cos := W * Cos (Psi);
            
               W_Sin := W * Sin (Psi);
            
               if Vh > W_Sin then
               
                  Uh := W_Cos + Sqrt (Vh ** 2.0 - W_Sin ** 2.0);
               
                  if Uh > 0.0 then
                  
                     G := Uh / Uv;
               
                  else
                 
                     G := 0.0;
                
                  end if;
               
               else
               
                  G := 0.0;
               
               end if;
            
               if G > G_Max then
               
                  G_Max := G;
               
                  Gliding_Spectrum (I).Airspeed      := Gliding_States (J).V;
               
                  Gliding_Spectrum (I).Ground_Speed  := Uh;
               
                  Gliding_Spectrum (I).Sink_Rate     := Uv;
                 
                  Gliding_Spectrum (I).Gliding_Ratio := G;
                 
               end if;
               
            end if;
            
         end loop;
         
         Put_Line ("@" & Natural'Image (I) & " G/R=" & Float'Image (G_Max) & " V=" & Float'Image (Gliding_Spectrum (I).Airspeed));
               
      end loop;
            
      Maps.Terrain.Notify_Range_Changed;
            
   end Calculate_Gliding_Spectrum;
   -----------------------------------------------------------------------------
     
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Final_Altitude (V : Vector2_Record) return Float is
      
      D : Float   := Float (V.Norm2);
      P : Float   := abs Float (Range_Cone.Wind.Angle (V));
      I : Natural := Natural (P / Float (Math.Pi) * 180.0);
      R : Float   := Gliding_Spectrum (I).Gliding_Ratio;
      A : Float   := Range_Cone.Altitude;
      
   begin
      
      if R > 0.0 then
               
         return A - D / R;
               
      else
         
         return No_Altitude;
         
      end if;
      
   end Get_Final_Altitude;
   -----------------------------------------------------------------------------
   



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Final_Altitude (Position : Position_Record) return Float is

      R : Vector2_Record := Vector (Range_Cone.Center, Position);

   begin

      R.Scale (1000.0);

      return Get_Final_Altitude (R);

   end Get_Final_Altitude;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Update_Cone is
      
      Changed : Boolean := False;
      
      Radius : constant Float := 1.0 / 120.0; -- about 900m in latitude
      
   begin
      
      if 
        Flight.Data.Altitude > Range_Cone.Altitude + 20.0 or
        Flight.Data.Altitude < Range_Cone.Altitude - 20.0 
      then
         
         Range_Cone.Altitude := Flight.Data.Altitude;
         
         Changed := True;
         
      end if;
                  
      if Maps.Distance (Flight.Data.Position, Range_Cone.Center) > Radius then
            
         Range_Cone.Center := Flight.Data.Position;
           
         Changed := True;
         
      end if;
         
      --TODO: check the angle and intensity
      -------------------------------------
         
      if 
        Flight.Data.Wind.Get_X > Range_Cone.Wind.Get_X + 1.0 or
        Flight.Data.Wind.Get_X < Range_Cone.Wind.Get_X - 1.0 or
        Flight.Data.Wind.Get_Y > Range_Cone.Wind.Get_Y + 1.0 or
        Flight.Data.Wind.Get_Y < Range_Cone.Wind.Get_Y - 1.0           
      then
         
         Range_Cone.Wind := Flight.Data.Wind;
         
         Calculate_Gliding_Spectrum;
         
         Changed := True;
         
      end if;
      
      if Changed then
         
         Maps.Terrain.Notify_Range_Changed;
         
      end if;
               
   end Update_Cone;
   -----------------------------------------------------------------------------
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Init is
   begin
      
      Read_Aircraft_Data;
      
      Update_Cone;
      
      Maps.Range_Cone_Function := Get_Final_Altitude'Access;
      
      Timing.Events.Register_Timer (1.0, Update_Cone'Access);
      
   end Init;
   
   
end Flight.Aircraft;
--------------------------------------------------------------------------------
