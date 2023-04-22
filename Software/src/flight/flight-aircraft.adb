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
with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;
-- Gnav
with Maps.Terrain;
with Utility.Log;
with Utility.Strings;
use  Utility.Strings;
with Utility.Units;
use  Utility.Units;
with Timing.Events;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Aircraft is
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the data has been modified (it will be saved if so)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Setup_Modified : Boolean := False;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Setup_File_Name : constant String := Utility.Base_Directory & "data/aircraft_setup.dat";
      
   --===========================================================================
   -- Loads the last saved setup
   --===========================================================================
   procedure Read_Setup is
      
      use Ada.Text_IO;
      
      File_Id      : File_Type;
 
      Line_Reader  : String_Buffer (1000);

      Mass_Index   : Mass_Point_Range := Mass_Point_Range'First;
      
   begin
      
      if Ada.Directories.Exists (Setup_File_Name) then

         Open (File_Id, In_File, Setup_File_Name);

         Utility.Log.Put_Message ("reading aircraft setup");

         while not End_Of_File (File_Id) loop
            
            Line_Reader.Load (Ada.Text_IO.Get_Line (File_Id));

            declare
               Key   : String := Line_Reader.Read_Next ('=');
               Value : String := Line_Reader.Read_Next ('=');
            begin
               
               if Key = "INDEX" then -- (this must be the first entry)
               
                  declare                     
                     Index : Integer := Integer'Value (Value);
                  begin
                     
                     if Index in Aircraft_Range then  
                        
                        This_Aircraft := Aircrafts (Aircraft_Range (Index))'Access;       
                        
                     else
                        
                        Utility.Log.Put_Message ("invalid index, cannot load setup");

                        return;
                        
                     end if;
                     
                  end;
                   
               elsif Key = "MASS" then
                    
                  if This_Aircraft.Mass_Points (Mass_Index).Active then                     
                     This_Aircraft.Mass_Points (Mass_Index).Mass := Float'Value (Value);                  
                  end if;
                  
                  if Mass_Index < Mass_Point_Range'Last then
                     Mass_Index := Mass_Index + 1;
                  end if;
                  
               end if;
               
            end;
            
         end loop;
         
         Close (File_Id);
         
      end if;
            
      Recalculate_Mass;
      
      Setup_Modified := False;

   exception
      when E : others => 
         
         Utility.Log.Put_Message (E, "error while reading aircraft setup");

         Close (File_Id);
         
   end Read_Setup;
   -----------------------------------------------------------------------------  
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Write_Aircraft_Setup is

      use Ada.Text_IO;
      use Utility.Strings;

      File_Id   : File_Type;

   begin

      Create (File_Id, Out_File, Setup_File_Name);

      Utility.Log.Put_Message ("writing aircraft setup");

      for A in Aircraft_Range loop
         
         if 
           This_Aircraft.Model        = Aircrafts (A).Model and then
           This_Aircraft.Registration = Aircrafts (A).Registration
         then
            
            Put_Line (File_Id, "INDEX=" & Trim (Aircraft_Range'Image (A)));
            
            for Point of This_Aircraft.Mass_Points loop

               if Point.Active then

                  Put_Line (File_Id, "MASS=" & Float_Image (Point.Mass, 0));
            
               end if;
         
            end loop;

            exit;
                        
         end if;
         
      end loop;
      
      Close (File_Id);

   exception
      when E : others => 
         
         Utility.Log.Put_Message (E, "error while writing aircraft setup");

         Close (File_Id);
         
   end Write_Aircraft_Setup;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Save_If_Modified is
   begin

      if Setup_Modified then

         Write_Aircraft_Setup;

         Setup_Modified := False;

      end if;

   end Save_If_Modified;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Final_Altitude_1 (Position : Position_Record) return Float renames Get_Final_Altitude;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a steady gliding equilibrium state
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Gliding_State_Record is record
           
      V  : Float; -- Aerodynamic speed   
      Vh : Float; -- Horizontal component
      Vv : Float; -- Vertical component (sink rate)
      
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
   -- NOTE: the gliding ratio is measured as Vv / Vh for numerical convenience.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Best_Gliding_Record is record
      
      Airspeed       : Float; -- Aerodynamic speed
      Ground_Speed   : Float; -- Ground speed
      Gliding_Ratio  : Float; -- Inverse of the gliding slope
      Vertical_Speed : Float; -- Sink rate (-) => down (+) => up
      
   end record;
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Gliding_Ratio : constant Float := -10000.0;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Best_Gliding_Record : constant Best_Gliding_Record := (Airspeed       => 0.0,
                                                             Ground_Speed   => 0.0,
                                                             Gliding_Ratio  => No_Gliding_Ratio,
                                                             Vertical_Speed => 0.0);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Lookup table representing the spectrum of best gliding slopes 
   -- (from 0 to 180 degrees)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Gliding_Spectrum : array (0..180) of Best_Gliding_Record := (others => No_Best_Gliding_Record);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a gliding range ceiling cone that can be used to intersect the
   -- terrain.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Range_Cone_Record is tagged record

      Center   : Position_Record := No_Position_Record; -- Reference position
      Altitude : Float           := No_Altitude;        -- Reference altitude
      Wind     : Vector2_Record  := No_Vector2_Record;  -- Reference wind (horizontal)
      Ascent   : Float           := 0.0;                -- Vertical wind component
      Lift     : Float           := 0.0;                -- Expected lift in next thermal (not used yet)
      Mass     : Float           := 0.0;
   
   end record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The variable properties of the range cone that determine when to update it
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Range_Cone : Range_Cone_Record;
       
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Ascent (Value : Float) is
   begin
      
      if Value /= Range_Cone.Ascent then
         
         Range_Cone.Ascent := Float'Max (-4.0, Float'Min (4.0, Value));
                         
         Calculate_Gliding_Spectrum;
         
      end if;
      
   end Set_Ascent;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Ascent return Float is
   begin
      
      return Range_Cone.Ascent;
      
   end Get_Ascent;
   -----------------------------------------------------------------------------
           
   
   
   
   --===========================================================================
   -- Recualculates the recomended landing speed
   --===========================================================================
   procedure Recalculate_Reference_Speed is
   begin
      
      This_Aircraft.V_LND := 1.3 * This_Aircraft.V_S0 + 0.5 * Float (Range_Cone.Wind.Norm2);
      
   end Recalculate_Reference_Speed;
   -----------------------------------------------------------------------------    
   
   
        
      
   --===========================================================================
   -- Recualculates the stall speed at sea level (EAS)
   --===========================================================================
   procedure Recalculate_Stall_Speed is
      
      -- NOTE: 16.0 is obtained from 2.0 * 9.8 / 1.225 (constant)
      
   begin
      
      if 
        This_Aircraft.Total_Mass > 0.0 and then
        This_Aircraft.Area       > 0.0 and then
        This_Aircraft.Cl_Max     > 0.0
      then
               
         This_Aircraft.V_S0 := Sqrt (16.0 * This_Aircraft.Total_Mass / This_Aircraft.Area / This_Aircraft.Cl_Max);
      
      end if;
      
      Recalculate_Reference_Speed;
      
      Utility.Log.Put_Message ("stall speed:" & Float'Image (This_Aircraft.V_S0));

   end Recalculate_Stall_Speed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Recualculates the maximum CL for the current flaps
   --===========================================================================
   procedure Recalculate_Cl_Max is                  
   begin
         
      This_Aircraft.Cl_Max := 0.0;
         
      if This_Aircraft.Valid then
         
         for I in 1..This_Aircraft.Polar_Count loop
               
            if This_Aircraft.Cl_Max < This_Aircraft.Polar (I).Cl then
               
               This_Aircraft.Cl_Max := This_Aircraft.Polar (I).Cl;
               
            end if;
                                 
         end loop;
            
      end if;
                 
      Recalculate_Stall_Speed;
      
      Utility.Log.Put_Message ("CL max:" & Float'Image (This_Aircraft.Cl_Max));

   end Recalculate_Cl_Max;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Recalculate_Mass is
   begin
      
      This_Aircraft.Total_Mass := This_Aircraft.Empty_Mass;
      
      for I in Mass_Point_Range loop
         
         if This_Aircraft.Mass_Points (I).Active then
            
            This_Aircraft.Total_Mass := This_Aircraft.Total_Mass + This_Aircraft.Mass_Points (I).Mass;
            
         end if;
         
      end loop;
            
      Recalculate_Stall_Speed;
      
      Utility.Log.Put_Message ("total mass:" & Float'Image (This_Aircraft.Total_Mass));

      Setup_Modified := True;
      
   end Recalculate_Mass;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Read_Aircraft_Data is
      
      use Ada.Text_IO;
      
      File_Name    : constant String := "data/aircraft_list.dat";

      File_Id      : File_Type;
 
      Line_Reader  : String_Buffer (1000);

      Value_Reader : String_Buffer (100);
      
      Mass_Index   : Mass_Point_Range := Mass_Point_Range'First;
      
      Index        : Aircraft_Range   := Aircraft_Range'First;
      
   begin
      
      if Ada.Directories.Exists (File_Name) then

         Open (File_Id, In_File, File_Name);

         Utility.Log.Put_Message ("reading aircraft data");

         while not End_Of_File (File_Id) loop
            
            Line_Reader.Load (Ada.Text_IO.Get_Line (File_Id));

            declare
               Key   : String := Line_Reader.Read_Next ('=');
               Value : String := Line_Reader.Read_Next ('=');
            begin
               
               if Key = "#" then
               
                  if Index < Aircraft_Range'Last then
                     
                     -- New Aircraft
                     ------------------------
                     Index := Index + 1;
                  
                     Mass_Index := Mass_Point_Range'First;
                  
                     This_Aircraft := Aircrafts (Index)'Access;
                  
                     This_Aircraft.Valid := False;
                     
                  else
                     Utility.Log.Put_Message ("impossible to load all aircraft, stack is full");
                     
                     return;
                     
                  end if;
                     
               elsif Key = "MODEL" then
                  
                  Override (This_Aircraft.Model, Value);
                  
                  This_Aircraft.Valid := True;
                            
               elsif Key = "REGISTRATION" then
                  
            	  Override (This_Aircraft.Registration, Value);  
                            
               elsif Key = "WING_AREA" then
                  
                  This_Aircraft.Area := Float'Value (Value);
                           
               elsif Key = "EMPTY_MASS" then
                  
                  This_Aircraft.Empty_Mass := Float'Value (Value);
                  
               elsif Key = "MASS_MAX" then
                  
                  This_Aircraft.Maximum_Mass := Float'Value (Value);
                    
               elsif Key = "VNE" then
                  
                  This_Aircraft.V_NE := Utility.Units.Convert (Float'Value (Value), Unit_Kilometer_Hour, Unit_Meter_Second);
                  
               elsif Key = "VNO" then
                  
                  This_Aircraft.V_NO := Utility.Units.Convert (Float'Value (Value), Unit_Kilometer_Hour, Unit_Meter_Second);
                  
               elsif Key = "MASS_POINT" then
                  
                  Value_Reader.Load (Value);
                  
                  This_Aircraft.Mass_Points (Mass_Index).Active := True;
                  
                  Override (This_Aircraft.Mass_Points (Mass_Index).Label, Value_Reader.Read_Next ('@'));
                  
                  This_Aircraft.Mass_Points (Mass_Index).Arm      := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                  This_Aircraft.Mass_Points (Mass_Index).Mass     := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                  This_Aircraft.Mass_Points (Mass_Index).Mass_Min := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                  This_Aircraft.Mass_Points (Mass_Index).Mass_Max := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                  if Mass_Index < Mass_Point_Range'Last then
                     
                     Mass_Index := Mass_Index + 1;
                     
                  end if;
                  
               elsif Key = "POLAR_NAME" then
                  
                  null;
                  --Override (This_Aircraft.Polar_Name, Value);
                  
               elsif Key = "POLAR_POINT" then
                  
                  if This_Aircraft.Polar_Count < This_Aircraft.Polar'Last then
                     
                     This_Aircraft.Polar_Count := This_Aircraft.Polar_Count + 1;
                     
                     Value_Reader.Load (Value);
                  
                     This_Aircraft.Polar (This_Aircraft.Polar_Count).Cl := Float'Value (Trim (Value_Reader.Read_Next (';')));
                     
                     This_Aircraft.Polar (This_Aircraft.Polar_Count).Cd := Float'Value (Trim (Value_Reader.Read_Next (';')));
                     
                  end if;
                  
               end if;
               
            end;
                                   
         end loop;
         
         Close (File_Id);
         
      end if;
      
      for I in Aircraft_Range loop
         
         if Aircrafts (I).Valid then
            
            This_Aircraft := Aircrafts (I)'Access;      
            
            Recalculate_Cl_Max;
            
            Recalculate_Mass;
      
            Calculate_Gliding_States;
            
         end if;
         
      end loop;
      
      This_Aircraft := Aircrafts (Aircrafts'First)'Access;
      
   end Read_Aircraft_Data;
   -----------------------------------------------------------------------------

   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Calculate_Gliding_States is
      
      M : Float renames This_Aircraft.Total_Mass;
      S : Float renames This_Aircraft.Area;
      
      G : Float := 0.0;   -- Gliding angle (in radians)
      V : Float := 25.0;  -- Aerodynamic speed (EAS)
      R : Float := 1.225; -- Air density at ISA sea level
   
   begin
    
      for I in 1..This_Aircraft.Polar_Count loop
             
         G := Arctan (This_Aircraft.Polar (I).Cd / This_Aircraft.Polar (I).Cl);
            
         V := Sqrt (Cos (G) * 2.0 * M * 9.8 / (S * R * This_Aircraft.Polar (I).Cl));
         
         Gliding_States (I).V  := V;
         
         -- TODO: a possible improvement would be to use the TAS for Vh and Vv 
         --       in different altitude levels
         
         Gliding_States (I).Vh :=  V * Cos (G);
         
         Gliding_States (I).Vv := -V * Sin (G);
         
      end loop;
            
      Calculate_Gliding_Spectrum;
      
   exception
      when E : others =>         
         Utility.Log.Put_Message (E, "error while computing sink rates");               
      
   end Calculate_Gliding_States;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Calculates the gliding spectrump.
   -- Note: when there is positive ascent and the gliding ratio is locked on 
   -- +/- 10000.0 around the singularity          
   --===========================================================================
   procedure Calculate_Gliding_Spectrum is
      
      Psi   : Float   := 0.0; -- Direction angle
      G     : Float   := 0.0; -- Gliding ratio for a given speed and direction
      G_Max : Float   := 0.0; -- Maximum gliding ratio in a given direction
      W     : Float   := 0.0; -- Wind speed
      W_Cos : Float   := 0.0; -- Wind component in flight direction
      W_Sin : Float   := 0.0; -- Wind component normal to flight direction
      Vh    : Float   := 0.0; -- Horizontal speed relative to air
      Uh    : Float   := 0.0; -- Horizontal speed relative to ground
      Uv    : Float   := 0.0; -- Vertical speed relative to ground
      First : Boolean := True;
      
   begin

      Utility.Log.Put_Message ("computing gliding spectrum");
      
      Gliding_Spectrum := (others => No_Best_Gliding_Record);
      
      W := Float (Range_Cone.Wind.Norm2);
      
      for I in Gliding_Spectrum'Range loop
      
         Psi := Float (I) * Float (Math.Pi) / Float (Gliding_Spectrum'Length);
         
         G_Max := No_Gliding_Ratio;
         
         First := True;
         
         for J in 1..This_Aircraft.Polar_Count loop
            
            if Gliding_States (J).V > 0.0 and Gliding_States (J).V < This_Aircraft.V_NE then
            
               Uv := Gliding_States (J).Vv + Range_Cone.Ascent;
              
               Vh := Gliding_States (J).Vh;
            
               W_Cos := W * Cos (Psi);
            
               W_Sin := W * Sin (Psi);
            
               if Vh > W_Sin then
               
                  Uh := W_Cos + Sqrt (Vh ** 2.0 - W_Sin ** 2.0);
               
                  if Uh > 0.0 then
                  
                     if abs Uv > abs No_Gliding_Ratio * Uh then
                        
                        G := Float'Copy_Sign (No_Gliding_Ratio, Uv);
                        
                     else
                                             
                        G := Uv / Uh;
                        
                     end if;
               
                  else
                 
                     G := No_Gliding_Ratio;
                
                  end if;
               
               else
               
                  G := No_Gliding_Ratio;
               
               end if;
            
               -- TODO: take mean value when the maximum is found
               
               if First or G > G_Max then
               
                  G_Max := G;
               
                  Gliding_Spectrum (I).Airspeed       := Gliding_States (J).V;
               
                  Gliding_Spectrum (I).Ground_Speed   := Uh;
               
                  Gliding_Spectrum (I).Vertical_Speed := Uv;
                 
                  Gliding_Spectrum (I).Gliding_Ratio  := G;
                 
                  First := False;
                  
               end if;
               
            end if;
            
         end loop;
         
         Utility.Log.Put_Message ("@" & Natural'Image (I) & " G/R=" & Float'Image (G_Max) & " V=" & Float'Image (Gliding_Spectrum (I).Airspeed));
               
      end loop;
            
      Recalculate_Reference_Speed;
      
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
      G : Float   := Gliding_Spectrum (I).Gliding_Ratio;
      A : Float   := Range_Cone.Altitude;
      
   begin
      
      if G > No_Gliding_Ratio and A /= No_Altitude then
               
         return A + D * G;
               
      else
         
         return No_Altitude;
         
      end if;
      
   end Get_Final_Altitude;
   -----------------------------------------------------------------------------
   



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Final_Altitude (Position : Position_Record) return Float is

      R : Vector2_Record := Vector (Range_Cone.Center, Position, 1000.0);

   begin

      return Get_Final_Altitude (R);

   end Get_Final_Altitude;
   -----------------------------------------------------------------------------
   



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Required_Altitude (Position : Position_Record) return Float is

      R : Vector2_Record := Vector (Range_Cone.Center, Position, 1000.0);      
      D : Float   := Float (R.Norm2);
      P : Float   := abs Float (Range_Cone.Wind.Angle (R));
      I : Natural := Natural (P / Float (Math.Pi) * 180.0);
      G : Float   := Gliding_Spectrum (I).Gliding_Ratio;
      A : Float   := Range_Cone.Altitude;
      
   begin
         
      if G > 0.0 then
               
         return 0.0;
          
      elsif G > No_Gliding_Ratio then
               
         return abs (D * G);
               
      else
         
         return No_Altitude;
         
      end if;
      
   end Get_Required_Altitude;
   -----------------------------------------------------------------------------
   



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Optimal_Speed (Position : Position_Record) return Float is

      R : Vector2_Record := Vector (Range_Cone.Center, Position, 1000.0);      
      P : Float          := abs Float (Range_Cone.Wind.Angle (R));
      I : Natural        := Natural (P / Float (Math.Pi) * 180.0);
      
   begin
  
      return Gliding_Spectrum (I).Airspeed;
         
   end Get_Optimal_Speed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Optimal_Speed return Float is
   begin
      
      if Flight.Data.Is_Valid (Field_Course) then
         declare
            R : Vector2_Record;      
            P : Float;
            I : Integer;
         begin
           
            R.Set_From_Polar ((90.0 - Long_Float (Flight.Data.Course)) * Math.Pi / 180.0, 1.0);
            P := abs Float (Range_Cone.Wind.Angle (R));
            I := Natural (P / Float (Math.Pi) * 180.0);
            
            if I in Gliding_Spectrum'Range then         
               return Gliding_Spectrum (I).Airspeed;
            end if;

         end;
         
      end if;
         
      return 0.0;
      
   end Get_Optimal_Speed;
   -----------------------------------------------------------------------------
   


  
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Range_Mode : Range_Mode_Kinds := Range_Straight;
     
   --===========================================================================
   --
   --===========================================================================
   procedure Set_Range_Mode (Value : Range_Mode_Kinds) is
   begin
      if Range_Mode /= Value then
         
         Range_Mode := Value;
         
         if Range_Mode = Range_Straight then
            Maps.Range_Cone_Function := Get_Final_Altitude_1'Access;
         else
            Maps.Range_Cone_Function := Get_Final_Altitude_2'Access;
         end if;
         
         Maps.Terrain.Notify_Range_Changed;
                    
      end if;
      
   end Set_Range_Mode;
   -----------------------------------------------------------------------------
                  
   --===========================================================================
   --
   --===========================================================================
   function Get_Range_Mode return Range_Mode_Kinds is
   begin
      return Range_Mode;
   end Get_Range_Mode;
   -----------------------------------------------------------------------------
   
   
   
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Cone_Mode  : Cone_Mode_Kinds := Cone_10_To_1;
      
   --===========================================================================
   --
   --===========================================================================
   procedure Set_Cone_Mode (Value : Cone_Mode_Kinds) is
   begin
      if Cone_Mode /= Value then
         
         Cone_Mode := Value;
                          
         Maps.Terrain.Notify_Range_Changed;
                    
      end if;
      
   end Set_Cone_Mode;
   -----------------------------------------------------------------------------
   
   --===========================================================================
   --
   --===========================================================================
   function Get_Cone_Mode return Cone_Mode_Kinds is
   begin
      return Cone_Mode;
   end;
   -----------------------------------------------------------------------------
   
   
   
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Cone_Reference : Position_Record := No_Position_Record;
       
   --===========================================================================
   --
   --===========================================================================
   procedure Set_Reference (Value : Position_Record) is
   begin
      if Cone_Reference /= Value then
         
         Cone_Reference := Value;
         
         Maps.Terrain.Notify_Range_Changed;
                    
      end if;
      
   end Set_Reference;
   -----------------------------------------------------------------------------

   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Final_Altitude_2 (Position : Position_Record) return Float is

      Rp : Vector2_Record := Vector (Range_Cone.Center, Position, 1000.0);
      Rh : Vector2_Record := Vector (Position, Cone_Reference, 1000.0);
      Hp  : Float;

   begin

      Hp := Get_Final_Altitude (Rp);
      
      if Hp = No_Altitude then
         
         return No_Altitude;
         
      else
      
         case Cone_Mode is
         
            when Cone_Optimal =>
            
               declare
                  D : Float   := Float (Rh.Norm2);
                  P : Float   := abs Float (Range_Cone.Wind.Angle (Rh));
                  I : Natural := Natural (P / Float (Math.Pi) * 180.0);
                  G : Float   := Gliding_Spectrum (I).Gliding_Ratio;
               begin
               
                  if G > No_Gliding_Ratio then
                  
                     return Hp + G * D;
                  
                  else
                  
                     return No_Altitude;
                  
                  end if;
               
               end;
            
            when Cone_10_To_1 =>
   
               return Hp - Float (Rh.Norm2) / 10.0;

         end case;
         
      end if;
      
   end Get_Final_Altitude_2;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Update_Cone is
      
      Changed : Boolean := False;
      
      Radius : constant Float := 1.0 / 120.0; -- about 900m in latitude
      
      Wind_Delta : constant Long_Float := Math.Pi / 36.0;
      
   begin
       
      -- Check if the aircraft mass changed more than 5kg
      --------------------------------------------------------------------------
         
      if abs (This_Aircraft.Total_Mass - Range_Cone.Mass) >= 5.0 then
         
         Range_Cone.Mass := This_Aircraft.Total_Mass;
         
         Calculate_Gliding_States;
         
         Changed := True;
         
      end if;
        
      -- Check if the altitude changed more than 10m
      --------------------------------------------------------------------------
         
      if abs (Flight.Data.Altitude - Range_Cone.Altitude) >= 10.0 then
         
         Range_Cone.Altitude := Flight.Data.Altitude;
         
         Changed := True;
         
      end if;
             
      -- Check if the distance changed more than radius
      --------------------------------------------------------------------------
             
      if Maps.Distance (Flight.Data.Position, Range_Cone.Center) > Radius then
            
         Range_Cone.Center := Flight.Data.Position;
           
         Changed := True;
         
      end if;
         
      -- Check if the wind rotated more than 5 degrees or changed more than 2m/s
      --------------------------------------------------------------------------
         
      if 
        abs  Flight.Data.Wind.Angle (Range_Cone.Wind) >= Wind_Delta or
        abs (Flight.Data.Wind.Norm2 - Range_Cone.Wind.Norm2) >= 2.0        
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
      
      Read_Setup;
      
      Update_Cone;
      
      Maps.Range_Cone_Function := Get_Final_Altitude_1'Access;
      
      Timing.Events.Register_Timer (1.0, Update_Cone'Access);
      
      Timing.Events.Register_Timer (Timer    => 5.0,
                                    Callback => Save_If_Modified'Access);

   end Init;
   -----------------------------------------------------------------------------
      
   
   
   
   --===========================================================================
   -- Sets the next aircraft on the list as This_Aircraft
   --===========================================================================
   procedure Next_Aircraft is
      
      New_Aircraft : access Aircraft_Record := This_Aircraft;
      Other : access Aircraft_Record;
      
   begin
      
      for I in Aircraft_Range loop
         
         Other := Aircrafts (I)'Access;
         
         if This_Aircraft = Other then
            
            if I = Aircraft_Range'Last or not Other.Valid then
               
               New_Aircraft := Aircrafts (Aircrafts'First)'Access;
            
               return;
               
            else
               
               if Aircrafts (I+1).Valid then
                  
                  New_Aircraft := Aircrafts (I+1)'Access;
                    
               else
                  
                  New_Aircraft := Aircrafts (Aircrafts'First)'Access;
            
               end if;
                                
            end if;
                 
         end if;
           
      end loop;
      
      if New_Aircraft /= This_Aircraft then
         
         Utility.Log.Put_Message ("changing aircraft");
         
         This_Aircraft := New_Aircraft;
         
         Calculate_Gliding_States;
         
         Setup_Modified := True;
      
      end if;
      
   end Next_Aircraft;
   -----------------------------------------------------------------------------  

end Flight.Aircraft;
--------------------------------------------------------------------------------
