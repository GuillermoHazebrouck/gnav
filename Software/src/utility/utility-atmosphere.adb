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
with Utility.Log;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Atmosphere is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the QNH can be used for flight level corrections
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Qnh_Valid : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The QNH to be used for level corrections
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Qnh_Value : Float   := 1013.25;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The unit used to represent the altitude
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Altitude_Unit : Altitude_Units := Unit_Meter;




   --===========================================================================
   --
   --===========================================================================
   function Get_Qnh_Valid return Boolean is
   begin

      return Qnh_Valid;

   end Get_Qnh_Valid;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- Sets the QNH in hPa
   --===========================================================================
   procedure Set_Qnh (Value : Float; Valid : Boolean) is
   begin

      if Qnh_Value /= Value or else Qnh_Valid /= Valid then

         Qnh_Value := Value;

         Qnh_Valid := Valid;

         On_Qnh_Changed.Trigger;

      end if;

   end Set_Qnh;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Gets the selected QNH
   --===========================================================================
   function Get_Qnh return Float is
   begin

      return Qnh_Value;

   end Get_Qnh;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Sets the QNH in hPa
   --===========================================================================
   procedure Set_Altitude_Unit (Value : Altitude_Units) is
   begin

      if Altitude_Unit /= Value then

         Altitude_Unit := Value;

         Utility.Log.Put_Message ("selected unit changed to " & Image (Value));

         On_Altitude_Unit_Changed.Trigger;

      end if;

   end Set_Altitude_Unit;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- Gets the selected QNH
   --===========================================================================
   function Get_Altitude_Unit return Altitude_Units is
   begin

      return Altitude_Unit;

   end Get_Altitude_Unit;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function To_Altitude (Level : Float) return Float is
   begin

      --NOTE: using linearized formula

      return Units.Convert (Value  => 100.0 * Level + 26.95 * (Qnh_Value - 1013.25),
                            Source => Unit_Feet,
                            Target => Altitude_Unit);


   end To_Altitude;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function To_Altitude (Value : Altitude_Strings; Forced : Boolean := False) return Altitude_Strings is

      use Utility.Strings;

      Result : Altitude_Strings := Value;
      Data   : String := Trim (Value);
      H      : Float  := 0.0;

   begin

      if Data'Length > 2 then

         if Data = "GND" then

            return Value;

         elsif Data (Data'First) = 'F' and then Data (Data'First+1) = 'L' then

            -- Eg.: FL055

            if Qnh_Valid or Forced then

               H := Float'Value (Trim (Data (Data'First+2..Data'Last)));

               H := To_Altitude (H); -- (check if leading 0's do not harm the conversion)

               Override (Result, Float_Image (H, 0) & Image (Altitude_Unit));

               return Result;

            else

               return Value;

            end if;

         elsif Data (Data'Last-1) = 'F' and then Data (Data'Last) = 'T' then

            -- Eg.: 4500FT

            H := Float'Value (Data (Data'First..Data'Last-2));

            H := Convert (H, Unit_Feet, Altitude_Unit);

            Override (Result, Float_Image (H, 0) & Image (Altitude_Unit));

            return Result;

         end if;

      elsif Data'Length > 1 and Data (Data'Last) = 'M' then

         --Eg.: 750M

         H := Float'Value (Data (Data'First+1..Data'Last));

         H := Convert (H, Unit_Meter, Altitude_Unit);

         Override (Result, Float_Image (H, 0) & Image (Altitude_Unit));

         return Result;

      end if;

      return Value;

   exception

      when E : others =>

         Utility.Log.Put_Message (E, "warning: invalid altitude " & Value);

         return No_Altitude_String;

   end To_Altitude;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function To_Airspeed (Value : Float; Altitude : Float) return Float is
   begin

      -- TODO

      return Value;

   end To_Airspeed;
   -----------------------------------------------------------------------------

end Utility.Atmosphere;
--------------------------------------------------------------------------------
