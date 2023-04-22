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

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Units is

   --===========================================================================
   --
   --===========================================================================
   function Convert (Value : Float; Source, Target : Distance_Units) return Float is
   begin

      case Source is

         when Unit_Kilometer =>

            case Target is

               when Unit_Kilometer     => return Value;
               when Unit_Nautical_Mile => return Value / 1.852;
               when Unit_Meter         => return Value * 1000.0;
               when Unit_Feet          => return Value * 3280.84;
               when Unit_Hectofeet     => return Value * 32.8084;

            end case;

         when Unit_Nautical_Mile =>

            case Target is

               when Unit_Kilometer     => return Value * 1.852;
               when Unit_Nautical_Mile => return Value;
               when Unit_Meter         => return Value * 1852.0;
               when Unit_Feet          => return Value * 6076.115;
               when Unit_Hectofeet     => return Value * 60.76115;

            end case;

         when Unit_Meter         =>

            case Target is

               when Unit_Kilometer     => return Value * 0.001;
               when Unit_Nautical_Mile => return Value / 1852.0;
               when Unit_Meter         => return Value;
               when Unit_Feet          => return Value * 3.28084;
               when Unit_Hectofeet     => return Value * 0.0328084;

            end case;

         when Unit_Feet          =>

            case Target is

               when Unit_Kilometer     => return Value / 3280.84;
               when Unit_Nautical_Mile => return Value / 6076.115;
               when Unit_Meter         => return Value / 3.28084;
               when Unit_Feet          => return Value;
               when Unit_Hectofeet     => return Value / 0.0328084;

            end case;

         when Unit_Hectofeet     =>

            case Target is

               when Unit_Kilometer     => return Value / 32.8084;
               when Unit_Nautical_Mile => return Value / 60.76115;
               when Unit_Meter         => return Value / 0.0328084;
               when Unit_Feet          => return Value * 100.0;
               when Unit_Hectofeet     => return Value;

            end case;

      end case;

   end Convert;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Convert (Value : Float; Source, Target : Velocity_Units) return Float is
   begin

      case Source is

         when Unit_Meter_Second =>

            case Target is

               when Unit_Meter_Second   => return Value;
               when Unit_Kilometer_Hour => return Value * 3.6;
               when Unit_Knot           => return Value * 3.6 / 1.852;

            end case;

         when Unit_Kilometer_Hour =>

            case Target is

               when Unit_Meter_Second   => return Value / 3.6;
               when Unit_Kilometer_Hour => return Value;
               when Unit_Knot           => return Value / 1.852;

            end case;

         when Unit_Knot =>

            case Target is

               when Unit_Meter_Second   => return Value * 1.852 / 3.6;
               when Unit_Kilometer_Hour => return Value * 1.852;
               when Unit_Knot           => return Value;

            end case;

      end case;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Image (Value : Distance_Units) return String is
   begin

      case Value is

         when Unit_Kilometer     => return "KM";
         when Unit_Nautical_Mile => return "NM";
         when Unit_Meter         => return "M";
         when Unit_Feet          => return "FT";
         when Unit_Hectofeet     => return "FL";

      end case;

   end Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Image (Value : Velocity_Units) return String is
   begin

      case Value is

         when Unit_Meter_Second   => return "M/S";
         when Unit_Kilometer_Hour => return "KM/H";
         when Unit_Knot           => return "KTS";

      end case;

   end;
   -----------------------------------------------------------------------------

end Utility.Units;
--------------------------------------------------------------------------------
