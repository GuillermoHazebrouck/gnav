--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020, Luke A. Guest
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with System.Address_To_Access_Conversions;

package body SDL.Video.Surfaces.Makers is
   procedure Create (Self       : in out Surface;
                     Size       : in SDL.Sizes;
                     BPP        : in Pixel_Depths;
                     Red_Mask   : in Colour_Masks;
                     Blue_Mask  : in Colour_Masks;
                     Green_Mask : in Colour_Masks;
                     Alpha_Mask : in Colour_Masks) is
      function SDL_Create_RGB_Surface
        (Flags      : in Surface_Flags := 0;  --  TODO: Is this the correct type?
         Width      : in C.int         := 0;
         Height     : in C.int         := 0;
         Depth      : in Pixel_Depths  := 1;
         Red_Mask   : in Colour_Masks  := 0;
         Green_Mask : in Colour_Masks  := 0;
         Blue_Mask  : in Colour_Masks  := 0;
         Alpha_Mask : in Colour_Masks  := 0) return Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateRGBSurface";
   begin
      Self.Internal := SDL_Create_RGB_Surface (Width      => Size.Width,
                                               Height     => Size.Height,
                                               Depth      => BPP,
                                               Red_Mask   => Red_Mask,
                                               Green_Mask => Green_Mask,
                                               Blue_Mask  => Blue_Mask,
                                               Alpha_Mask => Alpha_Mask);
   end Create;

   procedure Create_From (Self       : in out Surface;
                          Pixels     : in Element_Pointer;
                          Size       : in SDL.Sizes;
                          BPP        : in Pixel_Depths := Element'Size;
                          Pitch      : in System.Storage_Elements.Storage_Offset;
                          Red_Mask   : in Colour_Masks;
                          Green_Mask : in Colour_Masks;
                          Blue_Mask  : in Colour_Masks;
                          Alpha_Mask : in Colour_Masks) is
      type Element_Pointer_C is access all Element with Convention => C;
      function SDL_Create_RGB_Surface_From
        (Pixels     : in Element_Pointer_C;
         Width      : in C.int           := 0;
         Height     : in C.int           := 0;
         Depth      : in Pixel_Depths    := 1;
         Pitch      : in C.int           := 0;
         Red_Mask   : in Colour_Masks    := 0;
         Green_Mask : in Colour_Masks    := 0;
         Blue_Mask  : in Colour_Masks    := 0;
         Alpha_Mask : in Colour_Masks    := 0) return Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateRGBSurfaceFrom";
   begin
      Self.Internal := SDL_Create_RGB_Surface_From (Pixels     => Element_Pointer_C (Pixels),
                                                    Width      => Size.Width,
                                                    Height     => Size.Height,
                                                    Depth      => BPP,
                                                    Pitch      => C.int (Pitch),
                                                    Red_Mask   => Red_Mask,
                                                    Green_Mask => Green_Mask,
                                                    Blue_Mask  => Blue_Mask,
                                                    Alpha_Mask => Alpha_Mask);
   end Create_From;

   procedure Create_From_Array (Self       : in out Surface;
                                Pixels     : access Element_Array;
                                Red_Mask   : in Colour_Masks;
                                Green_Mask : in Colour_Masks;
                                Blue_Mask  : in Colour_Masks;
                                Alpha_Mask : in Colour_Masks) is
      type Element_Pointer_C is access all Element with Convention => C;
      package Convert is new System.Address_To_Access_Conversions (Element);
      function SDL_Create_RGB_Surface_From
        (Pixels     : in Element_Pointer_C; -- Note: using System.Address here is not portable
         Width      : in C.int           := 0;
         Height     : in C.int           := 0;
         Depth      : in Pixel_Depths    := 1;
         Pitch      : in C.int           := 0;
         Red_Mask   : in Colour_Masks    := 0;
         Green_Mask : in Colour_Masks    := 0;
         Blue_Mask  : in Colour_Masks    := 0;
         Alpha_Mask : in Colour_Masks    := 0) return Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateRGBSurfaceFrom";
      use System.Storage_Elements;
      Pitch : constant Storage_Offset := Pixels (Index'Succ (Pixels'First (1)), Pixels'First (2))'Address
                                       - Pixels (Pixels'First (1), Pixels'First (2))'Address;
   begin
      Self.Internal := SDL_Create_RGB_Surface_From (
         Pixels     => Element_Pointer_C (Convert.To_Pointer (Pixels (Pixels'First (1), Pixels'First (2))'Address)),
         Width      => Pixels'Length (2),
         Height     => Pixels'Length (1),
         Depth      => Element'Size,
         Pitch      => C.int (Pitch),
         Red_Mask   => Red_Mask,
         Green_Mask => Green_Mask,
         Blue_Mask  => Blue_Mask,
         Alpha_Mask => Alpha_Mask);
   end Create_From_Array;

   --  TODO: SDL_CreateRGBSurfaceFrom

   --     procedure Create (Self : in out Surface; File_Name : in String) is
   --        --  This is actually a macro in the header.
   --        function SDL_Load_BMP (File_Name : in C.char_array) return Internal_Surface_Pointer with
   --          Import        => True,
   --          Convention    => C,
   --          External_Name => "SDL_LoadBMP";
   --     begin
   --        Self.Internal := SDL_Load_BMP (C.To_C (File_Name));
   --     end Create;

   function Get_Internal_Surface (Self : in Surface) return Internal_Surface_Pointer is
   begin
      return Self.Internal;
   end Get_Internal_Surface;

   function Make_Surface_From_Pointer (S : in Internal_Surface_Pointer; Owns : in Boolean := False) return Surface is
   begin
      return (Ada.Finalization.Controlled with Internal => S, Owns => Owns);
   end Make_Surface_From_Pointer;
end SDL.Video.Surfaces.Makers;
