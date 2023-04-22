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
--  SDL.Video.Surfaces.Makers
--
--  Functions to create surface objects.
--------------------------------------------------------------------------------------------------------------------
with System.Storage_Elements;

package SDL.Video.Surfaces.Makers is
   pragma Preelaborate;

   procedure Create (Self       : in out Surface;
                     Size       : in SDL.Sizes;
                     BPP        : in Pixel_Depths;
                     Red_Mask   : in Colour_Masks;
                     Blue_Mask  : in Colour_Masks;
                     Green_Mask : in Colour_Masks;
                     Alpha_Mask : in Colour_Masks);

   generic
       type Element is private;
       type Element_Pointer is access all Element;
   procedure Create_From (Self       : in out Surface;
                          Pixels     : in Element_Pointer;
                          Size       : in SDL.Sizes;
                          BPP        : in Pixel_Depths := Element'Size;
                          Pitch      : in System.Storage_Elements.Storage_Offset;
                          Red_Mask   : in Colour_Masks;
                          Green_Mask : in Colour_Masks;
                          Blue_Mask  : in Colour_Masks;
                          Alpha_Mask : in Colour_Masks);

   generic
      type Element is private;
      type Index is (<>);
      type Element_Array is array (Index range <>, Index range <>) of Element;
   procedure Create_From_Array (Self       : in out Surface;
                                Pixels     : access Element_Array;
                                Red_Mask   : in Colour_Masks;
                                Green_Mask : in Colour_Masks;
                                Blue_Mask  : in Colour_Masks;
                                Alpha_Mask : in Colour_Masks);
   --  Note: I'm unsure what happen when packed (1- or -4bit) arrays are used here.
   --        So, at least check that they have whole number of bytes per row
   --        (E. g. even width in 4-bit)
   --  Note: There may be issue with 24-bit pixels (does SDL imply 4-byte alignment in this case?)

   --  TODO: This is likely a temporary place for this. It's likely I will add a Streams package.
   --     procedure Create (Self : in out Surface; File_Name : in String);
private
   function Get_Internal_Surface (Self : in Surface) return Internal_Surface_Pointer with
     Export     => True,
     Convention => Ada;

   --  Create a surface from an internal pointer, this pointer will be owned by something else, so we don't delete it.
   function Make_Surface_From_Pointer (S : in Internal_Surface_Pointer; Owns : in Boolean := False) return Surface with
     Export     => True,
     Convention => Ada;

   --  TODO: SDL_ConvertSurface
   --  TODO: SDL_ConvertSurfaceFormat
   --  TODO: SDL_CreateRGBSurfaceFrom
end SDL.Video.Surfaces.Makers;
