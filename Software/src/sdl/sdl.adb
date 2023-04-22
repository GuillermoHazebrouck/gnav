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
package body SDL is

   function Initialise (Flags : in Init_Flags := Enable_Everything) return Boolean is
      function SDL_Init (Flags : in Init_Flags := Enable_Everything) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Init";

      Result : constant C.int := SDL_Init (Flags);
   begin
      return (Result = Success);
   end Initialise;

   function Initialise_Sub_System (Flags : in Init_Flags) return Boolean is
      function SDL_Init_Sub_System (Flags : in Init_Flags) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_InitSubSystem";

      Result : constant C.int := SDL_Init_Sub_System (Flags);
   begin
      return (Result = Success);
   end Initialise_Sub_System;

   function SDL_Was_Initialised (Flags : in Init_Flags := Null_Init_Flags) return Init_Flags with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_WasInit";

   function Was_Initialised return Init_Flags is
   begin
      return SDL_Was_Initialised;
   end Was_Initialised;

   function Was_Initialised (Flags : in Init_Flags) return Boolean is
   begin
      return (SDL_Was_Initialised (Flags) = Flags);
   end Was_Initialised;

   function To_Bool (Value : in Boolean) return SDL_Bool is
   begin
      return (if Value then SDL_True else SDL_False);
   end To_Bool;

end SDL;
