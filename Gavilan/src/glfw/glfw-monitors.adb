--//////////////////////////////////////////////////////////////////////////////
-- G-NAV PROJECT
-- Developed by Guillermo HAZEBROUCK - gahazebrouck@gmail.com
--\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- This file is part of "G-NAV".
-- The original code has been extracted from OpenGLAda, (c) 2017 Felix Krause
-- released under the terms of the MIT license.
-- Adaptation by Guillermo Hazebrouck.
--------------------------------------------------------------------------------

-- Depencencies
--//////////////////////////////////////////////////////////////////////////////

-- Standard
with Interfaces.C.Strings;
-- Local
with Glfw.Api;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Glfw.Monitors is

   --===========================================================================
   --
   --===========================================================================
   function Monitors return Monitor_List is
      use type Api.Address_List_Pointers.Pointer;
      Count : aliased Interfaces.C.int;
      Raw : constant Api.Address_List_Pointers.Pointer :=
        Api.Get_Monitors (Count'Access);
   begin
      if Raw /= null then
         declare
            List : constant Api.Address_List := Api.Address_List_Pointers.Value
              (Raw, Interfaces.C.ptrdiff_t (Count));
            Ret : Monitor_List (List'Range);
         begin
            for I in List'Range loop
               Ret (I).Handle := List (I);
            end loop;
            return Ret;
         end;
      else
         raise Operation_Exception;
      end if;
   end Monitors;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Primary_Monitor return Monitor is
      use type System.Address;
      Raw : constant System.Address := Api.Get_Primary_Monitor;
   begin
      if Raw /= System.Null_Address then
         return Monitor'(Handle => Raw);
      else
         raise Operation_Exception;
      end if;
   end Primary_Monitor;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Get_Position (Object : Monitor; X, Y : out Integer) is
      X_Raw, Y_Raw : Interfaces.C.int;
   begin
      Api.Get_Monitor_Pos (Object.Handle, X_Raw, Y_Raw);
      X := Integer (X_Raw);
      Y := Integer (Y_Raw);
   end Get_Position;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Get_Physical_Size (Object : Monitor;
                                Width, Height : out Integer) is
      Width_Raw, Height_Raw : Interfaces.C.int;
   begin
      Api.Get_Monitor_Physical_Size (Object.Handle, Width_Raw, Height_Raw);
      Width := Integer (Width_Raw);
      Height := Integer (Height_Raw);
   end Get_Physical_Size;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Name (Object : Monitor) return String is
   begin
      return Interfaces.C.Strings.Value (Api.Get_Monitor_Name (Object.Handle));
   end Name;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Video_Modes (Object : Monitor) return Video_Mode_List is
      use type Api.VMode_List_Pointers.Pointer;
      Count : aliased Interfaces.C.int;
      Raw   : constant Api.VMode_List_Pointers.Pointer
        := Api.Get_Video_Modes (Object.Handle, Count'Access);
   begin
      if Raw /= null then
         return Api.VMode_List_Pointers.Value (Raw,
                                               Interfaces.C.ptrdiff_t (Count));
      else
         raise Operation_Exception;
      end if;
   end Video_Modes;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Current_Video_Mode (Object : Monitor) return Video_Mode is
   begin
      return Api.Get_Video_Mode (Object.Handle).all;
   end Current_Video_Mode;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Gamma (Object : Monitor; Gamma : Float) is
   begin
      Api.Set_Gamma (Object.Handle, Interfaces.C.C_float (Gamma));
   end Set_Gamma;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Current_Gamma_Ramp (Object : Monitor) return Gamma_Ramp is
      Raw : constant access constant Api.Raw_Gamma_Ramp
        := Api.Get_Gamma_Ramp (Object.Handle);

      procedure UShort_To_Gamma_List (Source : Api.Unsigned_Short_List;
                                      Target : in out Gamma_Value_Array) is
      begin
         for I in Source'Range loop
            Target (I) := Source (I);
         end loop;
      end UShort_To_Gamma_List;
   begin
      return Ret : Gamma_Ramp (Integer (Raw.Size)) do
         UShort_To_Gamma_List (Api.Unsigned_Short_List_Pointers.Value
                               (Raw.Red, Interfaces.C.ptrdiff_t (Raw.Size)),
                               Ret.Red);
         UShort_To_Gamma_List (Api.Unsigned_Short_List_Pointers.Value
                               (Raw.Green, Interfaces.C.ptrdiff_t (Raw.Size)),
                               Ret.Green);
         UShort_To_Gamma_List (Api.Unsigned_Short_List_Pointers.Value
                               (Raw.Blue, Interfaces.C.ptrdiff_t (Raw.Size)),
                               Ret.Blue);
      end return;
   end Current_Gamma_Ramp;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Gamma_Ramp (Object : Monitor; Value : Gamma_Ramp) is
      Raw : aliased Api.Raw_Gamma_Ramp;
   begin
      Raw.Size  := Interfaces.C.unsigned (Value.Size);
      Raw.Red   := Value.Red   (1)'Unrestricted_Access;
      Raw.Green := Value.Green (1)'Unrestricted_Access;
      Raw.Blue  := Value.Blue  (1)'Unrestricted_Access;

      Api.Set_Gamma_Ramp (Object.Handle, Raw'Access);
   end Set_Gamma_Ramp;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Raw_Pointer (Object : Monitor) return System.Address is
   begin
      return Object.Handle;
   end Raw_Pointer;
   -----------------------------------------------------------------------------

end Glfw.Monitors;
--------------------------------------------------------------------------------

