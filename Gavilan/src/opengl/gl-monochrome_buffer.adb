--------------------------------------------------------------------------------
-- LIBRARY_UNIT_NAME : Hmi.Static_Buffer
--
-- DESIGNER          : Guillermo HAZEBROUCK
--
-- CREATION_DATE     : 16 Dec 2019
--
-- LAST_MODIFICATION : 16 Dec 2019 Extracted from Gl.Helper
--------------------------------------------------------------------------------
-- External
with Ada.Text_IO;
with Interfaces.C;
-- Local
with Gl.Resources;





--******************************************************************************
--
--******************************************************************************
package body Gl.Monochrome_Buffer is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Reset is
   begin

      Offset := 0;

      Count  := 0;

   end Reset;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Vertex (X, Y : Float) is
   begin

      if Count < Size then

         Offset := Offset + 1;
         Buffer (Offset) := Gl_Float (X);

         Offset := Offset + 1;
         Buffer (Offset) := Gl_Float (Y);

         Count := Count + 1;

      else

         Ada.Text_IO.Put_Line ("insufficient buffer size");

      end if;

   end Load_Vertex;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Vertex (X, Y : Integer) is
   begin

      if Count < Size then

         Offset := Offset + 1;
         Buffer (Offset) := Gl_Float (X);

         Offset := Offset + 1;
         Buffer (Offset) := Gl_Float (Y);

         Count := Count + 1;

      else

         Ada.Text_IO.Put_Line ("insufficient buffer size");

      end if;

   end Load_Vertex;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Offset return Natural is
   begin

      return Offset;

   end Get_Offset;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Update (This : in out Buffer_Object) is
   begin

      This.Size    := Offset;

      This.Count   := Count;

      This.Pending := False;

      Gl.Resources.Update_Resource (This.Id, Buffer'Unrestricted_Access, Offset);

   end Update;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Free (This : in out Buffer_Object) is

      use type Interfaces.C.unsigned;

      Ids : aliased Gl_Uint_Vec := (1 => This.Id);

   begin

      if This.Id > 0 then

         This.Id      := 0;

         This.Size    := 0;

         This.Count   := 0;

         This.Pending := False;

         Gl.Delete_Buffers (1, Ids'Access);

      end if;

   end Free;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (This : Buffer_Object; Mode : Gl_Enum) is

      use Interfaces.C;

   begin

      if This.Id > 0 and then This.Count > 0 then

         Gl.Bind_Buffer (GL_ARRAY_BUFFER, This.Id);

         Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

         Gl.Draw_Arrays (Mode, 0, Gl_Sizei (This.Count));

      end if;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (This : Buffer_Object; Mode : Gl_Enum; First : Gl_Int; Count : Gl_Sizei) is

      use Interfaces.C;

   begin

      if This.Id > 0 then

         if First >= 0 and then First + Count <= Gl_Int (This.Count) then

            Gl.Bind_Buffer (GL_ARRAY_BUFFER, This.Id);

            Gl.Vertex_Attrib_Pointer (0, 2, GL_TYPE_FLOAT, GL_FALSE, 0);

            Gl.Draw_Arrays (Mode, First, Count);

         else

            Ada.Text_IO.Put_Line ("sub buffer index out of range");

         end if;

      end if;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Count (This : Buffer_Object) return Natural is
   begin

      return This.Count;

   end Get_Count;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Mark_Refresh_Pending (This : in out Buffer_Object) is
   begin

      This.Pending := True;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Refresh_Pending (This : Buffer_Object) return Boolean is
   begin

      return This.Pending;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- The current number of vertices
   --===========================================================================
   function Get_Count return Natural is
   begin
      return Count;
   end;
   -----------------------------------------------------------------------------

end Gl.Monochrome_Buffer;
--------------------------------------------------------------------------------
