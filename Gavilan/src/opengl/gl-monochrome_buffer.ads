--------------------------------------------------------------------------------
-- LIBRARY_UNIT_NAME : Gl.Monochrome_Buffer
--
-- DESIGNER          : Guillermo HAZEBROUCK
--
-- CREATION_DATE     : 16 Dec 2019
--
-- LAST_MODIFICATION : 16 Dec 2019 Extracted from Gl.Helper
--------------------------------------------------------------------------------




--******************************************************************************
-- This package provides a way to load vertices in a buffer and to manage
-- its resouce in a static way (once created, the resource will persist).
-- Each Buffer_Object points to a single resouces in the GPU.
--
-- NOTE:
-- Space is allocated to load the data in main memory, and a procedure is given
-- to load the whole data packet at once. However, note that the data is
-- package-wide and not inside each object! You can only use one object of a
-- same buffer at a time.
--
-- NOTE:
-- Calling Reset is required before loading data into the buffer to set the
-- internal offset back to 0.
--******************************************************************************
generic

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The maximum number of vertices that can be stored
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Size : Positive;

package Gl.Monochrome_Buffer is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A buffer object (protected against overwriting attempts).
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Buffer_Object is tagged limited private;

   --===========================================================================
   -- Resets the counter to 0.
   --===========================================================================
   procedure Reset;

   --===========================================================================
   -- Loads the given vertex (2 floats).
   --===========================================================================
   procedure Load_Vertex (X, Y : Float);

   --===========================================================================
   -- Loads the given vertex (converted to 2 floats).
   --===========================================================================
   procedure Load_Vertex (X, Y : Integer);

   --===========================================================================
   -- Updates the resource using the current buffer data.
   --===========================================================================
   procedure Update (This : in out Buffer_Object);

   --===========================================================================
   -- Removes the buffer from the GPU (if there is one)
   --===========================================================================
   procedure Free (This : in out Buffer_Object);

   --===========================================================================
   -- Binds the resource and calls the drawing method using the given primitive.
   -- NOTE: this method draws the whole buffer.
   --===========================================================================
   procedure Draw (This : Buffer_Object; Mode : Gl_Enum);

   --===========================================================================
   -- Binds the resource and calls the drawing method using the given primitive.
   -- Only the vertices between first and first + count are used.
   -- NOTE: the index First is 0-based.
   --===========================================================================
   procedure Draw (This : Buffer_Object; Mode : Gl_Enum; First : Gl_Int; Count : Gl_Sizei);

   --===========================================================================
   -- A facility function to remind that this resource should be updated later.
   -- The flag is turned off when calling Update.
   --===========================================================================
   procedure Mark_Refresh_Pending (This : in out Buffer_Object);

   --===========================================================================
   -- Indicates if this resource was marked for later refresh.
   --===========================================================================
   function Refresh_Pending (This : Buffer_Object) return Boolean;

   --===========================================================================
   -- The number of vertices
   --===========================================================================
   function Get_Count (This : Buffer_Object) return Natural;

   --===========================================================================
   -- The current number of vertices
   --===========================================================================
   function Get_Count return Natural;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The current data (valid in the range 1 to Offset)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Buffer : Gl_Float_Vec (1..2 * Size) := (others => 0.0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The current number of items
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Offset : Natural := 0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The current number of vertices
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Count  : Natural := 0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Buffer_Object is tagged limited record

      -- The identification of the resource in the GPU
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Id      : Gl_Uint := 0;

      -- The sice of the buffer in the GPU
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Size    : Natural := 0;

      -- The number of vertices that were loaded
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Count   : Natural := 0;

      -- Indicates if the buffer should be updated the next time
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Pending : Boolean := True;

   end record;

end Gl.Monochrome_Buffer;
--------------------------------------------------------------------------------
