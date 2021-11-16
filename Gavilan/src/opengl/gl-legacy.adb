--------------------------------------------------------------------------------
-- LIBRARY_UNIT_NAME : Gl.Legacy
--
-- DESIGNER          : Guillermo HAZEBROUCK
--
-- CREATION_DATE     : 24 Mar 2020
--
-- LAST_MODIFICATION : 24 Mar 2020 Initial package creation
--------------------------------------------------------------------------------





--******************************************************************************
--
--******************************************************************************
package body Gl.Legacy is

   --===========================================================================
   --
   --===========================================================================
   procedure Load_Matrix (Matrix : Gl_Mat_4) is

      procedure glLoadMatrixf (First : access Gl_Float);
      pragma Import (Stdcall, glLoadMatrixf, "glLoadMatrixf");
      Local : aliased Gl_Mat_4 := Matrix;

   begin

      glLoadMatrixf (Local (Local'First)'Unrestricted_Access);

   end Load_Matrix;
   -----------------------------------------------------------------------------

end Gl.Legacy;
--------------------------------------------------------------------------------
