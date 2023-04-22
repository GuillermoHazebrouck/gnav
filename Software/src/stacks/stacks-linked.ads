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
with Ada.Finalization;

--//////////////////////////////////////////////////////////////////////////////
-- Provides a linkable object.
--//////////////////////////////////////////////////////////////////////////////
package Stacks.Linked is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents an object that can be linked from two sides the element of a
   -- chain.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Linked_Record is new Ada.Finalization.Controlled with private;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A reference to a linked object.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Linked_Access is access all Linked_Record'Class;

   --===========================================================================
   -- Gets the previous element from a linked object.
   --===========================================================================
   function Get_Previous (This : Linked_Access) return Linked_Access;

   --===========================================================================
   -- Sets the previous element from a linked object.
   --===========================================================================
   procedure Set_Previous (This : Linked_Access; Item : Linked_Access);

   --===========================================================================
   -- Gets the next element from a linked object.
   --===========================================================================
   function Get_Next (This : Linked_Access) return Linked_Access;

   --===========================================================================
   -- Sets the previous elementr from a linked object.
   --===========================================================================
   procedure Set_Next (This : Linked_Access; Item : Linked_Access);

   --===========================================================================
   -- Initialization.
   --===========================================================================
   overriding procedure Initialize (This : in out Linked_Record);

   --===========================================================================
   -- Adjustment.
   --===========================================================================
   overriding procedure Adjust (This : in out Linked_Record);

   --===========================================================================
   -- Finalization.
   --===========================================================================
   overriding procedure Finalize (This : in out Linked_Record);

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Linked_Record is new Ada.Finalization.Controlled with record

      Previous : Linked_Access := null;

      Next : Linked_Access := null;

   end record;

   pragma Convention (Convention => C, Entity => Linked_Record);

end Stacks.Linked;
--------------------------------------------------------------------------------
