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
-- Provides a linkable object.
--//////////////////////////////////////////////////////////////////////////////
package body Stacks.Linked is

   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   function Get_Previous (This : Linked_Access) return Linked_Access is
   begin

      return This.Previous;

   end Get_Previous;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Set_Previous (This : Linked_Access; Item : Linked_Access) is
   begin

      This.Previous := Item;

   end Set_Previous;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Set_Next (This : Linked_Access; Item : Linked_Access) is
   begin

      This.Next := Item;

   end Set_Next;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   function Get_Next (This : Linked_Access) return Linked_Access is
   begin

      return This.Next;

   end Get_Next;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   overriding procedure Initialize (This : in out Linked_Record) is
   begin
      null;
   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   overriding procedure Adjust (This : in out Linked_Record) is
   begin
      null;
   end Adjust;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   overriding procedure Finalize (This : in out Linked_Record) is
   begin
      null;
   end Finalize;
   -----------------------------------------------------------------------------

end Stacks.Linked;
--------------------------------------------------------------------------------
