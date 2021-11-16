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
with Ada.Unchecked_Deallocation;
-- Gnav
with Stacks.Linked;

--//////////////////////////////////////////////////////////////////////////////
-- Represents a generic double-linked list where items can be dynamically
-- allocated and deallocated.
-- Items must inherit the Linked_Record type defined in the Linked package.
-- The index is 1-based.
--//////////////////////////////////////////////////////////////////////////////
generic

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Contained type (linked object).
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type T_Record is new Stacks.Linked.Linked_Record with private;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Access to the contained type (linked object).
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type T_Access is access all T_Record'Class;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Maximum number of elements.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Maximum : Positive := 100000;

package Stacks.Generic_List is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The signature of a callback used to announce changes in the list
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type T_Access_Callback is access procedure (Argument : T_Access);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The signature of a callback used to announce changes in the list
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Callback is access procedure;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a stack containing elements of T_Record.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stack is new Ada.Finalization.Controlled with private;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Access to the stack.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stack_Access is access all Stack'Class;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Exception raised when the stack is full
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Stack_Full : exception;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Exception raised when attempting to access an item out of the list.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Index_Out_Of_Range : exception;

   --===========================================================================
   -- Adds a new trailing item to the list. If the stack is full, it raises
   -- Stack_Full exception.
   --===========================================================================
   procedure Add_Item (This : in out Stack);

   --===========================================================================
   -- Adds a new trailing item to the list and returns an access to it. If the
   -- stack is full, it raises Stack_Full exception and returns null.
   --===========================================================================
   procedure Add_Item (This : in out Stack; New_Item : out T_Access);

   --===========================================================================
   -- Removes the selected item from the list and frees its allocated memory.
   -- When removing an object, take special care with remaining dangling
   -- pointers, since they will not point to null. One way to handle this is by
   -- having a group of cleaning procedures connected to On_Item_Removed and
   -- On_Cleared signals in every package where references are kept long-term.
   -- Attempting to use a dangling pointer will cause an exeption.
   --===========================================================================
   procedure Remove_Item (This : in out Stack; I : Positive);

   --===========================================================================
   -- Removes the selected item from the list and frees its allocated memory.
   -- When removing an object, take special care with remaining dangling
   -- pointers since they will not point to null (see overloaded procedure).
   --===========================================================================
   procedure Remove_Item (This : in out Stack; Item : in out T_Access);

   --===========================================================================
   -- Inserts a new item to the list at the specified index.
   -- If the index is out of the current range plus one, an exeption is raised.
   -- If the index is equal to the number of items plus one, a new trailing item
   -- is added.
   -- If the stack is full, it raises Stack_Full exception.
   --===========================================================================
   procedure Insert_Item (This : in out Stack; i : Positive);

   --===========================================================================
   -- Inserts a new item to the list at the specified index.
   -- If the index is out of the current range plus one, an exeption is raised.
   -- If the index is equal to the number of items plus one, a new trailing item
   -- is added.
   -- If the stack is full, it raises Stack_Full exception and returns null.
   --===========================================================================
   procedure Insert_Item (This : in out Stack; i : Positive; New_Item : in out T_Access);

   --===========================================================================
   -- Returns the index of the given item in the stack. If the item does not
   -- belong to the stack, it returns 0.
   --===========================================================================
   function Get_Item_Index (This : Stack; Item : T_Access) return Natural;

   --===========================================================================
   -- Clears the list and frees the allocated memory.
   -- When clearing the list, take special care with remaining dangling pointers
   -- since they will not point to null.
   -- Attempting to use a dangling pointer will cause an exeption (see comments
   -- on Remove_Item).
   --===========================================================================
   procedure Clear (This : in out Stack);

   --===========================================================================
   -- Gets the item next to the provided one. Warning: this procedure does not
   -- check if the given item belongs to this stack.
   -- Don't ever mix references to different staks while looping!
   --===========================================================================
   procedure Get_Next_Item (This : Stack; Item : in out T_Access);

   --===========================================================================
   -- Gets the item preceding to the provided one.
   --===========================================================================
   procedure Get_Previous_Item (This : Stack; Item : in out T_Access);

   --===========================================================================
   -- Sets the first item as the current item. Don't use this for multithreading
   -- or nested loops!
   --===========================================================================
   procedure Set_Current_To_First (This : in out Stack);

   --===========================================================================
   -- Sets the current item as the next item of the current. Don't use this for
   -- multithreading or nested loops!
   --===========================================================================
   procedure Set_Current_To_Next (This : in out Stack);

   --===========================================================================
   -- Puts the item in the head of the list, and the former head in the position
   -- of the item.
   --===========================================================================
   procedure Swap_To_First (This : in out Stack; Item : in out T_Access);

   --===========================================================================
   -- Puts the item in the back of the list, and the former last in the position
   -- of the item.
   --===========================================================================
   procedure Swap_To_Last (This : in out Stack; Item : in out T_Access);

   --===========================================================================
   -- Puts the item in the head of the list, moving the stack upwards.
   --===========================================================================
   procedure Move_To_First (This : in out Stack; Item : in out T_Access);

   --===========================================================================
   -- Puts the item in the back of the list, moving the stack downwards.
   --===========================================================================
   procedure Move_To_Last (This : in out Stack; Item : in out T_Access);

   --===========================================================================
   -- Swaps the given item to the end of the stack.
   --===========================================================================
   procedure Swap_Up (This : in out Stack; Item : in out T_Access);

   --===========================================================================
   -- Swaps the given item to the begin of the stack.
   --===========================================================================
   procedure Swap_Down (This : in out Stack; Item : in out T_Access);

   --===========================================================================
   -- Gets an item from the list at the specific index i.
   -- If the index is out of range, an exeption is raised.
   --===========================================================================
   function Get_Item (This : Stack; i : Positive) return T_Access;

   --===========================================================================
   -- Gets the current item in the list.
   -- Warning: don't use this for multithreading or nested loops!
   --===========================================================================
   function Get_Current (This : Stack) return T_Access;

   --===========================================================================
   -- Gets the first item in the list.
   --===========================================================================
   function Get_First_Item (This : Stack) return T_Access;

   --===========================================================================
   -- Gets the last item in the list.
   --===========================================================================
   function Get_Last_Item (This : Stack) return T_Access;

   --===========================================================================
   -- Gets the number of items in the list.
   --===========================================================================
   function Get_Count (This : Stack) return Natural;

   --===========================================================================
   -- Gets the maximum number of items in the list.
   --===========================================================================
   function Get_Maximum (This : Stack) return Positive;

   --===========================================================================
   -- Gets the usage of the stack relative to its maximum capacity (%).
   --===========================================================================
   function Get_Usage (This : Stack) return Natural;

   --===========================================================================
   -- Assigns a delegate to the On_Item_Added signal. The argument of the
   -- callback is a reference to the new item.
   --===========================================================================
   procedure Add_On_Item_Added_Signal_Handler (This : in out Stack; Signal : T_Access_Callback);

   --===========================================================================
   -- Assigns a delegate to the On_Item_Removed signal. The argument of the
   -- callback is a reference to the item that is going to be removed.
   --===========================================================================
   procedure Add_On_Item_Removed_Signal_Handler (This : in out Stack; Signal : T_Access_Callback);

   --===========================================================================
   -- Assigns a delegate to the On_Cleared signal.
   --===========================================================================
   procedure Add_On_Cleared_Signal_Handler (This : in out Stack; Signal : Callback);

   --===========================================================================
   -- Handles the initialization of an instance.
   --===========================================================================
   overriding procedure Initialize (This : in out Stack);

   --===========================================================================
   -- Handles the adjustment of an instance.
   --===========================================================================
   overriding procedure Adjust (This : in out Stack);

   --===========================================================================
   -- Handles the finalization of an instance.
   --===========================================================================
   overriding procedure Finalize (This : in out Stack);

   --===========================================================================
   -- Frees the memory occupied by the a Stack.
   --===========================================================================
   procedure Free_Stack (Stack_To_Free : in out Stack_Access);


private

   type Stack is new Ada.Finalization.Controlled with record

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      -- First item of the list.
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Head : T_Access := null;

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      -- The current item.
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Current : T_Access := null;

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      -- Number of items in the list.
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Count : Natural := 0;

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      -- Signals that handle the addition of an item to the list.
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      On_Item_Added : T_Access_Callback;

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      -- Signals that handle the removal of an item outside the list.
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      On_Item_Removed : T_Access_Callback;

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      -- Signals that handle the clearance of the list.
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      On_Cleared : Callback;

   end record;

   procedure Free_Stack_Procedure is new Ada.Unchecked_Deallocation(Object => Stack'Class, Name => Stack_Access);


end Stacks.Generic_List;
--------------------------------------------------------------------------------
