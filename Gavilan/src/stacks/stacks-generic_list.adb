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
with Ada.Unchecked_Deallocation;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Stacks.Generic_List is

   --===========================================================================
   -- Frees an item releasing it from memory
   --===========================================================================
   procedure Free_Item is new Ada.Unchecked_Deallocation (Object => T_Record'Class,
                                                          Name   => T_Access);

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Add_Item (This : in out Stack) is

      Next : T_Access := This.Head;

      Previous : T_Access;

      Item : T_Access;

      use Stacks.Linked;

      i : Positive := 1;

   begin

      if This.Count < Maximum then

         if This.Head /= null then

            loop

               if Next = null then

                  Item := new T_Record;

                  Set_Next(Linked_Access (Previous), Linked_Access (Item));

                  Set_Previous(Linked_Access (Item), Linked_Access (Previous));

                  This.Count := This.Count + 1;

                  if This.On_Item_Added /= null then

                     This.On_Item_Added (Item);

                  end if;

                  exit;

               else

                  Previous := Next;

                  Next := T_Access (Get_Next (Linked_Access (Next)));

               end if;

            end loop;

         else

            This.Head := new T_Record;

            This.Count := This.Count + 1;

            if This.On_Item_Added /= null then

               This.On_Item_Added (This.Head);

            end if;

         end if;

      else

         raise Stack_Full with "the stack reached the limit";

      end if;

   end Add_Item;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Add_Item (This : in out Stack; New_Item : out T_Access) is

      Next : T_Access := This.Head;

      Previous : T_Access;

      Item : T_Access;

      use Stacks.Linked;

   begin

      if This.Count < Maximum then

         if This.Head /= null then

            loop

               if Next = null then

                  Item := new T_Record;

                  Set_Next(Linked_Access (Previous), Linked_Access (Item));

                  Set_Previous(Linked_Access (Item), Linked_Access (Previous));

                  This.Count := This.Count + 1;

                  New_Item := Item;

                  if This.On_Item_Added /= null then

                     This.On_Item_Added (Item);

                  end if;

                  exit;

               else

                  Previous := Next;

                  Next := T_Access (Get_Next (Linked_Access (Next)));

               end if;

            end loop;

         else

            This.Head := new T_Record;

            This.Count := This.Count + 1;

            New_Item := This.Head;

            if This.On_Item_Added /= null then

               This.On_Item_Added (This.Head);

            end if;

         end if;

      else

         New_Item := null;

         raise Stack_Full with "the stack reached the limit";

      end if;

   end Add_Item;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Remove_Item (This : in out Stack; i : Positive) is

      Item : T_Access;

   begin

      --------------------------------------------------------------------------
      -- Note: If the index is out of range, Get_Item will raise an exeption.
      --------------------------------------------------------------------------

      Item := This.Get_Item (i);

      This.Remove_Item (Item);

   end Remove_Item;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Remove_Item (This : in out Stack; Item : in out T_Access) is

      Next : T_Access;

      Previous : T_Access;

      use Stacks.Linked;

      Item_Nullified : exception;

   begin

      if Item /= null then

         if This.On_Item_Removed /= null then

            This.On_Item_Removed (Item);

         end if;

         if Item = null then

            raise Item_Nullified with "item nullified before removal";

         else

            if Item = This.Current then

               This.Current := null;

            end if;

            Previous := T_Access (Get_Previous (Linked_Access (Item)));

            Next := T_Access (Get_Next (Linked_Access (Item)));

            if Previous /= null and Next /= null then

               Set_Next (Linked_Access (Previous), Linked_Access(Next));

               Set_Previous (Linked_Access(Next), Linked_Access (Previous));

            elsif Previous = null and Next /= null then

               Set_Previous (Linked_Access (Next), null);

               This.Head := Next;

            elsif Previous /= null and Next = null then

               Set_Next( Linked_Access(Previous), null);

            else

               This.Head := null;

            end if;

            Set_Next (Linked_Access (Item), null);

            Set_Previous (Linked_Access (Item), null);

            Free_Item (Item);

            Item := null;

            This.Count := This.Count - 1;

         end if;

      end if;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Insert_Item (This : in out Stack; i : Positive) is

      New_Item : T_Access;

      use Stacks.Linked;

   begin

      This.Insert_Item (i, New_Item);

   end Insert_Item;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Insert_Item (This : in out Stack; i : Positive; New_Item : in out T_Access) is

      j : Positive := 1;

      Next : T_Access := This.Head;

      Previous : T_Access := null;

      Item : T_Access := null;

      use Stacks.Linked;

   begin

      if This.Count < Maximum then

         if i = 1 then

            -- Add item as first element (Head).

            if This.Head /= null then

               Item := new T_Record;

               Set_Previous (Linked_Access(This.Head), Linked_Access(Item));

               Set_Next (Linked_Access(Item), Linked_Access(This.Head));

               This.Head := Item;

               This.Count := This.Count + 1;

            else

               Item := new T_Record;

               This.Count := This.Count + 1;

            end if;

         elsif i = This.Count + 1 then

            -- Add item as trailing element.

            This.Add_Item (Item);

         elsif i > This.Count + 1 then

            -- Out of range.

            raise Index_Out_Of_Range;

         else

            -- For any other case, put the item in the selected index and swap surrouding objects.

            loop

               -- Next is the Item at position j.

               if Next /= null then

                  if j = i then

                     -- Fetch the previous item

                     Previous := T_Access (Get_Previous (Linked_Access (Next)));

                     -- Allocate item in memory

                     Item := new T_Record;

                     -- Swap references to previous and next items

                     Set_Previous (Linked_Access (Next), Linked_Access (Item));

                     Set_Previous (Linked_Access (Item), Linked_Access (Previous));

                     Set_Next (Linked_Access (Previous), Linked_Access (Item));

                     Set_Next (Linked_Access (Item), Linked_Access (Next));

                     -- Increase counter

                     This.Count := This.Count + 1;

                     exit;

                  end if;

                  Next := T_Access (Get_Next (Linked_Access (Next)));

                  j := j + 1;

               else

                  -- It should never come to this situation, however, we raise an exeption.

                  raise Index_Out_Of_Range;

               end if;

            end loop;

         end if;

         New_Item := Item;

      else

         New_Item := null;

         raise Stack_Full with "the stack reached the limit";

      end if;

   end Insert_Item;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Item_Index (This : Stack; Item : T_Access) return Natural is

      Next  : T_Access := This.Head;
      Count : Natural  := 0;

   begin

      if Item = null then

         return 0;

      else

         while Next /= null loop

            Count := Count + 1;

            if Next = Item then

               return Count;

            end if;

            This.Get_Next_Item (Next);

         end loop;

         return 0;

      end if;

   end Get_Item_Index;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Clear (This : in out Stack) is

      Previous : T_Access;

      Next : T_Access := This.Head;

      use Stacks.Linked;

   begin

      if This.On_Cleared /= null then

         This.On_Cleared.all;

      end if;

      if This.Head /= null then

         while Next /= null loop

            Previous := Next;

            Next := T_Access (Get_Next (Linked_Access (Previous)));

            Free_Item (Previous);

            Previous := null;

         end loop;

      end if;

      This.Head := null;

      This.Current := null;

      This.Count := 0;

   end Clear;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Get_Next_Item (This : Stack; Item : in out T_Access) is

      use Stacks.Linked;

   begin

      if Item /= null then

         Item := T_Access (Linked.Get_Next (Linked_Access (Item)));

      else

         Item := null;

      end if;

   end Get_Next_Item;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Get_Previous_Item (This : Stack; Item : in out T_Access) is

      use Stacks.Linked;

   begin

      if Item /= null then

         Item := T_Access (Linked.Get_Previous (Linked_Access (Item)));

      else

         Item := null;

      end if;

   end Get_Previous_Item;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Current_To_First (This : in out Stack) is
   begin

      This.Current := This.Head;

   end Set_Current_To_First;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Current_To_Next (This : in out Stack) is

      use Stacks.Linked;

   begin

      if This.Current /= null then

         This.Current := T_Access (Get_Next (Linked_Access (This.Current)));

      end if;

   end Set_Current_To_Next;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Swap_To_First (This : in out Stack; Item : in out T_Access) is

      use Stacks.Linked;

      First, Second, Actual, Previous, Next : Linked_Access;

   begin

      if This.Count > 1 and Item /= null then

         First := Linked_Access (This.Head);

         Second := Get_Next (First);

         Actual := Linked_Access (Item);

         --
         -- If there is at least two items, and the given item is not already the
         -- first one
         --

         if Actual /= First and Second /= null then

            Previous := Get_Previous (Actual);

            Next := Get_Next (Actual);

            -- Bring first to the position of the actual

            Set_Previous (First, Previous);

            Set_Next (First, Next);

            if Next /= null then

               Set_Previous (Next, First);

            end if;

            if Previous /= null then

               Set_Next (Previous, First);

            end if;

            --

            Set_Previous (Actual, null);

            Set_Next (Actual, Second);

            Set_Previous (Second, Actual);

            This.Head := Item;

         end if;

      end if;

   end Swap_To_First;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Swap_To_Last (This : in out Stack; Item : in out T_Access) is

      use Stacks.Linked;

      Last, Penultimate, Actual, Previous, Next : Linked_Access;

   begin

      if This.Count > 1 and Item /= null then

         Last := Linked_Access (This.Get_Last_Item);

         Penultimate := Get_Previous (Last);

         Actual := Linked_Access (Item);

         --
         -- If there is at least two items, and the given item is not already the
         -- last one
         --

         if Actual /= Last and Penultimate /= null then

            Previous := Get_Previous (Actual);

            Next := Get_Next (Actual);

            -- Brinf last to the position of the actual

            Set_Previous (Last, Previous);

            Set_Next (Last, Next);

            if Next /= null then

               Set_Previous (Next, Last);

            end if;

            if Previous /= null then

               Set_Next (Previous, Last);

            end if;

            -- Bring actual to the position of the last one

            Set_Previous (Actual, Penultimate);

            Set_Next (Actual, null);

            Set_Next (Penultimate, Actual);

            if Previous = null then

               This.Head := T_Access (Last);

            end if;

         end if;

      end if;

   end Swap_To_Last;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Move_To_First (This : in out Stack; Item : in out T_Access) is

      use Stacks.Linked;

      First, Actual, Previous, Next : Linked_Access;

   begin

      if This.Count > 1 and Item /= null then

         First := Linked_Access (This.Head);

         Actual := Linked_Access (Item);

         --
         -- If there is at least two items, and the given item is not already the
         -- first one
         --

         if Actual /= First then

            Previous := Get_Previous (Actual);

            Next := Get_Next (Actual);

            if Next /= null then

               Set_Previous (Next, Previous);

            end if;

            if Previous /= null then

               Set_Next (Previous, Next);

            end if;

            --

            Set_Previous (First, Actual);

            Set_Previous (Actual, null);

            Set_Next (Actual, First);

            This.Head := Item;

         end if;

      end if;

   end Move_To_First;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Move_To_Last (This : in out Stack; Item : in out T_Access) is

      use Stacks.Linked;

      Last, Actual, Previous, Next : Linked_Access;

   begin

      if This.Count > 1 and Item /= null then

         Last := Linked_Access (This.Get_Last_Item);

         Actual := Linked_Access (Item);

         --
         -- If there is at least two items, and the given item is not already the
         -- last one
         --

         if Actual /= Last then

            Previous := Get_Previous (Actual);

            Next := Get_Next (Actual);

            if Next /= null then

               Set_Previous (Next, Previous);

            end if;

            if Previous /= null then

               Set_Next (Previous, Next);

            end if;

            Set_Next (Last, Actual);

            Set_Previous (Actual, Last);

            Set_Next (Actual, null);

            if Previous = null then

               This.Head := T_Access (Next);

            end if;

         end if;

      end if;

   end Move_To_Last;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Swap_Up (This : in out Stack; Item : in out T_Access) is

      use Stacks.Linked;

      Next_Item, First_Item, Last_Item : T_Access;

   begin

      if Item /= null then

         Next_Item := Item;

         This.Get_Next_Item (Next_Item);

         if Next_Item /= null then

            First_Item := Item;

            This.Get_Previous_Item (First_Item);

            Last_Item := Next_Item;

            This.Get_Next_Item (Last_Item);

            -- Item

            Set_Next (Linked_Access(Item), Linked_Access(Last_Item));

            Set_Previous (Linked_Access(Item), Linked_Access(Next_Item));

            -- Next

            Set_Next (Linked_Access(Next_Item), Linked_Access(Item));

            Set_Previous (Linked_Access(Next_Item), Linked_Access(First_Item));

            -- First

            if First_Item /= null then

               Set_Next (Linked_Access(First_Item), Linked_Access(Next_Item));

            else

               This.Head := Next_Item;

            end if;

            -- Last

            if Last_Item /= null then

               Set_Previous (Linked_Access(Last_Item), Linked_Access(Item));

            end if;

         end if;

      end if;

   end Swap_Up;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Swap_Down (This : in out Stack; Item : in out T_Access) is

      use Stacks.Linked;

      Previous_Item, First_Item, Last_Item : T_Access;

   begin

      if Item /= null then

         Previous_Item := Item;

         This.Get_Previous_Item (Previous_Item);

         if Previous_Item /= null then

            First_Item := Previous_Item;

            This.Get_Previous_Item (First_Item);

            Last_Item := Item;

            This.Get_Next_Item (Last_Item);

            -- Previous

            Set_Next (Linked_Access(Previous_Item), Linked_Access(Last_Item));

            Set_Previous (Linked_Access(Previous_Item), Linked_Access(Item));

            -- Item

            Set_Next (Linked_Access(Item), Linked_Access(Previous_Item));

            Set_Previous (Linked_Access(Item), Linked_Access(First_Item));

            -- First

            if First_Item /= null then

               Set_Next (Linked_Access(First_Item), Linked_Access(Item));

            else

               This.Head := Item;

            end if;

            -- Last

            if Last_Item /= null then

               Set_Previous (Linked_Access(Last_Item), Linked_Access(Previous_Item));

            end if;

         end if;

      end if;

   end Swap_Down;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Item (This : Stack; i : Positive) return T_Access is

      j : Positive := 1;

      Next : T_Access := This.Head;

      use Stacks.Linked;

   begin

      if This.Count > 0 then

         loop

            if Next /= null then

               if j = i then

                  return Next;

               else

                  Next := T_Access (Get_Next(Linked_Access (Next)));

                  j := j + 1;

               end if;

            else

               raise Index_Out_Of_Range;

               return null;

            end if;

         end loop;

      end if;

      raise Index_Out_Of_Range;

      return null;

   end Get_Item;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Current (This : Stack) return T_Access is
   begin

      return This.Current;

   end Get_Current;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_First_Item (This : Stack) return T_Access is
   begin

      return This.Head;

   end Get_First_Item;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Last_Item (This : Stack) return T_Access is

      Item      : T_Access := This.Head;

      Next_Item : T_Access := This.Head;

   begin

      while Next_Item /= null loop

         Item := Next_Item;

         This.Get_Next_Item (Next_Item);

      end loop;

      return Item;

   end Get_Last_Item;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Count (This : Stack) return Natural is
   begin

      return This.Count;

   end Get_Count;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Maximum (This : Stack) return Positive is
   begin

      return Maximum;

   end Get_Maximum;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Usage (This : Stack) return Natural is
   begin

      return (100 * This.Get_Count) / Maximum;

   end Get_Usage;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Add_On_Item_Added_Signal_Handler (This : in out Stack; Signal : T_Access_Callback) is
   begin

      --------------------------------------------------------------------------
      -- Note: if the stack is full, a trace will be logged by the callback loader
      --------------------------------------------------------------------------

      This.On_Item_Added := Signal;

   end Add_On_Item_Added_Signal_Handler;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Add_On_Item_Removed_Signal_Handler (This : in out Stack; Signal : T_Access_Callback) is
   begin

      --------------------------------------------------------------------------
      -- Note: if the stack is full, a trace will be logged by the callback loader
      --------------------------------------------------------------------------

      This.On_Item_Removed := Signal;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Add_On_Cleared_Signal_Handler (This : in out Stack; Signal : Callback) is
   begin

      --------------------------------------------------------------------------
      -- Note: if the stack is full, a trace will be logged by the callback loader
      --------------------------------------------------------------------------

      This.On_Cleared :=  (Signal);

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Initialize (This : in out Stack) is
   begin

      null;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Adjust (This : in out Stack) is
   begin

      null;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Finalize (This : in out Stack) is
   begin

      This.Clear;

   end Finalize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Free_Stack (Stack_To_Free : in out Stack_Access) is
   begin

      Free_Stack_Procedure (Stack_To_Free);

   end Free_Stack;
   -----------------------------------------------------------------------------


end Stacks.Generic_List;
--------------------------------------------------------------------------------
