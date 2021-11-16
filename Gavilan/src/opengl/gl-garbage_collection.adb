--------------------------------------------------------------------------------
-- LIBRARY_UNIT_NAME : Hmi.Gl_Helper
--
-- DESIGNER          : Guillermo HAZEBROUCK
--
-- CREATION_DATE     : 03 Dec 2019
--
-- LAST_MODIFICATION : 03 Dec 2019 Initial package creation
--------------------------------------------------------------------------------
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Interfaces.C;
with System;
-- Util
with Error;
-- Local
with Gl.Legacy;





--******************************************************************************
--
--******************************************************************************
package body Gl.Garbage_Collection is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Request_Buffer_Removal (Context : Context_Range; Buffer : Gl_Uint) is

      use Interfaces.C;

      Bag   : Garbage_Bag_Access;
      Index : Natural;

   begin

      if Buffer > 0 then

         Bag := Buffers (Context).Get_Last_Item;

         -- Create a new bag if there are no bags or the last bag is full
         -----------------------------------------------------------------------

         if Bag = null or else Bag.Count = Garbage_Bag_Size then
            Buffers (Context).Add_Item (Bag);
         end if;

         -- Put the item in the bag and increase the bag counter
         -----------------------------------------------------------------------

         Index             := Bag.Count + 1;

         Bag.Count         := Index;

         Bag.Items (Index) := Buffer;

      end if;

   end Request_Buffer_Removal;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Request_List_Removal (Context : Context_Range; List : Gl_Uint) is

      use Interfaces.C;

      Bag   : Garbage_Bag_Access;
      Index : Natural;

   begin

      if List > 0 then

         Bag := Lists (Context).Get_Last_Item;

         -- Create a new bag if there are no bags or the last bag is full
         -----------------------------------------------------------------------

         if Bag = null or else Bag.Count = Garbage_Bag_Size then
            Lists (Context).Add_Item (Bag);
         end if;

         -- Put the item in the bag and increase the bag counter
         -----------------------------------------------------------------------

         Index             := Bag.Count + 1;

         Bag.Count         := Index;

         Bag.Items (Index) := List;

      end if;

   end Request_List_Removal;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Free_Unused_Resources is

      Bag : Garbage_Bag_Access;

   begin

      -- Remove all the items from each bag in the buffers garbage
      --------------------------------------------------------------------------

      if Buffers (Active_Context).Get_Count > 0 then

         Bag := Buffers (Active_Context).Get_First_Item;

         while Bag /= null loop

            Gl.Delete_Buffers (Gl_Sizei (Bag.Count), Bag.Items'Unrestricted_Access);

            Log_Error ("while collecting resources");

            Buffers (Active_Context).Get_Next_Item (Bag);

         end loop;

         Buffers (Active_Context).Clear;

      end if;

      -- Remove all the items from each bag in the lists garbage
      --------------------------------------------------------------------------

      if Lists (Active_Context).Get_Count > 0 then

         Bag := Lists (Active_Context).Get_First_Item;

         while Bag /= null loop

            for I in 1..Bag.Count loop

               Gl.Legacy.Delete_Lists (Bag.Items (I), 1);

            end loop;

            Log_Error ("while collecting resources");

            Lists (Active_Context).Get_Next_Item (Bag);

         end loop;

         Lists (Active_Context).Clear;

      end if;

   end Free_Unused_Resources;
   -----------------------------------------------------------------------------

end Gl.Garbage_Collection;
--------------------------------------------------------------------------------
