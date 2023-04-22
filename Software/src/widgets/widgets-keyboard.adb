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

-- Gnav
with Gl.Colors;
use  Gl.Colors;
with Utility.Strings;
with Widgets.Button;
use  Widgets.Button;

--//////////////////////////////////////////////////////////////////////////////

--//////////////////////////////////////////////////////////////////////////////
package body Widgets.Keyboard is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The text that is being edited
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Text_Buffer : Utility.Strings.Dynamic_String := Utility.Strings.Empty_String;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Key_Button is new Button_Record with record
      
      Letter : String (1..1);
      
   end record;
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The buttons
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Buttons : array (Keyboard_Keys) of Key_Button;   
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The position and size of the keyboard on the screen
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Allocation : Allocation_Record;
      
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Setup is 
      
      A : Allocation_Record;
      C : Float := 0.0;

   begin
          
      A.W := Allocation.W / 10.0;
      A.H := Allocation.H /  4.0;
      
      -- First row: numbers
      --------------------------------------------------------------------------
           
      A.Y := Allocation.Y + 3.0 * A.H;        
      C   := 0.0; 
      
      for K in Key_0..Key_9 loop
           
         A.X := Allocation.X + C * A.W;
         C   := C + 1.0;
         
         Buttons (K).Set_Allocation (A);
         
      end loop;
           
      -- Second row: A to J
      --------------------------------------------------------------------------
           
      A.Y := Allocation.Y + 2.0 * A.H;        
      C   := 0.0; 
            
      for K in Key_A..Key_P loop
         
         A.X := Allocation.X + C * A.W;
         C   := C + 1.0;
         
         Buttons (K).Set_Allocation (A);
         
      end loop;
            
      -- Second row: K to T
      --------------------------------------------------------------------------
           
      A.Y := Allocation.Y + 1.0 * A.H;        
      C   := 0.0; 
            
      for K in Key_Q..Key_M loop
         
         A.X := Allocation.X + C * A.W;
         C   := C + 1.0;
         
         Buttons (K).Set_Allocation (A);
         
      end loop;
           
      -- Second row: U to Z, space, back, enter
      --------------------------------------------------------------------------
           
      A.Y := Allocation.Y;        
      C   := 0.0; 
            
      for K in Key_W..Key_Enter loop
         
         A.X := Allocation.X + C * A.W;
         C   := C + 1.0;
         
         Buttons (K).Set_Allocation (A);
         
      end loop;
          
   end Setup;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Allocation (Value : Allocation_Record) is 
   begin
      
      if Value /= Allocation then
         
         Allocation := Value;
         
         Setup;
         
      end if;
            
   end Set_Allocation;
   -----------------------------------------------------------------------------
   
   
   

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Init is 
   begin
      
      Buttons (Key_A).Letter := "A";
      Buttons (Key_B).Letter := "B";
      Buttons (Key_C).Letter := "C";
      Buttons (Key_D).Letter := "D";
      Buttons (Key_E).Letter := "E";
      Buttons (Key_F).Letter := "F";
      Buttons (Key_G).Letter := "G";
      Buttons (Key_H).Letter := "H";
      Buttons (Key_I).Letter := "I";
      Buttons (Key_J).Letter := "J";
      Buttons (Key_K).Letter := "K";
      Buttons (Key_L).Letter := "L";
      Buttons (Key_M).Letter := "M";
      Buttons (Key_N).Letter := "N";
      Buttons (Key_O).Letter := "O";
      Buttons (Key_P).Letter := "P";
      Buttons (Key_Q).Letter := "Q";
      Buttons (Key_R).Letter := "R";
      Buttons (Key_S).Letter := "S";
      Buttons (Key_T).Letter := "T";
      Buttons (Key_U).Letter := "U";
      Buttons (Key_V).Letter := "V";
      Buttons (Key_W).Letter := "W";
      Buttons (Key_X).Letter := "X";
      Buttons (Key_Y).Letter := "Y";
      Buttons (Key_Z).Letter := "Z";
      
      Buttons (Key_0).Letter := "0";
      Buttons (Key_1).Letter := "1";
      Buttons (Key_2).Letter := "2";
      Buttons (Key_3).Letter := "3";
      Buttons (Key_4).Letter := "4";
      Buttons (Key_5).Letter := "5";
      Buttons (Key_6).Letter := "6";
      Buttons (Key_7).Letter := "7";
      Buttons (Key_8).Letter := "8";
      Buttons (Key_9).Letter := "9";
            
      Buttons (Key_Minus).Letter := "-"; 
      Buttons (Key_Space).Letter := ">";      
      Buttons (Key_Back ).Letter := "<";
      Buttons (Key_Enter).Letter := "}";
      
      for K in Keyboard_Keys loop
         
         Buttons (K).Initialize;
         
         Buttons (K).Set_Label (Buttons (K).Letter);
         
         Buttons (K).Set_Label_Color (Color_White);

         Buttons (K).Set_Transparency (0.5);

         Buttons (K).Set_Font_Size (0.4, 0.4);

         Buttons (K).Set_Show_Border (True);
         
      end loop;
            
      for K in Key_0..Key_9 loop
         
         Buttons (K).Set_Background_Color (Color_Gray_2);

         Buttons (K).Set_Transparency (0.5);

      end loop;
            
      for K in Key_Space..Key_Enter loop
         
         Buttons (K).Set_Label_Color (Color_Gray_6);

         Buttons (K).Set_Background_Color (Color_Black);

         Buttons (K).Set_Transparency (0.5);

      end loop;
            
   end Init;
   -----------------------------------------------------------------------------
   
   
   

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw is 
   begin

      for K in Keyboard_Keys loop
         
         Buttons (K).Draw;

      end loop;
            
   end Draw;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Sets the original text
   --===========================================================================
   procedure Set_Text (Text : String) is 
      
      use Utility.Strings;
      
   begin
      
      Text_Buffer := +Text;
      
   end Set_Text;
   -----------------------------------------------------------------------------
      
   
   
   
   --===========================================================================
   -- Gets the adapted text
   --===========================================================================
   function Get_Text return String is 
      
      use Utility.Strings;
      
   begin
      
      return -Text_Buffer;
      
   end Get_Text;
   -----------------------------------------------------------------------------
   
   
   

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Key_Pressed (X, Y : Float) return Boolean is 
      
      use Utility.Strings;

   begin

      for K in Keyboard_Keys loop
         
         if Buttons (K).Contains (X, Y) then
         
            if K = Key_Back then
               
               declare
                  Text : String := -Text_Buffer;
               begin
                  
                  if Text'Length = 1 then
                     
                     Text_Buffer := Empty_String;
                     
                  elsif Text'Length > 0 then
                     
                     Text_Buffer := +Text (Text'First..Text'Last-1);
                     
                  end if;
                                    
               end;
                      
            elsif K = Key_Space then
               
               Text_Buffer := +(-Text_Buffer & " ");
            
            elsif K = Key_Enter then
               
               null;
            
            else
               
               Text_Buffer := +(-Text_Buffer & Buttons (K).Letter);
            
            end if;
            
            return True;
            
         end if;
         
      end loop;
      
      return False;
             
   end Key_Pressed;
   -----------------------------------------------------------------------------

   
   
   
end Widgets.Keyboard;
--------------------------------------------------------------------------------
