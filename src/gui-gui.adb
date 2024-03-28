with Ada.Unchecked_Deallocation;

package body GUI.GUI is

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Gnoga.Application.Multi_Connect.End_Application;
   end On_Exit;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : App_Access := new App_Data;

      procedure Free_Data is new Ada.Unchecked_Deallocation (App_Data, App_Access);
   begin
      Main_Window.Connection_Data (Data => App, Dynamic => False);

      App.Main_Window := Main_Window'Unchecked_Access;

      App.Main_Window.Disable_Auto_Set_View;

      App.Main_Table.Create (App.Main_Window.all);
      App.Main_Table.Place_Inside_Top_Of (App.Main_Window.Document.Body_Element.all);
      --  This avoids the window resizing our tables.

      --  Config Editor
      begin
         App.Config_Editor_Table.Create (App.Main_Table.Cards);
         App.Main_Table.Add_Tab ("Config Editor", App.Config_Editor_Table'Access);

         --  Config Editor > Prunt
         App.Config_Editor_Prunt_Widget.Create_Widget (App.Config_Editor_Table.Cards);
         App.Config_Editor_Table.Add_Tab ("Prunt", App.Config_Editor_Prunt_Widget'Access);

         --  Config Editor > Steppers
         begin
            App.Config_Editor_Steppers_Table.Create (App.Config_Editor_Table.Cards);
            App.Config_Editor_Table.Add_Tab ("Steppers", App.Config_Editor_Steppers_Table'Access);

            --  Config Editor > Steppers > X
            for I in App.Config_Editor_Stepper_Widgets'Range loop
               App.Config_Editor_Stepper_Widgets (I).Create_Widget (App.Config_Editor_Steppers_Table.Cards, I);
               App.Config_Editor_Steppers_Table.Add_Tab
                 (UXStrings.From_Latin_1 (I'Image), App.Config_Editor_Stepper_Widgets (I)'Access);
            end loop;
         end;

         --  Config Editor > Kinematics
         App.Config_Editor_Kinematics_Widget.Create_Widget (App.Config_Editor_Table.Cards);
         App.Config_Editor_Table.Add_Tab ("Kinematics", App.Config_Editor_Kinematics_Widget'Access);

         --  Config Editor > Input Switches
         begin
            App.Config_Editor_Input_Switches_Table.Create (App.Config_Editor_Table.Cards);
            App.Config_Editor_Table.Add_Tab ("Input Switches", App.Config_Editor_Input_Switches_Table'Access);

            --  Config Editor > Input Switches > X
            for I in App.Config_Editor_Input_Switch_Widgets'Range loop
               App.Config_Editor_Input_Switch_Widgets (I).Create_Widget
                 (App.Config_Editor_Input_Switches_Table.Cards, I);
               App.Config_Editor_Input_Switches_Table.Add_Tab
                 (UXStrings.From_Latin_1 (I'Image), App.Config_Editor_Input_Switch_Widgets (I)'Access);
            end loop;
         end;

         --  Config Editor > Homing
         begin
            App.Config_Editor_Homing_Table.Create (App.Config_Editor_Table.Cards);
            App.Config_Editor_Table.Add_Tab ("Homing", App.Config_Editor_Homing_Table'Access);

            --  Config Editor > Homing > X
            for I in App.Config_Editor_Homing_Widgets'Range loop
               App.Config_Editor_Homing_Widgets (I).Create_Widget (App.Config_Editor_Homing_Table.Cards, I);
               App.Config_Editor_Homing_Table.Add_Tab
                 (UXStrings.From_Latin_1 (I'Image), App.Config_Editor_Homing_Widgets (I)'Access);
            end loop;
         end;

         --  Config Editor > Extruder
         App.Config_Editor_Extruder_Widget.Create_Widget (App.Config_Editor_Table.Cards);
         App.Config_Editor_Table.Add_Tab ("Extruder", App.Config_Editor_Extruder_Widget'Access);

         --  Config Editor > Thermistors
         begin
            App.Config_Editor_Thermistors_Table.Create (App.Config_Editor_Table.Cards);
            App.Config_Editor_Table.Add_Tab ("Thermistors", App.Config_Editor_Thermistors_Table'Access);

            --  Config Editor > Thermistors > X
            for I in App.Config_Editor_Thermistor_Widgets'Range loop
               App.Config_Editor_Thermistor_Widgets (I).Create_Widget (App.Config_Editor_Thermistors_Table.Cards, I);
               App.Config_Editor_Thermistors_Table.Add_Tab
                 (UXStrings.From_Latin_1 (I'Image), App.Config_Editor_Thermistor_Widgets (I)'Access);
            end loop;
         end;

         --  Config Editor > Heaters
         begin
            App.Config_Editor_Heaters_Table.Create (App.Config_Editor_Table.Cards);
            App.Config_Editor_Table.Add_Tab ("Heaters", App.Config_Editor_Heaters_Table'Access);

            --  Config Editor > Heaters > X
            for I in App.Config_Editor_Heater_Widgets'Range loop
               App.Config_Editor_Heater_Widgets (I).Create_Widget (App.Config_Editor_Heaters_Table.Cards, I);
               App.Config_Editor_Heaters_Table.Add_Tab
                 (UXStrings.From_Latin_1 (I'Image), App.Config_Editor_Heater_Widgets (I)'Access);
            end loop;
         end;

         --  Config Editor > Bed_Mesh
         App.Config_Editor_Bed_Mesh_Widget.Create_Widget (App.Config_Editor_Table.Cards);
         App.Config_Editor_Table.Add_Tab ("Bed Mesh", App.Config_Editor_Bed_Mesh_Widget'Access);

         --  Config Editor > Fans
         begin
            App.Config_Editor_Fans_Table.Create (App.Config_Editor_Table.Cards);
            App.Config_Editor_Table.Add_Tab ("Fans", App.Config_Editor_Fans_Table'Access);

            --  Config Editor > Fans > X
            for I in App.Config_Editor_Fan_Widgets'Range loop
               App.Config_Editor_Fan_Widgets (I).Create_Widget (App.Config_Editor_Fans_Table.Cards, I);
               App.Config_Editor_Fans_Table.Add_Tab
                 (UXStrings.From_Latin_1 (I'Image), App.Config_Editor_Fan_Widgets (I)'Access);
            end loop;
         end;

         --  Config Editor > G-Code Assignment
         App.Config_Editor_G_Code_Assignment_Widget.Create_Widget (App.Config_Editor_Table.Cards);
         App.Config_Editor_Table.Add_Tab ("G-Code Assignment", App.Config_Editor_G_Code_Assignment_Widget'Access);

      end;

      --  Log
      begin
         App.Log_Widget.Create (App.Main_Table.Cards);
         App.Log_Widget.Style ("max-height", "500px");
         App.Main_Table.Add_Tab ("Log", App.Log_Widget'Access);
      end;

      Connection.Hold;

      --  Gnoga leaks memory if we allow it to handle freeing the data.
      Free_Data (App);
   end On_Connect;

   procedure Log_And_Switch_Tab (Object : Gnoga.Types.Pointer_to_Connection_Data_Class; Message : UXString) is
      function To_HTML (S : UXString) return UXString is
         function Translate_Character (C : Unicode_Character) return UXString is
         begin
            if C = Unicode_Character'Val (10) then
               return "</br>";
            elsif C = Unicode_Character'Val (13) then
               return "</br>";
            elsif C = '&' then
               return "&amp;";
            elsif C = '<' then
               return "&lt;";
            elsif C = '>' then
               return "&gt;";
            elsif C = '"' then
               return "&quot;";
            elsif C = ''' then
               return "&#39;";
            else
               return From_Unicode (C);
            end if;
         end Translate_Character;

         R : UXString;
      begin
         for C in S loop
            Append (R, Translate_Character (S (C)));
         end loop;

         return R;
      end To_HTML;

      procedure Inner (App : App_Access) is
      begin
         App.Log_Widget.Put_Line (To_HTML (Message));
         App.Log_Widget.New_Line;
         App.Main_Table.Tabs.Select_Tab ("Log");
      end Inner;
   begin
      Inner (App_Access (Object));
   end Log_And_Switch_Tab;

   procedure Run is
   begin
      Gnoga.Application.Title ("Prunt");
      Gnoga.Application.HTML_On_Close ("Prunt terminated. Reload this page to reconnect.");
      Gnoga.Application.Multi_Connect.Initialize;
      Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Unrestricted_Access, Path => "default");
      Gnoga.Application.Multi_Connect.Message_Loop;
   end Run;

end GUI.GUI;
