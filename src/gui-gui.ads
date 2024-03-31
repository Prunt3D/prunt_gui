with Physical_Types; use Physical_Types;
with Config.Config;
private with GUI.Config_Editor;
with Gnoga.Types;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Element.Table;
with GUI.Cards_Table; use GUI.Cards_Table;
with UXStrings;       use UXStrings;

generic
   with package My_Config is new Config.Config (<>);
   with function Get_Status_Message return String;
   with function Get_Position return Physical_Types.Position;
   --  with procedure Submit_Gcode (Command : String; Result : out String);
package GUI.GUI is

   procedure Run;

private

   procedure Log_And_Switch_Tab (Object : Gnoga.Types.Pointer_to_Connection_Data_Class; Message : UXString);

   package My_Config_Editor is new Config_Editor (My_Config => My_Config, Log_And_Switch_Tab => Log_And_Switch_Tab);

   type Stepper_Widgets is array (My_Config.Stepper_Name) of aliased My_Config_Editor.Section_Widgets.Stepper_Widget;
   type Input_Switch_Widgets is
     array (My_Config.Input_Switch_Name) of aliased My_Config_Editor.Section_Widgets.Input_Switch_Widget;
   type Homing_Widgets is array (Axis_Name) of aliased My_Config_Editor.Section_Widgets.Homing_Widget;
   type Thermistor_Widgets is
     array (My_Config.Thermistor_Name) of aliased My_Config_Editor.Section_Widgets.Thermistor_Widget;
   type Heater_Widgets is array (My_Config.Heater_Name) of aliased My_Config_Editor.Section_Widgets.Heater_Widget;
   type Fan_Widgets is array (My_Config.Fan_Name) of aliased My_Config_Editor.Section_Widgets.Fan_Widget;

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      Main_Window : aliased Gnoga.Gui.Window.Pointer_To_Window_Class;

      Loading_Div : aliased Gnoga.Gui.Element.Common.DIV_Type;

      Main_Table : aliased Cards_Table_Type;

      Config_Editor_Table                    : aliased Cards_Table_Type;
      Config_Editor_Prunt_Widget             : aliased My_Config_Editor.Section_Widgets.Prunt_Widget;
      Config_Editor_Steppers_Table           : aliased Cards_Table_Type;
      Config_Editor_Stepper_Widgets          : aliased Stepper_Widgets;
      Config_Editor_Kinematics_Widget        : aliased My_Config_Editor.Section_Widgets.Kinematics_Widget;
      Config_Editor_Input_Switches_Table     : aliased Cards_Table_Type;
      Config_Editor_Input_Switch_Widgets     : aliased Input_Switch_Widgets;
      Config_Editor_Homing_Table             : aliased Cards_Table_Type;
      Config_Editor_Homing_Widgets           : aliased Homing_Widgets;
      Config_Editor_Extruder_Widget          : aliased My_Config_Editor.Section_Widgets.Extruder_Widget;
      Config_Editor_Thermistors_Table        : aliased Cards_Table_Type;
      Config_Editor_Thermistor_Widgets       : aliased Thermistor_Widgets;
      Config_Editor_Heaters_Table            : aliased Cards_Table_Type;
      Config_Editor_Heater_Widgets           : aliased Heater_Widgets;
      Config_Editor_Bed_Mesh_Widget          : aliased My_Config_Editor.Section_Widgets.Bed_Mesh_Widget;
      Config_Editor_Fans_Table               : aliased Cards_Table_Type;
      Config_Editor_Fan_Widgets              : aliased Fan_Widgets;
      Config_Editor_G_Code_Assignment_Widget : aliased My_Config_Editor.Section_Widgets.G_Code_Assignment_Widget;

      Log_Widget : aliased Gnoga.Gui.View.Console.Console_View_Type;

      Status_Table         : aliased Gnoga.Gui.Element.Table.Table_Type;
      Status_Message_Row   : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Status_Message_Text  : aliased Gnoga.Gui.Element.Common.DIV_Type;
      Status_Position_Row  : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Status_Position_Text : aliased Gnoga.Gui.Element.Common.DIV_Type;
   end record;

   type App_Access is access all App_Data;

   task type Status_Updater is
      entry Start (In_App : App_Access);
      entry Stop;
   end Status_Updater;

end GUI.GUI;
