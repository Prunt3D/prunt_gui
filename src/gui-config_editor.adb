with Physical_Types; use Physical_Types;
with PragmARC.Images;
with Ada.Exceptions;
with Ada.Strings;
with Gnoga.Gui.View;
use type Gnoga.Gui.View.Pointer_To_View_Base_Class;

package body GUI.Config_Editor is

   pragma Unsuppress (All_Checks);

   --  The default 'Image outputs scientific notion, which isn't very user friendly, so we use this instead.
   function Image (Number : Dimensioned_Float) return UXString is
      function PragmARC_Image is new PragmARC.Images.Float_Image (Dimensioned_Float);
      Raw : constant String := PragmARC_Image (Number, Fore => 1, Aft => Dimensioned_Float'Digits - 1, Exp => 0);
   begin
      for I in reverse Raw'Range loop
         if Raw (I) /= '0' then
            if Raw (I) = '.' then
               if I - Raw'First > Dimensioned_Float'Digits + 1 then
                  return UXStrings.From_Latin_1 (Number'Image).Trim (Ada.Strings.Both);
               else
                  return UXStrings.From_Latin_1 (Raw (Raw'First .. I + 1)).Trim (Ada.Strings.Both);
               end if;
            else
               return UXStrings.From_Latin_1 (Raw (Raw'First .. I)).Trim (Ada.Strings.Both);
            end if;
         end if;
      end loop;

      raise Program_Error;
   end Image;

   package body Basic_Inputs is

      function Get (Input : Length_Input) return Physical_Types.Length is
      begin
         return Physical_Types.Length'Value (Input.Value.To_Latin_1);
      end Get;

      procedure Set (Input : in out Length_Input; Value : Physical_Types.Length) is
      begin
         Input.Value (Image (Value));
      end Set;

      function Get (Input : Time_Input) return Time is
      begin
         return Time'Value (Input.Value.To_Latin_1);
      end Get;

      procedure Set (Input : in out Time_Input; Value : Time) is
      begin
         Input.Value (Image (Value));
      end Set;

      function Get (Input : Cruise_Ratio_Input) return Cruise_Ratio is
      begin
         return Cruise_Ratio'Value (Input.Value.To_Latin_1);
      end Get;

      procedure Set (Input : in out Cruise_Ratio_Input; Value : Cruise_Ratio) is
      begin
         Input.Value (Image (Value));
      end Set;

      function Get (Input : Temperature_Input) return Temperature is
      begin
         return Temperature'Value (Input.Value.To_Latin_1);
      end Get;

      procedure Set (Input : in out Temperature_Input; Value : Temperature) is
      begin
         Input.Value (Image (Value));
      end Set;

      function Get (Input : Dimensionless_Input) return Dimensionless is
      begin
         return Dimensionless'Value (Input.Value.To_Latin_1);
      end Get;

      procedure Set (Input : in out Dimensionless_Input; Value : Dimensionless) is
      begin
         Input.Value (Image (Value));
      end Set;

      function Get (Input : PWM_Scale_Input) return PWM_Scale is
      begin
         return PWM_Scale'Value (Input.Value.To_Latin_1);
      end Get;

      procedure Set (Input : in out PWM_Scale_Input; Value : PWM_Scale) is
      begin
         Input.Value (Image (Value));
      end Set;

      function Get (Input : Voltage_Input) return Voltage is
      begin
         return Voltage'Value (Input.Value.To_Latin_1);
      end Get;

      procedure Set (Input : in out Voltage_Input; Value : Voltage) is
      begin
         Input.Value (Image (Value));
      end Set;

      function Get (Input : Path_String_Input) return Path_Strings.Bounded_String is
      begin
         return Path_Strings.To_Bounded_String (Input.Value.To_Latin_1);
      end Get;

      procedure Set (Input : in out Path_String_Input; Value : Path_Strings.Bounded_String) is
      begin
         Input.Value (Value => UXStrings.From_Latin_1 (Path_Strings.To_String (Value)));
      end Set;

      function Get (Input : Boolean_Input) return Boolean is
      begin
         return Input.Checked;
      end Get;

      procedure Set (Input : in out Boolean_Input; Value : Boolean) is
      begin
         Input.Checked (Value);
      end Set;

   end Basic_Inputs;

   package body Parameter_Rows is

      overriding procedure Create
        (Row : in out Parameter_Row; Parent : in out Gnoga.Gui.Element.Element_Type'Class; ID : UXString := "")
      is
      begin
         Parent_Type (Row).Create (Parent, ID);
         Row.Name_Col.Create (Row);
         Row.Name.Create (Row.Name_Col);
         Row.Data_Col.Create (Row);
         Row.Description_Col.Create (Row);
         Row.Description.Create (Row.Description_Col);
         Row.Style ("border", "1px solid black");
      end Create;

      procedure Create
        (Row         : in out Parameter_Row;
         Parent      : in out Gnoga.Gui.Element.Element_Type'Class;
         Name        :        UXString;
         Description :        UXString;
         Data        : in out Gnoga.Gui.Element.Element_Type'Class;
         ID          :        UXString := "")
      is
      begin
         Row.Create (Parent, ID);
         Row.Name.Text (Name);
         Row.Description.Text (Description);
         Data.Place_Inside_Top_Of (Row.Data_Col);
      end Create;

   end Parameter_Rows;

   package body Grouped_Element_Widgets is

      procedure Create
        (Row    : in out Numeric_Row;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         Name   :        UXString;
         ID     :        UXString := "")
      is
      begin
         Row.Create (Parent, ID);
         Row.Name_Col.Create (Row);
         Row.Name.Create (Row.Name_Col);
         Row.Name.Text (Name);
         Row.Input_Col.Create (Row);
         Row.Input.Create (Form);
         Row.Input.Place_Inside_Top_Of (Row.Input_Col);
         Row.Style ("border", "1px solid black");
      end Create;

      procedure Create
        (Row    : in out Check_Box_Row;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         Name   :        UXString;
         ID     :        UXString := "")
      is
      begin
         Row.Create (Parent, ID);
         Row.Input_Col.Create (Row);
         Row.Input.Create (Form);
         Row.Input.Place_Inside_Top_Of (Row.Input_Col);
         Row.Name_Col.Create (Row);
         Row.Name.Create (Row.Name_Col);
         Row.Name.Text (Name);
         Row.Style ("border", "1px solid black");
      end Create;

      procedure Create
        (Widget : in out Position_Widget;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         ID     :        UXString := "")
      is
      begin
         Gnoga.Gui.Element.Table.Table_Type (Widget).Create (Parent, ID);
         for I in Widget.Rows'Range loop
            Widget.Rows (I).Create (Widget, Form, UXStrings.From_Latin_1 (I'Image & ":"));
         end loop;
      end Create;

      function Get (Widget : Position_Widget) return Physical_Types.Position is
         Pos : Physical_Types.Position;
      begin
         for I in Pos'Range loop
            Pos (I) := Physical_Types.Length'Value (Widget.Rows (I).Input.Value.To_Latin_1);
         end loop;

         return Pos;
      end Get;

      procedure Set (Widget : in out Position_Widget; Pos : Physical_Types.Position) is
      begin
         for I in Pos'Range loop
            Widget.Rows (I).Input.Value (Image (Pos (I)));
         end loop;
      end Set;

      procedure Create
        (Widget : in out Position_Scale_Widget;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         ID     :        UXString := "")
      is
      begin
         Gnoga.Gui.Element.Table.Table_Type (Widget).Create (Parent, ID);
         for I in Widget.Rows'Range loop
            Widget.Rows (I).Create (Widget, Form, UXStrings.From_Latin_1 (I'Image & ":"));
         end loop;
      end Create;

      function Get (Widget : Position_Scale_Widget) return Physical_Types.Position_Scale is
         Scale : Physical_Types.Position_Scale;
      begin
         for I in Scale'Range loop
            Scale (I) := Physical_Types.Dimensionless'Value (Widget.Rows (I).Input.Value.To_Latin_1);
         end loop;

         return Scale;
      end Get;

      procedure Set (Widget : in out Position_Scale_Widget; Scale : Physical_Types.Position_Scale) is
      begin
         for I in Scale'Range loop
            Widget.Rows (I).Input.Value (Image (Scale (I)));
         end loop;
      end Set;

      procedure Create
        (Widget : in out Kinematic_Limits_Widget;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         ID     :        UXString := "")
      is
      begin
         Gnoga.Gui.Element.Table.Table_Type (Widget).Create (Parent, ID);
         Widget.Tangential_Velocity_Max.Create (Widget, Form, UXStrings.From_Latin_1 ("Velocity:"));
         Widget.Acceleration_Max.Create (Widget, Form, UXStrings.From_Latin_1 ("Acceleration:"));
         Widget.Jerk_Max.Create (Widget, Form, UXStrings.From_Latin_1 ("Jerk:"));
         Widget.Snap_Max.Create (Widget, Form, UXStrings.From_Latin_1 ("Snap:"));
         Widget.Crackle_Max.Create (Widget, Form, UXStrings.From_Latin_1 ("Crackle:"));
         Widget.Chord_Error_Max.Create (Widget, Form, UXStrings.From_Latin_1 ("Deviation:"));
      end Create;

      function Get (Widget : Kinematic_Limits_Widget) return Motion_Planner.Kinematic_Limits is
      begin
         return
           (Tangential_Velocity_Max => Velocity'Value (Widget.Tangential_Velocity_Max.Input.Value.To_Latin_1),
            Acceleration_Max        => Acceleration'Value (Widget.Acceleration_Max.Input.Value.To_Latin_1),
            Jerk_Max                => Jerk'Value (Widget.Jerk_Max.Input.Value.To_Latin_1),
            Snap_Max                => Snap'Value (Widget.Snap_Max.Input.Value.To_Latin_1),
            Crackle_Max             => Crackle'Value (Widget.Crackle_Max.Input.Value.To_Latin_1),
            Chord_Error_Max         => Physical_Types.Length'Value (Widget.Chord_Error_Max.Input.Value.To_Latin_1));
      end Get;

      procedure Set (Widget : in out Kinematic_Limits_Widget; Limits : Motion_Planner.Kinematic_Limits) is
      begin
         Widget.Tangential_Velocity_Max.Input.Value (Image (Limits.Tangential_Velocity_Max));
         Widget.Acceleration_Max.Input.Value (Image (Limits.Acceleration_Max));
         Widget.Jerk_Max.Input.Value (Image (Limits.Jerk_Max));
         Widget.Snap_Max.Input.Value (Image (Limits.Snap_Max));
         Widget.Crackle_Max.Input.Value (Image (Limits.Crackle_Max));
         Widget.Chord_Error_Max.Input.Value (Image (Limits.Chord_Error_Max));
      end Set;

      procedure Create
        (Widget : in out Attached_Steppers_Widget;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         ID     :        UXString := "")
      is
      begin
         Gnoga.Gui.Element.Table.Table_Type (Widget).Create (Parent, ID);
         for I in Widget.Rows'Range loop
            Widget.Rows (I).Create (Widget, Form, UXStrings.From_Latin_1 (I'Image));
         end loop;
      end Create;

      function Get (Widget : Attached_Steppers_Widget) return Attached_Steppers is
         Steppers : Attached_Steppers;
      begin
         for I in Widget.Rows'Range loop
            Steppers (I) := Widget.Rows (I).Input.Checked;
         end loop;

         return Steppers;
      end Get;

      procedure Set (Widget : in out Attached_Steppers_Widget; Steppers : Attached_Steppers) is
      begin
         for I in Widget.Rows'Range loop
            Widget.Rows (I).Input.Checked (Steppers (I));
         end loop;
      end Set;

   end Grouped_Element_Widgets;

   package body Outer_Section_Widgets is

      procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         App   : constant Gnoga.Types.Pointer_to_Connection_Data_Class := Object.Connection_Data;
         Image : UXString;
         procedure Inner (Section : in out Outer_Section_Widget'Class) is
         begin
            Log_And_Switch_Tab (App, "Saving. Please wait.");
            Save_Data (Section, Image);
            Log_And_Switch_Tab (App, "Save done.");
            Log_And_Switch_Tab (App, Image);
            begin
               Log_And_Switch_Tab (App, "Please wait for read-back.");
               Read_Data (Section);
               Log_And_Switch_Tab (App, "Read-back done.");
               Log_And_Switch_Tab
                 (App,
                  "A restart is required to apply the new configuration. " &
                  "You may keep editing the configuration and further changes will also be saved.");
            exception
               when E : others =>
                  Log_And_Switch_Tab (App, "Read-back failed.");
                  Log_And_Switch_Tab (App, UXStrings.From_Latin_1 (Ada.Exceptions.Exception_Information (E)));
            end;
         exception
            when E : others =>
               Log_And_Switch_Tab (App, "Save failed.");
               Log_And_Switch_Tab (App, UXStrings.From_Latin_1 (Ada.Exceptions.Exception_Information (E)));
         end Inner;
      begin
         Inner (Outer_Section_Widget'Class (Object));
      end On_Submit;

   end Outer_Section_Widgets;

   package body Section_Widgets is

      procedure Create_Widget (View : in out Prunt_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class) is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Enabled_Input.Create (Form => View);
         View.Enabled_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Enabled:",
            Description => "If set, allow Prunt to start.",
            Data        => View.Enabled_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Prunt_Widget) is
         Params : Prunt_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         View.Enabled_Input.Set (Params.Enabled);
      end Read_Data;

      overriding procedure Save_Data (View : in out Prunt_Widget; Image : out UXString) is
         Params : Prunt_Parameters;
      begin
         Params.Enabled := View.Enabled_Input.Get;

         My_Config.Config_File.Write (Params);

         Image := UXStrings.From_Latin_1 (Params'Image);
      end Save_Data;

      procedure Create_Widget
        (View : in out Stepper_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class; Stepper : Stepper_Name)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Stepper := Stepper;

         View.Enabled_Input.Create (Form => View);
         View.Enabled_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Enabled:",
            Description =>
              "If set, enable outputs to the stepper and allows it to be used for motion. A stepper that is not enabled will never output any signals. On some boards the outputs may be high impedance in this state.",
            Data        => View.Enabled_Input);

         View.Invert_Direction_Input.Create (Form => View);
         View.Invert_Direction_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Invert_Direction:",
            Description => "If set, invert the direction signal output.",
            Data        => View.Invert_Direction_Input);

         View.Enabled_On_High_Input.Create (Form => View);
         View.Enabled_On_High_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Enabled_On_High:",
            Description => "If set, set the enable pin to high to enable the stepper.",
            Data        => View.Enabled_On_High_Input);

         View.Fault_On_High_Input.Create (Form => View);
         View.Fault_On_High_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Fault_On_High:",
            Description => "If set, high reading on fault pin indicates that the stepper is in fault state.",
            Data        => View.Fault_On_High_Input);

         View.Mm_Per_Step_Input.Create (Form => View);
         View.Mm_Per_Step_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Mm_Per_Step (mm):",
            Description =>
              "The distance moved by the stepper for each transition of the step signal. Note that this is the distance for each transition, not the distance for each complete cycle. For stepper drivers that only step on either the rising or falling edge, take the average value for each transition (mm per cycle / 2). Note that Prunt will always output steps in groups of two in the default configuration, so direction signals will always be output on the same step signal state.",
            Data        => View.Mm_Per_Step_Input);

         View.Direction_Setup_Time_Input.Create (Form => View);
         View.Direction_Setup_Time_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Direction_Setup_Time (s):",
            Description => "Time between any direction transition and any step transition on this stepper.",
            Data        => View.Direction_Setup_Time_Input);

         View.Step_Time_Input.Create (Form => View);
         View.Step_Time_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Step_Time (s):",
            Description =>
              "Time between any step transitions on this stepper. These limits will be enforced even if the commanded feedrate is faster, resulting in a discontinuity in acceleration if the limits are reached. The limits are also enforced if the step generator needs to catch up in case the step generation task is interrupted or is overloaded.",
            Data        => View.Step_Time_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Stepper_Widget) is
         Params : Stepper_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Stepper);

         View.Enabled_Input.Set (Params.Enabled);
         View.Invert_Direction_Input.Set (Params.Invert_Direction);
         View.Enabled_On_High_Input.Set (Params.Enabled_On_High);
         View.Fault_On_High_Input.Set (Params.Fault_On_High);
         View.Mm_Per_Step_Input.Set (Params.Mm_Per_Step);
         View.Direction_Setup_Time_Input.Set (Params.Direction_Setup_Time);
         View.Step_Time_Input.Set (Params.Step_Time);
      end Read_Data;

      overriding procedure Save_Data (View : in out Stepper_Widget; Image : out UXString) is
         Params : Stepper_Parameters;
      begin
         Params.Enabled              := View.Enabled_Input.Get;
         Params.Invert_Direction     := View.Invert_Direction_Input.Get;
         Params.Enabled_On_High      := View.Enabled_On_High_Input.Get;
         Params.Fault_On_High        := View.Fault_On_High_Input.Get;
         Params.Mm_Per_Step          := View.Mm_Per_Step_Input.Get;
         Params.Direction_Setup_Time := View.Direction_Setup_Time_Input.Get;
         Params.Step_Time            := View.Step_Time_Input.Get;

         My_Config.Config_File.Write (Params, View.Stepper);

         Image := UXStrings.From_Latin_1 (Params'Image);
      end Save_Data;

      procedure Create_Widget (View : in out Kinematics_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class) is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Lower_Pos_Limit_Input.Create (Parent => View.Widget_Table, Form => View);
         View.Lower_Pos_Limit_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Lower_Pos_Limit:",
            Description =>
              "Minimum position that the printer may move to. The E axis may be set to " &
              Image (Physical_Types.Length'First / 2.0) & " for effectively infinite range.",
            Data        => View.Lower_Pos_Limit_Input);

         View.Upper_Pos_Limit_Input.Create (Parent => View.Widget_Table, Form => View);
         View.Upper_Pos_Limit_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Upper_Pos_Limit:",
            Description =>
              "Maximum position that the printer may move to. The E axis may be set to " &
              Image (Physical_Types.Length'Last / 2.0) & " for effectively infinite range.",
            Data        => View.Upper_Pos_Limit_Input);

         View.Max_Limits_Input.Create (Parent => View.Widget_Table, Form => View);
         View.Max_Limits_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Max_Limits:",
            Description =>
              "Maximum motion profile limits. Any limits set higher than these by g-code will be clipped.",
            Data        => View.Max_Limits_Input);

         View.Starting_Limits_Input.Create (Parent => View.Widget_Table, Form => View);
         View.Starting_Limits_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Starting_Limits:",
            Description => "Starting motion profile limits before any limits are set by g-code.",
            Data        => View.Starting_Limits_Input);

         View.Planning_Scaler_Input.Create (Parent => View.Widget_Table, Form => View);
         View.Planning_Scaler_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Planning_Scaler:",
            Description =>
              "Inside the motion planner, all positions are multiples by this value before applying motion profile limits, allowing for different limits on different axes. You do not need to take this value in to account when setting position limits or mm per step values.",
            Data        => View.Planning_Scaler_Input);

         View.Minimum_Cruise_Ratio_Input.Create (Form => View);
         View.Minimum_Cruise_Ratio_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Minimum_Cruise_Ratio:",
            Description => "Minimum ratio of time in a move that must be spent at zero acceleration.",
            Data        => View.Minimum_Cruise_Ratio_Input);

         View.Z_Steppers_Input.Create (Parent => View.Widget_Table, Form => View);
         View.Z_Steppers_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Z_Steppers:",
            Description => "Steppers attached to the Z axis.",
            Data        => View.Z_Steppers_Input);

         View.E_Steppers_Input.Create (Parent => View.Widget_Table, Form => View);
         View.E_Steppers_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "E_Steppers:",
            Description => "Steppers attached to the E axis.",
            Data        => View.E_Steppers_Input);

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.Cartesian_Table.Create (View.Kind_Table);
         View.Cartesian_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Cartesian", View.Cartesian_Table'Access);

         View.X_Steppers_Input.Create (Parent => View.Cartesian_Table, Form => View);
         View.X_Steppers_Row.Create
           (Parent      => View.Cartesian_Table,
            Name        => "X_Steppers:",
            Description => "Steppers attached to the X axis.",
            Data        => View.X_Steppers_Input);

         View.Y_Steppers_Input.Create (Parent => View.Cartesian_Table, Form => View);
         View.Y_Steppers_Row.Create
           (Parent      => View.Cartesian_Table,
            Name        => "Y_Steppers:",
            Description => "Steppers attached to the Y axis.",
            Data        => View.Y_Steppers_Input);

         View.Core_XY_Table.Create (View.Kind_Table);
         View.Core_XY_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Core_XY", View.Core_XY_Table'Access);

         View.A_Steppers_Input.Create (Parent => View.Core_XY_Table, Form => View);
         View.A_Steppers_Row.Create
           (Parent      => View.Core_XY_Table,
            Name        => "A_Steppers:",
            Description => "Steppers attached to the A axis. X axis = 0.5 * (A + B), Y axis = 0.5 * (A – B).",
            Data        => View.A_Steppers_Input);

         View.B_Steppers_Input.Create (Parent => View.Core_XY_Table, Form => View);
         View.B_Steppers_Row.Create
           (Parent      => View.Core_XY_Table,
            Name        => "B_Steppers:",
            Description => "Steppers attached to the B axis. X axis = 0.5 * (A + B), Y axis = 0.5 * (A – B).",
            Data        => View.B_Steppers_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Kind_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Kinematics_Widget) is
         Params : Kinematics_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         case Params.Kind is
            when Cartesian_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Cartesian");
               View.X_Steppers_Input.Set (Params.X_Steppers);
               View.Y_Steppers_Input.Set (Params.Y_Steppers);
            when Core_XY_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Core_XY");
               View.A_Steppers_Input.Set (Params.A_Steppers);
               View.B_Steppers_Input.Set (Params.B_Steppers);
         end case;
         View.Lower_Pos_Limit_Input.Set (Params.Lower_Pos_Limit);
         View.Upper_Pos_Limit_Input.Set (Params.Upper_Pos_Limit);
         View.Max_Limits_Input.Set (Params.Max_Limits);
         View.Starting_Limits_Input.Set (Params.Starting_Limits);
         View.Planning_Scaler_Input.Set (Params.Planning_Scaler);
         View.Minimum_Cruise_Ratio_Input.Set (Params.Minimum_Cruise_Ratio);
         View.Z_Steppers_Input.Set (Params.Z_Steppers);
         View.E_Steppers_Input.Set (Params.E_Steppers);
      end Read_Data;

      overriding procedure Save_Data (View : in out Kinematics_Widget; Image : out UXString) is
         Params : Kinematics_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Cartesian_Table'Unrestricted_Access then
            Params            := (Kind => Cartesian_Kind, others => <>);
            Params.X_Steppers := View.X_Steppers_Input.Get;
            Params.Y_Steppers := View.Y_Steppers_Input.Get;
         elsif View.Kind_Table.Cards.Current_Card = View.Core_XY_Table'Unrestricted_Access then
            Params            := (Kind => Core_XY_Kind, others => <>);
            Params.A_Steppers := View.A_Steppers_Input.Get;
            Params.B_Steppers := View.B_Steppers_Input.Get;
         else
            raise Constraint_Error with "Kinematics type must be selected.";
         end if;

         Params.Lower_Pos_Limit      := View.Lower_Pos_Limit_Input.Get;
         Params.Upper_Pos_Limit      := View.Upper_Pos_Limit_Input.Get;
         Params.Max_Limits           := View.Max_Limits_Input.Get;
         Params.Starting_Limits      := View.Starting_Limits_Input.Get;
         Params.Planning_Scaler      := View.Planning_Scaler_Input.Get;
         Params.Minimum_Cruise_Ratio := View.Minimum_Cruise_Ratio_Input.Get;
         Params.Z_Steppers           := View.Z_Steppers_Input.Get;
         Params.E_Steppers           := View.E_Steppers_Input.Get;

         My_Config.Config_File.Write (Params);

         Image := UXStrings.From_Latin_1 (Params'Image);
      end Save_Data;

      procedure Create_Widget
        (View         : in out Input_Switch_Widget;
         Parent       : in out Gnoga.Gui.Base.Base_Type'Class;
         Input_Switch :        Input_Switch_Name)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Input_Switch := Input_Switch;

         View.Enabled_Input.Create (Form => View);
         View.Enabled_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Enabled:",
            Description => "If set, marks the switch as enabled so it may be used for homing.",
            Data        => View.Enabled_Input);

         View.Hit_On_High_Input.Create (Form => View);
         View.Hit_On_High_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Hit_On_High:",
            Description => "If set, the switch is considered to be hit when the related signal is high.",
            Data        => View.Hit_On_High_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Input_Switch_Widget) is
         Params : Input_Switch_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Input_Switch);

         View.Enabled_Input.Set (Params.Enabled);
         View.Hit_On_High_Input.Set (Params.Hit_On_High);
      end Read_Data;

      overriding procedure Save_Data (View : in out Input_Switch_Widget; Image : out UXString) is
         Params : Input_Switch_Parameters;
      begin
         Params.Enabled     := View.Enabled_Input.Get;
         Params.Hit_On_High := View.Hit_On_High_Input.Get;

         My_Config.Config_File.Write (Params, View.Input_Switch);

         Image := UXStrings.From_Latin_1 (Params'Image);
      end Save_Data;

      procedure Create_Widget
        (View : in out Homing_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class; Axis : Axis_Name)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Axis := Axis;

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.Double_Tap_Table.Create (View.Kind_Table);
         View.Double_Tap_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Double_Tap", View.Double_Tap_Table'Access);

         View.Switch_Input.Create (Form => View);
         View.Switch_Row.Create
           (Parent      => View.Double_Tap_Table,
            Name        => "Switch:",
            Description => "The switch used for homing this axis.",
            Data        => View.Switch_Input);

         View.First_Move_Distance_Input.Create (Form => View);
         View.First_Move_Distance_Row.Create
           (Parent      => View.Double_Tap_Table,
            Name        => "First_Move_Distance (mm):",
            Description =>
              "The minimum length of the first move, may be negative to home towards negative infinity. The axis will first move twice this far away from the homing direction iff the switch is hit. The axis will then move at least this far in total, and no further than this far after the switch is hit. The axis will then move twice this far away from the switch before the second move.",
            Data        => View.First_Move_Distance_Input);

         View.First_Move_Limits_Input.Create (Parent => View.Double_Tap_Table, Form => View);
         View.First_Move_Limits_Row.Create
           (Parent      => View.Double_Tap_Table,
            Name        => "First_Move_Limits:",
            Description => "The motion profile limits for the first move.",
            Data        => View.First_Move_Limits_Input);

         View.Second_Move_Distance_Input.Create (Form => View);
         View.Second_Move_Distance_Row.Create
           (Parent      => View.Double_Tap_Table,
            Name        => "Second_Move_Distance (mm):",
            Description =>
              "The minimum length of the second move, may be negative to home towards negative infinity, must be the same sign as the first distance. The axis will move at least this far in total, and no further than this far after the switch is hit.",
            Data        => View.Second_Move_Distance_Input);

         View.Second_Move_Limits_Input.Create (Parent => View.Double_Tap_Table, Form => View);
         View.Second_Move_Limits_Row.Create
           (Parent      => View.Double_Tap_Table,
            Name        => "Second_Move_Limits:",
            Description => "The motion profile limits for the second move.",
            Data        => View.Second_Move_Limits_Input);

         View.Switch_Position_Input.Create (Form => View);
         View.Switch_Position_Row.Create
           (Parent      => View.Double_Tap_Table,
            Name        => "Switch_Position (mm):",
            Description =>
              "The position that the axis is considered to be at when the switch is hit during the second move.",
            Data        => View.Switch_Position_Input);

         View.Set_To_Value_Table.Create (View.Kind_Table);
         View.Set_To_Value_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Set_To_Value", View.Set_To_Value_Table'Access);

         View.Value_Input.Create (Form => View);
         View.Value_Row.Create
           (Parent      => View.Set_To_Value_Table,
            Name        => "Value (mm):",
            Description =>
              "The value the axis is assumed to be at when the axis is homed. No homing move will occur in this mode.",
            Data        => View.Value_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Kind_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Homing_Widget) is
         Params : Homing_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Axis);

         case Params.Kind is
            when Double_Tap_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Double_Tap");
               View.Switch_Input.Set (Params.Switch);
               View.First_Move_Distance_Input.Set (Params.First_Move_Distance);
               View.First_Move_Limits_Input.Set (Params.First_Move_Limits);
               View.Second_Move_Distance_Input.Set (Params.Second_Move_Distance);
               View.Second_Move_Limits_Input.Set (Params.Second_Move_Limits);
               View.Switch_Position_Input.Set (Params.Switch_Position);
            when Set_To_Value_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Set_To_Value");
               View.Value_Input.Set (Params.Value);
         end case;
      end Read_Data;

      overriding procedure Save_Data (View : in out Homing_Widget; Image : out UXString) is
         Params : Homing_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Double_Tap_Table'Unrestricted_Access then
            Params                      := (Kind => Double_Tap_Kind, others => <>);
            Params.Switch               := View.Switch_Input.Get;
            Params.First_Move_Distance  := View.First_Move_Distance_Input.Get;
            Params.First_Move_Limits    := View.First_Move_Limits_Input.Get;
            Params.Second_Move_Distance := View.Second_Move_Distance_Input.Get;
            Params.Second_Move_Limits   := View.Second_Move_Limits_Input.Get;
            Params.Switch_Position      := View.Switch_Position_Input.Get;
         elsif View.Kind_Table.Cards.Current_Card = View.Set_To_Value_Table'Unrestricted_Access then
            Params       := (Kind => Set_To_Value_Kind, others => <>);
            Params.Value := View.Value_Input.Get;
         else
            raise Constraint_Error with "Homing type must be selected.";
         end if;

         My_Config.Config_File.Write (Params, View.Axis);

         Image := UXStrings.From_Latin_1 (Params'Image);
      end Save_Data;

      procedure Create_Widget (View : in out Extruder_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class) is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Nozzle_Diameter_Input.Create (Form => View);
         View.Nozzle_Diameter_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Nozzle_Diameter (mm):",
            Description => "The diameter of the nozzle.",
            Data        => View.Nozzle_Diameter_Input);

         View.Filament_Diameter_Input.Create (Form => View);
         View.Filament_Diameter_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Filament_Diameter (mm):",
            Description => "The diameter of the filament.",
            Data        => View.Filament_Diameter_Input);

         View.Starting_Pressure_Advance_Time_Input.Create (Form => View);
         View.Starting_Pressure_Advance_Time_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Starting_Pressure_Advance_Time (s):",
            Description =>
              "The initial pressure advance time before a value is set by g-code. There is no equivalent of smoothing time as used by other motion controllers as the use of crackle-limited motion planning prevents the discontinuity in velocity that would otherwise be present. There is instead a discontinuity in snap.",
            Data        => View.Starting_Pressure_Advance_Time_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Extruder_Widget) is
         Params : Extruder_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         View.Nozzle_Diameter_Input.Set (Params.Nozzle_Diameter);
         View.Filament_Diameter_Input.Set (Params.Filament_Diameter);
         View.Starting_Pressure_Advance_Time_Input.Set (Params.Starting_Pressure_Advance_Time);
      end Read_Data;

      overriding procedure Save_Data (View : in out Extruder_Widget; Image : out UXString) is
         Params : Extruder_Parameters;
      begin
         Params.Nozzle_Diameter                := View.Nozzle_Diameter_Input.Get;
         Params.Filament_Diameter              := View.Filament_Diameter_Input.Get;
         Params.Starting_Pressure_Advance_Time := View.Starting_Pressure_Advance_Time_Input.Get;

         My_Config.Config_File.Write (Params);

         Image := UXStrings.From_Latin_1 (Params'Image);
      end Save_Data;

      procedure Create_Widget
        (View : in out Thermistor_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class; Thermistor : Thermistor_Name)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Thermistor := Thermistor;

         View.Enabled_Input.Create (Form => View);
         View.Enabled_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Enabled:",
            Description => "If set, marks the thermistor as enabled so it may be used by a heater.",
            Data        => View.Enabled_Input);

         View.Minimum_Temperature_Input.Create (Form => View);
         View.Minimum_Temperature_Row.Create
           (Parent      => View.Widget_Table,
            Name        => UXStrings.From_UTF_8 ("Minimum_Temperature (°C):"),
            Description => "Any temperature below this value indicates a failure.",
            Data        => View.Minimum_Temperature_Input);

         View.Maximum_Temperature_Input.Create (Form => View);
         View.Maximum_Temperature_Row.Create
           (Parent      => View.Widget_Table,
            Name        => UXStrings.From_UTF_8 ("Maximum_Temperature (°C):"),
            Description => "Any temperature above this value indicates a failure.",
            Data        => View.Maximum_Temperature_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Thermistor_Widget) is
         Params : Thermistor_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Thermistor);

         View.Enabled_Input.Set (Params.Enabled);
         View.Minimum_Temperature_Input.Set (Params.Minimum_Temperature);
         View.Maximum_Temperature_Input.Set (Params.Maximum_Temperature);
      end Read_Data;

      overriding procedure Save_Data (View : in out Thermistor_Widget; Image : out UXString) is
         Params : Thermistor_Parameters;
      begin
         Params.Enabled             := View.Enabled_Input.Get;
         Params.Minimum_Temperature := View.Minimum_Temperature_Input.Get;
         Params.Maximum_Temperature := View.Maximum_Temperature_Input.Get;

         My_Config.Config_File.Write (Params, View.Thermistor);

         Image := UXStrings.From_Latin_1 (Params'Image);
      end Save_Data;

      procedure Create_Widget
        (View : in out Heater_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class; Heater : Heater_Name)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Heater := Heater;

         View.Thermistor_Input.Create (Form => View);
         View.Thermistor_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Thermistor:",
            Description => "Thermistor associated with this heater.",
            Data        => View.Thermistor_Input);

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.Disabled_Table.Create (View.Kind_Table);
         View.Disabled_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Disabled", View.Disabled_Table'Access);

         View.PID_Table.Create (View.Kind_Table);
         View.PID_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("PID", View.PID_Table'Access);

         View.Proportional_Scale_Input.Create (Form => View);
         View.Proportional_Scale_Row.Create
           (Parent      => View.PID_Table,
            Name        => "Proportional_Scale:",
            Description => "Coefficient for the proportional term.",
            Data        => View.Proportional_Scale_Input);

         View.Integral_Scale_Input.Create (Form => View);
         View.Integral_Scale_Row.Create
           (Parent      => View.PID_Table,
            Name        => "Integral_Scale:",
            Description => "Coefficient for the integral term.",
            Data        => View.Integral_Scale_Input);

         View.Derivative_Scale_Input.Create (Form => View);
         View.Derivative_Scale_Row.Create
           (Parent      => View.PID_Table,
            Name        => "Derivative_Scale:",
            Description => "Coefficient for the derivative term.",
            Data        => View.Derivative_Scale_Input);

         View.Bang_Bang_Table.Create (View.Kind_Table);
         View.Bang_Bang_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Bang_Bang", View.Bang_Bang_Table'Access);

         View.Max_Delta_Input.Create (Form => View);
         View.Max_Delta_Row.Create
           (Parent      => View.Bang_Bang_Table,
            Name        => UXStrings.From_UTF_8 ("Max_Delta (°C):"),
            Description =>
              "Maximum temperature below or above the target temperature where the heater will be switched on or off respectively.",
            Data        => View.Max_Delta_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Kind_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Heater_Widget) is
         Params : Heater_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Heater);

         case Params.Kind is
            when Disabled_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Disabled");
            when PID_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("PID");
               View.Proportional_Scale_Input.Set (Params.Proportional_Scale);
               View.Integral_Scale_Input.Set (Params.Integral_Scale);
               View.Derivative_Scale_Input.Set (Params.Derivative_Scale);
            when Bang_Bang_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Bang_Bang");
               View.Max_Delta_Input.Set (Params.Max_Delta);
         end case;
         View.Thermistor_Input.Set (Params.Thermistor);
      end Read_Data;

      overriding procedure Save_Data (View : in out Heater_Widget; Image : out UXString) is
         Params : Heater_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Disabled_Table'Unrestricted_Access then
            Params := (Kind => Disabled_Kind, others => <>);
         elsif View.Kind_Table.Cards.Current_Card = View.PID_Table'Unrestricted_Access then
            Params                    := (Kind => PID_Kind, others => <>);
            Params.Proportional_Scale := View.Proportional_Scale_Input.Get;
            Params.Integral_Scale     := View.Integral_Scale_Input.Get;
            Params.Derivative_Scale   := View.Derivative_Scale_Input.Get;
         elsif View.Kind_Table.Cards.Current_Card = View.Bang_Bang_Table'Unrestricted_Access then
            Params           := (Kind => Bang_Bang_Kind, others => <>);
            Params.Max_Delta := View.Max_Delta_Input.Get;
         else
            raise Constraint_Error with "Heater type must be selected.";
         end if;

         Params.Thermistor := View.Thermistor_Input.Get;

         My_Config.Config_File.Write (Params, View.Heater);

         Image := UXStrings.From_Latin_1 (Params'Image);
      end Save_Data;

      procedure Create_Widget (View : in out Bed_Mesh_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class) is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.No_Mesh_Table.Create (View.Kind_Table);
         View.No_Mesh_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("No_Mesh", View.No_Mesh_Table'Access);

         View.Beacon_Table.Create (View.Kind_Table);
         View.Beacon_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Beacon", View.Beacon_Table'Access);

         View.Serial_Port_Path_Input.Create (Form => View);
         View.Serial_Port_Path_Row.Create
           (Parent      => View.Beacon_Table,
            Name        => "Serial_Port_Path:",
            Description => "Path to the Beacon serial port.",
            Data        => View.Serial_Port_Path_Input);

         View.X_Offset_Input.Create (Form => View);
         View.X_Offset_Row.Create
           (Parent      => View.Beacon_Table,
            Name        => "X_Offset (mm):",
            Description => "Offset along the X axis of the probe to the nozzle.",
            Data        => View.X_Offset_Input);

         View.Y_Offset_Input.Create (Form => View);
         View.Y_Offset_Row.Create
           (Parent      => View.Beacon_Table,
            Name        => "Y_Offset (mm):",
            Description => "Offset along the Y axis of the probe to the nozzle.",
            Data        => View.Y_Offset_Input);

         View.Calibration_Floor_Input.Create (Form => View);
         View.Calibration_Floor_Row.Create
           (Parent      => View.Beacon_Table,
            Name        => "Calibration_Floor (mm):",
            Description => "Z axis value to use for lowest calibration point.",
            Data        => View.Calibration_Floor_Input);

         View.Calibration_Ceiling_Input.Create (Form => View);
         View.Calibration_Ceiling_Row.Create
           (Parent      => View.Beacon_Table,
            Name        => "Calibration_Ceiling (mm):",
            Description => "Z axis value to use for highest calibration point.",
            Data        => View.Calibration_Ceiling_Input);

         View.Calibration_Limits_Input.Create (Parent => View.Beacon_Table, Form => View);
         View.Calibration_Limits_Row.Create
           (Parent      => View.Beacon_Table,
            Name        => "Calibration_Limits:",
            Description =>
              "Motion profile limits to use during calibration. If using a scaler for the Z axis, you should increase this value to have a real velocity of around 1 mm/s.",
            Data        => View.Calibration_Limits_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Kind_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Bed_Mesh_Widget) is
         Params : Bed_Mesh_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         case Params.Kind is
            when No_Mesh_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("No_Mesh");
            when Beacon_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Beacon");
               View.Serial_Port_Path_Input.Set (Params.Serial_Port_Path);
               View.X_Offset_Input.Set (Params.X_Offset);
               View.Y_Offset_Input.Set (Params.Y_Offset);
               View.Calibration_Floor_Input.Set (Params.Calibration_Floor);
               View.Calibration_Ceiling_Input.Set (Params.Calibration_Ceiling);
               View.Calibration_Limits_Input.Set (Params.Calibration_Limits);
         end case;
      end Read_Data;

      overriding procedure Save_Data (View : in out Bed_Mesh_Widget; Image : out UXString) is
         Params : Bed_Mesh_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.No_Mesh_Table'Unrestricted_Access then
            Params := (Kind => No_Mesh_Kind, others => <>);
         elsif View.Kind_Table.Cards.Current_Card = View.Beacon_Table'Unrestricted_Access then
            Params                     := (Kind => Beacon_Kind, others => <>);
            Params.Serial_Port_Path    := View.Serial_Port_Path_Input.Get;
            Params.X_Offset            := View.X_Offset_Input.Get;
            Params.Y_Offset            := View.Y_Offset_Input.Get;
            Params.Calibration_Floor   := View.Calibration_Floor_Input.Get;
            Params.Calibration_Ceiling := View.Calibration_Ceiling_Input.Get;
            Params.Calibration_Limits  := View.Calibration_Limits_Input.Get;
         else
            raise Constraint_Error with "Bed_Mesh type must be selected.";
         end if;

         My_Config.Config_File.Write (Params);

         Image := UXStrings.From_Latin_1 (Params'Image);
      end Save_Data;

      procedure Create_Widget
        (View : in out Fan_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class; Fan : Fan_Name)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Fan := Fan;

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.Disabled_Table.Create (View.Kind_Table);
         View.Disabled_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Disabled", View.Disabled_Table'Access);

         View.Dynamic_PWM_Table.Create (View.Kind_Table);
         View.Dynamic_PWM_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Dynamic_PWM", View.Dynamic_PWM_Table'Access);

         View.Disable_Below_PWM_Input.Create (Form => View);
         View.Disable_Below_PWM_Row.Create
           (Parent      => View.Dynamic_PWM_Table,
            Name        => "Disable_Below_PWM:",
            Description => "Any set PWM ratio below this value will shut off power to the fan.",
            Data        => View.Disable_Below_PWM_Input);

         View.Max_PWM_Input.Create (Form => View);
         View.Max_PWM_Row.Create
           (Parent      => View.Dynamic_PWM_Table,
            Name        => "Max_PWM:",
            Description =>
              "The maximum PWM ratio of the fan, corresponding to 100% power setting in g-code or the UI.",
            Data        => View.Max_PWM_Input);

         View.Fixed_Voltage_Input.Create (Form => View);
         View.Fixed_Voltage_Row.Create
           (Parent      => View.Dynamic_PWM_Table,
            Name        => "Fixed_Voltage (V):",
            Description => "The constant voltage to output.",
            Data        => View.Fixed_Voltage_Input);

         View.Dynamic_Voltage_Table.Create (View.Kind_Table);
         View.Dynamic_Voltage_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Dynamic_Voltage", View.Dynamic_Voltage_Table'Access);

         View.Disable_Below_Voltage_Input.Create (Form => View);
         View.Disable_Below_Voltage_Row.Create
           (Parent      => View.Dynamic_Voltage_Table,
            Name        => "Disable_Below_Voltage (V):",
            Description => "Any set voltage below this voltage will shut off power to the fan.",
            Data        => View.Disable_Below_Voltage_Input);

         View.Max_Voltage_Input.Create (Form => View);
         View.Max_Voltage_Row.Create
           (Parent      => View.Dynamic_Voltage_Table,
            Name        => "Max_Voltage (V):",
            Description =>
              "The full scale voltage of the fan, corresponding to 100% power setting in g-code or the UI.",
            Data        => View.Max_Voltage_Input);

         View.Always_On_Table.Create (View.Kind_Table);
         View.Always_On_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Always_On", View.Always_On_Table'Access);

         View.Always_On_PWM_Input.Create (Form => View);
         View.Always_On_PWM_Row.Create
           (Parent      => View.Always_On_Table,
            Name        => "Always_On_PWM:",
            Description => "The constant PWM ratio to output.",
            Data        => View.Always_On_PWM_Input);

         View.Always_On_Voltage_Input.Create (Form => View);
         View.Always_On_Voltage_Row.Create
           (Parent      => View.Always_On_Table,
            Name        => "Always_On_Voltage (V):",
            Description => "The constant voltage to output.",
            Data        => View.Always_On_Voltage_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Kind_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Fan_Widget) is
         Params : Fan_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Fan);

         case Params.Kind is
            when Disabled_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Disabled");
            when Dynamic_PWM_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Dynamic_PWM");
               View.Disable_Below_PWM_Input.Set (Params.Disable_Below_PWM);
               View.Max_PWM_Input.Set (Params.Max_PWM);
               View.Fixed_Voltage_Input.Set (Params.Fixed_Voltage);
            when Dynamic_Voltage_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Dynamic_Voltage");
               View.Disable_Below_Voltage_Input.Set (Params.Disable_Below_Voltage);
               View.Max_Voltage_Input.Set (Params.Max_Voltage);
            when Always_On_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Always_On");
               View.Always_On_PWM_Input.Set (Params.Always_On_PWM);
               View.Always_On_Voltage_Input.Set (Params.Always_On_Voltage);
         end case;
      end Read_Data;

      overriding procedure Save_Data (View : in out Fan_Widget; Image : out UXString) is
         Params : Fan_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Disabled_Table'Unrestricted_Access then
            Params := (Kind => Disabled_Kind, others => <>);
         elsif View.Kind_Table.Cards.Current_Card = View.Dynamic_PWM_Table'Unrestricted_Access then
            Params                   := (Kind => Dynamic_PWM_Kind, others => <>);
            Params.Disable_Below_PWM := View.Disable_Below_PWM_Input.Get;
            Params.Max_PWM           := View.Max_PWM_Input.Get;
            Params.Fixed_Voltage     := View.Fixed_Voltage_Input.Get;
         elsif View.Kind_Table.Cards.Current_Card = View.Dynamic_Voltage_Table'Unrestricted_Access then
            Params                       := (Kind => Dynamic_Voltage_Kind, others => <>);
            Params.Disable_Below_Voltage := View.Disable_Below_Voltage_Input.Get;
            Params.Max_Voltage           := View.Max_Voltage_Input.Get;
         elsif View.Kind_Table.Cards.Current_Card = View.Always_On_Table'Unrestricted_Access then
            Params                   := (Kind => Always_On_Kind, others => <>);
            Params.Always_On_PWM     := View.Always_On_PWM_Input.Get;
            Params.Always_On_Voltage := View.Always_On_Voltage_Input.Get;
         else
            raise Constraint_Error with "Fan type must be selected.";
         end if;

         My_Config.Config_File.Write (Params, View.Fan);

         Image := UXStrings.From_Latin_1 (Params'Image);
      end Save_Data;

      procedure Create_Widget (View : in out G_Code_Assignment_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Bed_Heater_Input.Create (Form => View);
         View.Bed_Heater_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Bed_Heater:",
            Description => "The heater assigned to the bed in g-code.",
            Data        => View.Bed_Heater_Input);

         --  View.Chamber_Heater_Input.Create (Form => View);
         --  View.Chamber_Heater_Row.Create
         --    (Parent      => View.Widget_Table,
         --     Name        => "Chamber_Heater:",
         --     Description => "The heater assigned to the chamber in g-code.",
         --     Data        => View.Chamber_Heater_Input);

         View.Hotend_Heater_Input.Create (Form => View);
         View.Hotend_Heater_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Hotend_Heater:",
            Description => "The heater assigned to the hotend in g-code.",
            Data        => View.Hotend_Heater_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out G_Code_Assignment_Widget) is
         Params : G_Code_Assignment_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         View.Bed_Heater_Input.Set (Params.Bed_Heater);
         --  View.Chamber_Heater_Input.Set (Params.Chamber_Heater);
         View.Hotend_Heater_Input.Set (Params.Hotend_Heater);
      end Read_Data;

      overriding procedure Save_Data (View : in out G_Code_Assignment_Widget; Image : out UXString) is
         Params : G_Code_Assignment_Parameters;
      begin
         Params.Bed_Heater     := View.Bed_Heater_Input.Get;
         --  Params.Chamber_Heater := View.Chamber_Heater_Input.Get;
         Params.Hotend_Heater  := View.Hotend_Heater_Input.Get;

         My_Config.Config_File.Write (Params);

         Image := UXStrings.From_Latin_1 (Params'Image);
      end Save_Data;

   end Section_Widgets;

end GUI.Config_Editor;
