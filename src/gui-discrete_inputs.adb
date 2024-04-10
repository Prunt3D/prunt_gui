package body GUI.Discrete_Inputs is

   overriding procedure Create
     (Element         : in out Discrete_Input;
      Form            : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Multiple_Select :        Boolean  := False;
      Visible_Lines   :        Positive := 1;
      Name            :        UXString := "";
      ID              :        UXString := "")
   is
   begin
      Gnoga.Gui.Element.Form.Selection_Type (Element).Create
        (Form => Form, Multiple_Select => Multiple_Select, Visible_Lines => Visible_Lines, Name => Name, ID => ID);

      for I in T loop
         Element.Add_Option (Value => UXStrings.From_UTF_8 (I'Image), Text => UXStrings.From_UTF_8 (I'Image));
      end loop;
   end Create;

   function Get (Input : Discrete_Input) return T is
   begin
      return T'Value (Input.Value.To_UTF_8);
   end Get;

   procedure Set (Input : in out Discrete_Input; Value : T) is
   begin
      for I in 1 .. Input.Length loop
         Input.Selected (I, Input.Value (I) = UXStrings.From_UTF_8 (Value'Image));
      end loop;
   end Set;

end GUI.Discrete_Inputs;
