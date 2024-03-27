with Gnoga.Gui.Element.Form;
with UXStrings; use UXStrings;

generic
   type T is (<>);
package GUI.Discrete_Inputs is

   type Discrete_Input is new Gnoga.Gui.Element.Form.Selection_Type with null record;

   overriding procedure Create
     (Element         : in out Discrete_Input;
      Form            : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Multiple_Select :        Boolean  := False;
      Visible_Lines   :        Positive := 1;
      Name            :        UXString := "";
      ID              :        UXString := "");

   function Get (Input : Discrete_Input) return T;

   procedure Set (Input : in out Discrete_Input; Value : T);

end GUI.Discrete_Inputs;
