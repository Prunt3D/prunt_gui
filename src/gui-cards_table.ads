with Gnoga.Gui.View;
with Gnoga.Gui.View.Card;
with Gnoga.Gui.Element.Table;
with Gnoga.Gui.Base;
with UXStrings; use UXStrings;

package GUI.Cards_Table is

   type Cards_Table_Type is new Gnoga.Gui.Element.Table.Table_Type with record
      Tabs      : aliased Gnoga.Gui.View.Card.Tab_Type;
      Cards     : aliased Gnoga.Gui.View.Card.Card_View_Type;
      Tabs_Row  : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Cards_Row : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
   end record;

   overriding procedure Create
     (Table : in out Cards_Table_Type; Parent : in out Gnoga.Gui.Base.Base_Type'Class; ID : UXString := "");

   procedure Add_Tab
     (Table : in out Cards_Table_Type; Label : UXString; Card : access Gnoga.Gui.View.View_Base_Type'Class);

end GUI.Cards_Table;
