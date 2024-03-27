package body GUI.Cards_Table is

   overriding procedure Create
     (Table : in out Cards_Table_Type; Parent : in out Gnoga.Gui.Base.Base_Type'Class; ID : UXString := "")
   is
   begin
      Gnoga.Gui.Element.Table.Table_Type (Table).Create (Parent, ID);
      Table.Tabs_Row.Create (Table);
      Table.Cards_Row.Create (Table);
      Table.Cards.Create (Table.Cards_Row);
      Table.Tabs.Create (Table.Tabs_Row, Table.Cards);
      Table.Cards_Row.Style ("border", "medium solid black");
      Table.Style ("border-collapse", "collapse");
      Table.Tabs.Style ("margin-block-end", "9px");
   end Create;

   procedure Add_Tab
     (Table : in out Cards_Table_Type; Label : UXString; Card : access Gnoga.Gui.View.View_Base_Type'Class)
   is
   begin
      Table.Cards.Add_Card (Label, Card);
      Table.Tabs.Add_Tab (Label, Label);
   end Add_Tab;

end GUI.Cards_Table;
