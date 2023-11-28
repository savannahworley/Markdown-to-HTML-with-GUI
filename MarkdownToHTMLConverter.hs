
type Text = String 

data Token = Hash | Dash | Equal | Space | Ast 
    | UndScore | Lt | Num | Tab | Backslash 
    | LBra | RBra | Exclamation | BackTick 
    | LPar | RPar | Quote | Gt | PMD Elements 

data Elements = H1 Text | H2 Text | H3 Text 
    | H4 Text | H5 Text | H6 Text | Para Text
    | LnBreak | Bold Text | Italic Text | BoldAndItalic Text 
    | Blockquote Text | OrderedList [Elements] | ListItem 
    | UnorderedList [Elements] | Code Text | Image Text 
    | HorizontalRule | Link Text

    
