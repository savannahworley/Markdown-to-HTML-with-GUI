import Data.Char

type Text = String 

data Token = Hash | Dash | Equal | SpaceChar | Ast 
    | UndScore | Lt | Num | Tab | Backslash 
    | LBra | RBra | Exclamation | BackTick 
    | LPar | RPar | Quote | Gt | PMD Elements | Err Text
    | ContentText Text

data Elements = H1 Text | H2 Text | H3 Text 
    | H4 Text | H5 Text | H6 Text | Para Text
    | LnBreak | Bold Text | Italic Text | BoldAndItalic Text 
    | Blockquote Text | OrderedList [Elements] | ListItem 
    | UnorderedList [Elements] | Code Text | Image Text 
    | HorizontalRule | Link Text

--need to fix Space, BackTick, PMD??

lexer :: String -> [Token]
lexer "" = []
lexer ('#' : xs) = Hash : lexer xs 
lexer ('-' : xs) = Dash : lexer xs 
lexer ('=' : xs) = Equal : lexer xs 
lexer (x:xs) | isSpace x = SpaceChar : lexer xs 
lexer ('*' : xs) = Ast : lexer xs 
lexer ('_' : xs) = UndScore : lexer xs 
lexer ('<' : xs) = Lt : lexer xs 
lexer ('\t' : xs) = Tab : lexer xs 
lexer ('\\' : xs) = Backslash : lexer xs 
lexer ('{' : xs) = LBra : lexer xs 
lexer ('}' : xs) = RBra : lexer xs 
lexer ('!' : xs) = Exclamation : lexer xs 
lexer ('`' : xs) = BackTick : lexer xs 
lexer ('(' : xs) = LPar : lexer xs 
lexer (')' : xs) = RPar : lexer xs 
lexer ('"' : xs) = Quote : lexer xs 
lexer ('>' : xs) = Gt : lexer xs 
lexer (x:xs) | isDigit x = let (y,z) = parseNum (x:xs)
                              in Num : lexer z
lexer (x:xs) | isAlpha x = let (y,z) = span isAlphaNum xs 
                            in ContentText (read y) : lexer z
lexer xs = [Err xs]

parseNum :: String -> (String,String)
parseNum xs =
  let (x,x') = span isDigit xs
   in case x' of
      ('.':ys) -> (x ++ "." ++ y , z)       where (y,z) = span isDigit ys
      _        -> (x,x')
