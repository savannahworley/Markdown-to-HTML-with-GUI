import Data.Char

type Text = String 

data Token = Hash | Dash | Plus | Equal | SpaceChar | Ast 
    | UndScore | Lt | Num | Tab | Backslash | Dot
    | LBra | RBra | Exclamation | BackTick | NewLine
    | LPar | RPar | Quote | Gt | PMD Elements | Err Text
    | ContentText Text

--what is ListItem?

data Elements = H1 Text | H2 Text | H3 Text 
    | H4 Text | H5 Text | H6 Text | Para Text
    | LnBreak | Bold Text | Italic Text | BoldAndItalic Text 
    | Blockquote Text | OrderedList [Elements] | ListItem 
    | UnorderedList [Elements] | Code Text | Image Text Text
    | HorizontalRule | Link Text Text

lexer :: String -> [Token]
lexer "" = []
lexer ('#' : xs) = Hash : lexer xs 
lexer ('.' : xs) = Dot : lexer xs
lexer ('-' : xs) = Dash : lexer xs 
lexer ('+' : xs) = Plus : lexer xs 
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
lexer ('\n' : xs) = NewLine : lexer xs
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

parser :: [Token] -> Either Elements String 
parser s = case result of 
    [PMD element] -> Left element
    [Err e] -> Right e
    _ -> Right $ "Parse error: " ++ show s
    where
        result = sr [] s

sr :: [Token] -> [Token] -> [Token]
sr (ContentText t : Hash : ts) q = sr (PMD (H1 t) : ts) q
sr (ContentText t : Hash : Hash : ts) q = sr (PMD (H2 t): ts) q
sr (ContentText t : Hash : Hash : Hash : ts) q = sr (PMD (H3 t) : ts) q
sr (ContentText t : Hash : Hash : Hash : Hash : ts) q = sr (PMD (H4 t) : ts) q
sr (ContentText t : Hash : Hash : Hash : Hash : Hash : ts) q = sr (PMD (H5 t) : ts) q
sr (ContentText t : Hash : Hash : Hash : Hash : Hash : Hash : ts) q = sr (PMD (H6 t) : ts) q
sr (ContentText t : Newline : Newline : ts) q = sr (PMD (Para t) : ts) q 
sr (Newline : Space : Space : ts) q = sr (PMD (LnBreak) : ts) q 
sr (Ast : Ast : ContentText t : Ast : Ast : ts) q = sr (PMD (Bold t) : ts) q 
sr (UndScore : UndScore : ContentText t : UndScore : UndScore : ts) q = sr (PMD (Bold t) : ts) q 
sr (Ast : ContentText t : Ast : ts) q = sr (PMD (Italic t) : ts) q 
sr (UndScore : ContentText t : UndScore : ts) q = sr (PMD (Italic t) : ts) q 
sr (Ast : Ast : Ast : ContentText t : Ast : Ast : Ast : ts) q = sr (PMD (BoldAndItalic t) : ts) q 
sr (UndScore : UndScore : UndScore : ContentText t : UndScore : UndScore : UndScore : ts) q = sr (PMD (BoldAndItalic t) : ts) q 
sr (UndScore : UndScore : Ast : ContentText t : Ast : UndScore : UndScore : ts) q = sr (PMD (BoldAndItalic t) : ts) q 
sr (Ast : Ast : UndScore : ContentText t : UndScore : Ast : Ast : ts) q = sr (PMD (BoldAndItalic t) : ts) q 
sr (ContentText t : Gt : ts) q = sr (PMD (Blockquote) : ts) q
sr (ContentText t : Dot : Num : ts) q = sr (PMD (OrderedList [t]) : ts) q 
sr (ContentText t : Dash : ts) q = sr (PMD (UnorderedList [t]) : ts) q
sr (ContentText t : Ast : ts) q = sr (PMD (UnorderedList [t]) : ts) q
sr (ContentText t : Plus : ts) q = sr (PMD (UnorderedList [t]) : ts) q
sr (ContentText t : Space : Space : Space : Space : ts) q = sr (PMD (Code t) : ts) q
sr (ComtentText t : Tab : ts) q = sr (PMD (Code t) : ts) q
sr (RPar : ContentText t : LPar : RBra : ContentText tx : LBra : Exclamation : ts) q = sr (PMD (Image tx t) : ts) q
sr (Ast : Ast : Ast : ts) q = sr (PMD (HorizontalRule) : ts) q 
sr (UndScore : UndScore : UndScore : ts) q = sr (PMD (HorizontalRule) : ts) q 
sr (Dash : Dash : Dash : ts) q = sr (PMD (HorizontalRule) : ts) q 
sr (RPar : ContentText t : LPar : RBra : ContentText tx : LBra : ts) q = sr (PMD (Image tx t) : ts) q
sr (Err e : ts) q = [Err e]
sr ts (x:q) = sr (x:ts) q
sr ts [] = ts