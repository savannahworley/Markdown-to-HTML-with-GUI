module MarkdownToHTMLConverter where

import Data.Char


type Text = String 

data Token = Hash | Dash | Plus | Equal | SpaceChar | Ast 
    | UndScore | Lt | Tab Int | Backslash | Dot
    | LBra | RBra | Exclamation | BackTick | NewLine
    | LPar | RPar | Quote | Gt | PMD Elements | Err Text
    | ContentText Text | Hash2 | Hash3 | Hash4 | Hash5 | Hash6
    | NumLiteral Text 
    deriving (Eq, Show)

--added an indention field to the lists and tab, since we can't make
--a case for all the different amounts of tabs

data Elements = H1 Text | H2 Text | H3 Text 
    | H4 Text | H5 Text | H6 Text | Para Text
    | LnBreak | Bold Text | Italic Text | BoldAndItalic Text 
    | Blockquote Text | OrderedList Int [Elements] | ListItem Text
    | UnorderedList Int [Elements] | Code Text | Image Text Text
    | HorizontalRule | Link Text Text | Block [Token]
    deriving (Eq, Show)

lexer :: String -> [Token]
lexer "" = []
lexer ('#' : xs) = x : lexer (drop y xs)
    where (x, y) = parseHash ('#':xs)
lexer ('.' : xs) = Dot : lexer xs
lexer ('-' : xs) = Dash : lexer xs 
lexer ('+' : xs) = Plus : lexer xs 
lexer ('=' : xs) = Equal : lexer xs 
lexer ('\n' : xs) = NewLine : lexer xs
lexer ('\t' : xs) = x : lexer (drop (y-1) xs)
    where (x, y) = parseTab ('\t' : xs)
lexer (x:xs) | isSpace x = SpaceChar : lexer xs 
lexer ('*' : xs) = Ast : lexer xs 
lexer ('_' : xs) = UndScore : lexer xs 
lexer ('<' : xs) = Lt : lexer xs 
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
                              in NumLiteral y : lexer z
lexer (x:xs) | isAlpha x = let (y,z) = span isAlphaNum xs 
                            in ContentText (x:y) : lexer z
lexer xs = [Err xs]

parseNum :: String -> (String,String)
parseNum xs =
  let (x,x') = span isDigit xs
   in case x' of
      ('.':ys) -> (x ++ "." ++ y , z)       where (y,z) = span isDigit ys
      _        -> (x,x')

parseHash :: String -> (Token, Int)
parseHash s = 
    let (x, y) = span (== '#') s
    in case length x of
        1 -> (Hash, 0)
        2 -> (Hash2, 1)
        3 -> (Hash3, 2)
        4 -> (Hash4, 3)
        5 -> (Hash5, 4)
        6 -> (Hash6, 5)
        x -> (Err "too many/not enough hashmarks", 0)

parseTab :: String -> (Token, Int)
parseTab xs = (Tab (length x), length x)
    where (x, y) = span (== '\t') xs

parser :: [Token] -> Either [Token] String 
parser s = case result of
    (PMD e) : xs -> Left (PMD e : xs)
    [Err e] -> Right e
    _ -> Right $ "Parse error: " ++ show s
    where
        result = sr [] s

sr :: [Token] -> [Token] -> [Token]
sr (ContentText t : Hash : ts) q = sr (PMD (H1 t) : ts) q
sr (ContentText t : Hash2 : ts) q = sr (PMD (H2 t): ts) q
sr (ContentText t : Hash3 : ts) q = sr (PMD (H3 t) : ts) q
sr (ContentText t : Hash4 : ts) q = sr (PMD (H4 t) : ts) q
sr (ContentText t : Hash5 : ts) q = sr (PMD (H5 t) : ts) q
sr (ContentText t : Hash6 : ts) q = sr (PMD (H6 t) : ts) q
sr (ContentText t : NewLine : NewLine : ts) q = sr (PMD (Para t) : ts) q 
sr (SpaceChar : SpaceChar : ts) q = sr (PMD (LnBreak) : ts) q 
sr (Ast : Ast : ContentText t : Ast : Ast : ts) q = sr (PMD (Bold t) : ts) q 
sr (UndScore : UndScore : ContentText t : UndScore : UndScore : ts) q = sr (PMD (Bold t) : ts) q 
sr (Ast : ContentText t : Ast : ts) q = sr (PMD (Italic t) : ts) q 
sr (UndScore : ContentText t : UndScore : ts) q = sr (PMD (Italic t) : ts) q 
sr (Ast : Ast : Ast : ContentText t : Ast : Ast : Ast : ts) q = sr (PMD (BoldAndItalic t) : ts) q 
sr (UndScore : UndScore : UndScore : ContentText t : UndScore : UndScore : UndScore : ts) q = sr (PMD (BoldAndItalic t) : ts) q 
sr (UndScore : UndScore : Ast : ContentText t : Ast : UndScore : UndScore : ts) q = sr (PMD (BoldAndItalic t) : ts) q 
sr (Ast : Ast : UndScore : ContentText t : UndScore : Ast : Ast : ts) q = sr (PMD (BoldAndItalic t) : ts) q 
sr (ContentText t : Gt : ts) q = sr (PMD (Blockquote t) : ts) q

--ordered list code
sr (ContentText t : NumLiteral _ : Tab x1 : NewLine : PMD (OrderedList x xs) : ts) q | x == x1 = sr (PMD (OrderedList x (xs ++ [ListItem t])) : ts) q
    | otherwise = sr (PMD (OrderedList x (indentListHelper (OrderedList x1 [ListItem t]) xs)) : ts) q
sr (PMD (OrderedList i s2) : NewLine : PMD(OrderedList i1 xs) : ts) q | i == i1 = sr (PMD (OrderedList i1 (xs ++ s2)): ts) q
    | otherwise = sr (PMD (OrderedList i1 (xs ++ indentListHelper (OrderedList i s2) xs)) : ts) q
sr (ContentText t : NewLine : PMD(OrderedList i1 xs) : ts) q = sr (PMD (OrderedList i1 (xs ++ [ListItem t])): ts) q
sr (ContentText t : NumLiteral x : ts) q = sr (PMD (OrderedList 0 [ListItem t]) : ts) q
sr (PMD (OrderedList i s2) : Tab x1 : NewLine : NumLiteral _ : NewLine : PMD (OrderedList i1 s1) : ts) q = sr (PMD (OrderedList i1 (s1 ++ [OrderedList x1 s2])) : ts) q

-- sr (PMD (OrderedList i s2) : NewLine : PMD(OrderedList i1 xs) : ts) q = sr (PMD (OrderedList (xs ++ s2)): ts) q
-- sr (PMD (OrderedList i s2) : Tab x1 : NewLine : NumLiteral x : NewLine : PMD (OrderedList i1 s1) : ts) q = sr (PMD (OrderedList (s1 ++ [OrderedList s2])) : ts) q
-- sr (PMD (OrderedList i s2) : Tab x1 : NewLine : PMD (OrderedList i1 s1) : ts) q = sr (PMD (OrderedList (indentListHelper s1 s2)) : ts) q
-- sr (PMD (OrderedList i s2) : Tab x1 : NewLine : NumLiteral x : ts) q = sr (PMD (OrderedList [OrderedList s2]) : ts) q
-- sr (PMD (OrderedList i s2) : Tab x1 : NewLine : ContentText s : NumLiteral x : ts) q = sr (PMD (OrderedList (ListItem s : [OrderedList s2])) : ts) q
-- sr (ContentText t : NewLine : PMD(OrderedList i1 xs) : ts) q = sr (PMD (OrderedList (xs ++ [ListItem t])): ts) q
-- sr (ContentText t : NumLiteral x : ts) q = sr (PMD (OrderedList [ListItem t]) : ts) q


--unordered list code
sr (ContentText t : Dash : Tab x1 : NewLine : PMD (UnorderedList x xs) : ts) q | x == x1 = sr (PMD (UnorderedList x (xs ++ [ListItem t])) : ts) q
    | otherwise = sr (PMD (UnorderedList x (indentListHelper2 (UnorderedList x1 [ListItem t]) xs)) : ts) q
sr (ContentText t : Ast : Tab x1 : NewLine : PMD (UnorderedList x xs) : ts) q | x == x1 = sr (PMD (UnorderedList x (xs ++ [ListItem t])) : ts) q
    | otherwise = sr (PMD (UnorderedList x (indentListHelper2 (UnorderedList x1 [ListItem t]) xs)) : ts) q
sr (ContentText t : Plus : Tab x1 : NewLine : PMD (UnorderedList x xs) : ts) q | x == x1 = sr (PMD (UnorderedList x (xs ++ [ListItem t])) : ts) q
    | otherwise = sr (PMD (UnorderedList x (indentListHelper2 (UnorderedList x1 [ListItem t]) xs)) : ts) q
sr (PMD (UnorderedList i s2) : NewLine : PMD(UnorderedList i1 xs) : ts) q | i == i1 = sr (PMD (UnorderedList i1 (xs ++ s2)): ts) q
    | otherwise = sr (PMD (UnorderedList i1 (xs ++ indentListHelper (UnorderedList i s2) xs)) : ts) q
sr (ContentText t : NewLine : PMD(UnorderedList i1 xs) : ts) q = sr (PMD (UnorderedList i1 (xs ++ [ListItem t])): ts) q
sr (ContentText t : Dash : ts) q = sr (PMD (UnorderedList 0 [ListItem t]) : ts) q
sr (ContentText t : Ast : ts) q = sr (PMD (UnorderedList 0 [ListItem t]) : ts) q
sr (ContentText t : Plus : ts) q = sr (PMD (UnorderedList 0 [ListItem t]) : ts) q
sr (PMD (UnorderedList i s2) : Tab x1 : NewLine : Dash : NewLine : PMD (UnorderedList i1 s1) : ts) q = sr (PMD (UnorderedList i1 (s1 ++ [UnorderedList x1 s2])) : ts) q
sr (PMD (UnorderedList i s2) : Tab x1 : NewLine : Ast : NewLine : PMD (UnorderedList i1 s1) : ts) q = sr (PMD (UnorderedList i1 (s1 ++ [UnorderedList x1 s2])) : ts) q
sr (PMD (UnorderedList i s2) : Tab x1 : NewLine : Plus : NewLine : PMD (UnorderedList i1 s1) : ts) q = sr (PMD (UnorderedList i1 (s1 ++ [UnorderedList x1 s2])) : ts) q

-- sr (ContentText t : Dash : ts) q = sr (PMD (UnorderedList [ListItem t]) : ts) q
-- sr (ContentText t : Ast : ts) q = sr (PMD (UnorderedList [ListItem t]) : ts) q
-- sr (ContentText t : Plus : ts) q = sr (PMD (UnorderedList [ListItem t]) : ts) q

sr (ContentText t : SpaceChar : SpaceChar : SpaceChar : SpaceChar : ts) q = sr (PMD (Code t) : ts) q
sr (ContentText t : Tab : ts) q = sr (PMD (Code t) : ts) q
sr (BackTick : ContentText t : BackTick : ts) q = sr (PMD (Code t) : ts) q
sr (RPar : ContentText t : LPar : RBra : ContentText tx : LBra : Exclamation : ts) q = sr (PMD (Image tx t) : ts) q
sr (Ast : Ast : Ast : ts) q = sr (PMD (HorizontalRule) : ts) q 
sr (UndScore : UndScore : UndScore : ts) q = sr (PMD (HorizontalRule) : ts) q 
sr (Dash : Dash : Dash : ts) q = sr (PMD (HorizontalRule) : ts) q 
sr (RPar : ContentText t : LPar : RBra : ContentText tx : LBra : ts) q = sr (PMD (Image tx t) : ts) q
sr (Err e : ts) q = [Err e]
sr ts (x:q) = sr (x:ts) q
sr ts [] = ts

--needs Links, Images, HorizontalRule, OrderedList and UnOrderedList

indentListHelper :: Elements -> [Elements] -> [Elements]
indentListHelper x [] = [x]
indentListHelper (OrderedList x xs) (y:ys) = 
    case y of 
        OrderedList x1 zs -> if x == x1 then [OrderedList x (zs ++ xs)] else y : indentListHelper (OrderedList x xs) ys
        _ -> y : indentListHelper (OrderedList x xs) ys

indentListHelper2 :: Elements -> [Elements] -> [Elements]
indentListHelper2 (UnorderedList x xs) (y:ys) = 
    case y of 
        UnorderedList x1 zs -> if x == x1 then [UnorderedList x (zs ++ xs)] else y : indentListHelper2 (UnorderedList x xs) ys
        _ -> y : indentListHelper2 (UnorderedList x xs) ys
converter :: String -> [Token] -> String 
-- converter "" = []
-- converter s (PMD (H1 t) : xs) = s ++ "<h1>" ++ t ++ "</h1>" : converter s xs
-- converter s (PMD (H2 t) : xs) = s ++ "<h2>" ++ t ++ "</h2>" : converter s xs
converter s (PMD (H3 t) : xs) = s ++ "<h3>" ++ t ++ "</h3>" : converter s xs
converter s (PMD (H4 t) : xs) = s ++ "<h4>" ++ t ++ "</h4>" : converter s xs
converter s (PMD (H5 t) : xs) = s ++ "<h5>" ++ t ++ "</h5>" : converter s xs
converter s (PMD (H6 t) : xs) = s ++ "<h6>" ++ t ++ "</h6>" : converter s xs
converter s (PMD (Para t) : xs) = s ++ "<p>" ++ t ++ "</p>" : converter s xs 
converter s (PMD (LnBreak) : xs) = s ++ "<br>" : converter s xs 
converter s (PMD (Bold t) : xs) = s ++ "<strong>" ++ t ++ "</strong>" : converter s xs 
converter s (PMD (Italic t) : xs) = s ++ "<em>" ++ t ++ "</em>" : converter s xs 
converter s (PMD (BoldAndItalic t) : xs) = s ++ "<em><strong>" ++ t ++ "</strong></em>" : converter s xs 
converter s (PMD (Blockquote t) : xs) = s ++ ">" ++ t : converter s xs 
converter s (PMD (Code t) : xs) = s ++ "<code>" ++ t ++ "</code>" : converter s xs 

-- indentListHelper :: [Elements] -> [Elements] -> [Elements]
-- indentListHelper [] ys = [OrderedList  ys]
-- indentListHelper (x:xs) ys = 
--     case x of
--         OrderedList x1 zs -> xs ++ [addListToOrderedList (OrderedList x1 zs) ys]
--         _ -> x : indentListHelper xs ys

-- addListToOrderedList :: Elements -> [Elements] -> Elements
-- addListToOrderedList (OrderedList x existingList) newList = OrderedList (existingList ++ newList)
-- addListToOrderedList otherElement _ = otherElement
