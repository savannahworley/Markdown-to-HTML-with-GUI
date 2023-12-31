{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module MarkdownToHTMLConverter where

import Data.Char


type Content = String 

data Token = Hash | Dash | Plus | Equal | SpaceChar | Ast 
    | UndScore | Lt | Tab Int | Backslash | Dot
    | LBra | RBra | Exclamation | BackTick | NewLine
    | LPar | RPar | Quote | Gt | PMD Elements | Err Content
    | ContentText Content | Hash2 | Hash3 | Hash4 | Hash5 | Hash6
    | NumLiteral Content 
    deriving (Eq, Show)

--added an indention field to the lists and tab, since we can't make
--a case for all the different amounts of tabs

data Elements = H1 Content | H2 Content | H3 Content 
    | H4 Content | H5 Content | H6 Content | Para Content
    | LnBreak | Bold Content | Italic Content | BoldAndItalic Content 
    | Blockquote Content | OrderedList Int [Elements] | ListItem Content
    | UnorderedList Int [Elements] | Code Content | Image Content Content
    | HorizontalRule | Link Content Content | Block [Token]
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
lexer ('[' : xs) = LBra : ContentText x : lexer (drop y xs) 
    where (x,y) = parseAlt xs
lexer (']' : xs) = RBra : lexer xs 
lexer ('!' : xs) = Exclamation : lexer xs 
lexer ('`' : xs) = BackTick : lexer xs 
lexer ('(' : xs) = LPar : ContentText x : lexer (drop y xs)
    where (x,y) = parseLink xs
lexer (')' : xs) = RPar : lexer xs 
lexer ('"' : xs) = Quote : lexer xs 
lexer ('>' : xs) = Gt : lexer xs 
lexer (x:xs) | isDigit x = let (y,z) = parseNum (x:xs)
                              in NumLiteral y : lexer z
lexer (x:xs) | isAlpha x = let (y,z) = span textParse xs 
                            in ContentText (x:y) : lexer z
lexer xs = [Err xs]

parseLink :: String -> (String, Int)
parseLink s = (x, length x) where
    (x,y) = span linkParse s

parseAlt :: String -> (String, Int)
parseAlt s = (x, length x) where
    (x,y) = span altParse s

parseNum :: String -> (String,String)
parseNum xs =
  let (x,x') = span isDigit xs
   in case x' of
      ('.':ys) -> (x ++ "." ++ y , z)       where (y,z) = span isDigit ys
      _        -> (x,x')

--predicate for ContentText span to help keep lexer simple
textParse :: Char -> Bool
textParse c = (isAlphaNum c) || (c == ' ') || (c == '.')

altParse :: Char -> Bool
altParse c = (isPrint c && (c /= ']'))

linkParse :: Char -> Bool
linkParse c = (isPrint c && (c /= ')'))

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
    (NewLine : xs) -> Left (NewLine : xs)
    (ContentText t : xs) -> Left (ContentText t : xs)
    [Err e] -> Right e
    _ -> Right $ "Parse error: " ++ show s
    where
        result = sr [] s

sr :: [Token] -> [Token] -> [Token]

--creating bold text
sr (Ast : Ast : PMD (Para t) : Ast : Ast : ts) q = sr (PMD (Bold t) : ts) q
sr (Ast : Ast : ContentText t : SpaceChar : Ast : Ast : ts) q = sr (PMD (Bold t) : ts) q 
sr (UndScore : UndScore : ContentText t : UndScore : UndScore : ts) q = sr (PMD (Bold t) : ts) q 
--creating italic text
sr (Ast : ContentText t : Ast : ts) q = sr (PMD (Italic t) : ts) q 
sr (Ast : ContentText t : SpaceChar : Ast : ts) q = sr (PMD (Italic t) : ts) q
sr (UndScore : ContentText t : SpaceChar : UndScore : ts) q = sr (PMD (Italic t) : ts) q 
--creating bold and italic combo
sr (Ast : Ast : Ast : ContentText t : Ast : Ast : Ast : ts) q = sr (PMD (BoldAndItalic t) : ts) q 
sr (UndScore : UndScore : UndScore : ContentText t : UndScore : UndScore : UndScore : ts) q = sr (PMD (BoldAndItalic t) : ts) q 
sr (UndScore : UndScore : Ast : ContentText t : Ast : UndScore : UndScore : ts) q = sr (PMD (BoldAndItalic t) : ts) q 
sr (Ast : Ast : UndScore : ContentText t : UndScore : Ast : Ast : ts) q = sr (PMD (BoldAndItalic t) : ts) q

--ordered list code
--nesting ordered list in unordered list
sr (ContentText t : NumLiteral _ : Tab x : NewLine : PMD (UnorderedList x1 xs) : ts) q = sr (PMD (UnorderedList x1 (combineLists (xs ++ [OrderedList x [ListItem t]]))) : ts) q
--case for orderedlists nested inside orderedlists
sr (ContentText t : NumLiteral _ : Tab x1 : NewLine : PMD (OrderedList x xs) : ts) q | x == x1 = sr (PMD (OrderedList x (xs ++ [ListItem t])) : ts) q
    | otherwise = sr (PMD (OrderedList x (combineLists (indentListHelper (OrderedList x1 [ListItem t]) xs))) : ts) q
--combining orderedlists that are parsed, same indentation, and occur together
sr (PMD (OrderedList i s2) : NewLine : PMD(OrderedList i1 xs) : ts) q | i == i1 = sr (PMD (OrderedList i1 (xs ++ s2)): ts) q
    | otherwise = sr (PMD (OrderedList i1 (combineLists (indentListHelper (OrderedList i s2) xs))) : ts) q
--adding a listitem to an orderedlist
sr (ContentText t : NewLine : PMD(OrderedList i1 xs) : ts) q = sr (PMD (OrderedList i1 (xs ++ [ListItem t])): ts) q
--creating an orderedlist
sr (ContentText t : NumLiteral x : ts) q = sr (PMD (OrderedList 0 [ListItem t]) : ts) q
--case for if a nested ordered list is parsed before its identifiers are consumed
sr (PMD (OrderedList i s2) : Tab x1 : NewLine : NumLiteral _ : NewLine : PMD (OrderedList i1 s1) : ts) q = sr (PMD (OrderedList i1 (s1 ++ [OrderedList x1 s2])) : ts) q

--unordered list code
--nesting unordered list in ordered list with the different constructors for an unorderedlist
sr (ContentText t : Dash : Tab x : NewLine : PMD (OrderedList x1 xs) : ts) q = sr (PMD (OrderedList x1 (combineLists (xs ++ [UnorderedList x [ListItem t]]))) : ts) q
sr (ContentText t : Ast : Tab x : NewLine : PMD (OrderedList x1 xs) : ts) q = sr (PMD (OrderedList x1 (combineLists (xs ++ [UnorderedList x [ListItem t]]))) : ts) q
sr (ContentText t : Plus : Tab x : NewLine : PMD (OrderedList x1 xs) : ts) q = sr (PMD (OrderedList x1 (combineLists (xs ++ [UnorderedList x [ListItem t]]))) : ts) q
--cases for creating or adding to a nested unorderedlist
sr (ContentText t : Dash : Tab x1 : NewLine : PMD (UnorderedList x xs) : ts) q | x == x1 = sr (PMD (UnorderedList x (xs ++ [ListItem t])) : ts) q
    | otherwise = sr (PMD (UnorderedList x (indentListHelper2 (UnorderedList x1 [ListItem t]) xs)) : ts) q
sr (ContentText t : Ast : Tab x1 : NewLine : PMD (UnorderedList x xs) : ts) q | x == x1 = sr (PMD (UnorderedList x (xs ++ [ListItem t])) : ts) q
    | otherwise = sr (PMD (UnorderedList x (indentListHelper2 (UnorderedList x1 [ListItem t]) xs)) : ts) q
sr (ContentText t : Plus : Tab x1 : NewLine : PMD (UnorderedList x xs) : ts) q | x == x1 = sr (PMD (UnorderedList x (xs ++ [ListItem t])) : ts) q
    | otherwise = sr (PMD (UnorderedList x (indentListHelper2 (UnorderedList x1 [ListItem t]) xs)) : ts) q
--combining unorderedlists that are parsed, same indentation, and occur together
sr (PMD (UnorderedList i s2) : NewLine : PMD(UnorderedList i1 xs) : ts) q | i == i1 = sr (PMD (UnorderedList i1 (xs ++ s2)): ts) q
    | otherwise = sr (PMD (UnorderedList i1 (xs ++ indentListHelper (UnorderedList i s2) xs)) : ts) q
--adding a listitem to an unorderedlist
sr (ContentText t : NewLine : PMD(UnorderedList i1 xs) : ts) q = sr (PMD (UnorderedList i1 (xs ++ [ListItem t])): ts) q

--creating an unorderedlist
sr (ContentText t : Dash : ts) q = sr (PMD (UnorderedList 0 [ListItem t]) : ts) q
sr (ContentText t : Ast : ts) q = sr (PMD (UnorderedList 0 [ListItem t]) : ts) q
sr (ContentText t : Plus : ts) q = sr (PMD (UnorderedList 0 [ListItem t]) : ts) q
--cases for if a nested unorderedlist is parsed before its identifiers are consumed
sr (PMD (UnorderedList i s2) : Tab x1 : NewLine : Dash : NewLine : PMD (UnorderedList i1 s1) : ts) q = sr (PMD (UnorderedList i1 (s1 ++ [UnorderedList x1 s2])) : ts) q
sr (PMD (UnorderedList i s2) : Tab x1 : NewLine : Ast : NewLine : PMD (UnorderedList i1 s1) : ts) q = sr (PMD (UnorderedList i1 (s1 ++ [UnorderedList x1 s2])) : ts) q
sr (PMD (UnorderedList i s2) : Tab x1 : NewLine : Plus : NewLine : PMD (UnorderedList i1 s1) : ts) q = sr (PMD (UnorderedList i1 (s1 ++ [UnorderedList x1 s2])) : ts) q

--creating headers
sr (ContentText t : Hash : ts) q = sr (PMD (H1 t) : ts) q
sr (ContentText t : Hash2 : ts) q = sr (PMD (H2 t): ts) q
sr (ContentText t : Hash3 : ts) q = sr (PMD (H3 t) : ts) q
sr (ContentText t : Hash4 : ts) q = sr (PMD (H4 t) : ts) q
sr (ContentText t : Hash5 : ts) q = sr (PMD (H5 t) : ts) q
sr (ContentText t : Hash6 : ts) q = sr (PMD (H6 t) : ts) q

--creating links and images
sr (RPar : ContentText t : RPar : LBra : ContentText t1 : RBra : Exclamation : ts) q = sr (PMD (Image t1 t) : ts) q
sr (RPar : ContentText t : RPar : LBra : ContentText t1 : RBra : ts) q = sr (PMD (Link t1 t) : ts) q


--creating a paragraph
sr (NewLine : ContentText t1 : NewLine : ContentText t : ts) q = sr (PMD (Para t1) : PMD (Para t) : ts) q
sr (ContentText t1 : NewLine : ContentText t : ts) q = sr (PMD (Para t1) : PMD (Para t) : ts) q
sr (ContentText t : NewLine : ts) q = sr (PMD (Para t) : ts) q
--creating a line break 
sr (SpaceChar : SpaceChar : ts) q = sr (PMD (LnBreak) : ts) q 
--creating a blockquote 
sr (ContentText t : Gt : ts) q = sr (PMD (Blockquote t) : ts) q
--creating a code block
sr (ContentText t : SpaceChar : SpaceChar : SpaceChar : SpaceChar : ts) q = sr (PMD (Code t) : ts) q
sr (ContentText t : Tab x : ts) q = sr (PMD (Code t) : ts) q
sr (BackTick : ContentText t : BackTick : ts) q = sr (PMD (Code t) : ts) q
sr (PMD (Para t) : PMD (Code x) : PMD (Para t1) : ts) q = sr (PMD (Para (t1 ++ "<code>" ++ x ++ "</code> " ++ t)) : ts) q
--creating an image
sr (RPar : ContentText t : LPar : RBra : ContentText tx : LBra : Exclamation : ts) q = sr (PMD (Image tx t) : ts) q
--creating horizontal rules
sr (Ast : Ast : Ast : ts) q = sr (PMD (HorizontalRule) : ts) q 
sr (UndScore : UndScore : UndScore : ts) q = sr (PMD (HorizontalRule) : ts) q 
sr (Dash : Dash : Dash : ts) q = sr (PMD (HorizontalRule) : ts) q 
sr (RPar : ContentText t : LPar : RBra : ContentText tx : LBra : ts) q = sr (PMD (Image tx t) : ts) q
--lexer error case
sr (Err e : ts) q = [Err e]
--case for lone contenttext, going to make the assumption user wants a paragraph, otherwise it would be unhandled
sr (ContentText t : []) q = sr (PMD (Para t) : []) q
--sr (ContentText t : SpaceChar : ts) q = sr (PMD (Para t) : ts ) q
sr ts (x:q) = sr (x:ts) q
sr ts [] = ts

--assists in checking if the parsed orderedlist is part of a new nested list or the next part of a previous one
indentListHelper :: Elements -> [Elements] -> [Elements]
indentListHelper (OrderedList x xs) ((OrderedList y ys) : zs) = if x == y then [OrderedList y (ys ++ xs)] ++ zs else (OrderedList y ys) : zs ++ [OrderedList x xs]
indentListHelper (OrderedList x xs) ys = ys ++ [OrderedList x xs]

--assists in checking if the parsed unorderedlist is part of a new nested list or the next part of a previous one
indentListHelper2 :: Elements -> [Elements] -> [Elements]
indentListHelper2 (UnorderedList x xs) ((UnorderedList y ys) : zs) = if x == y then [UnorderedList y (ys ++ xs)] ++ zs else (UnorderedList y ys) : zs ++ [UnorderedList x xs]
indentListHelper2 (UnorderedList x xs) ys = ys ++ [UnorderedList x xs]

combineLists :: [Elements] -> [Elements]
combineLists ((OrderedList x t) : (OrderedList x1 t1) : xs) | x == x1 = [(OrderedList x (t ++ t1))] ++ combineLists xs
    | x1 > x = [OrderedList x (t ++ [OrderedList x1 t1])] ++ combineLists xs
combineLists ((UnorderedList x t) : (UnorderedList x1 t1) : xs) | x == x1 = [(UnorderedList x (t ++ t1))] ++ combineLists xs
    | x1 > x = [UnorderedList x (t ++ [UnorderedList x1 t1])] ++ combineLists xs
combineLists ((OrderedList x t) : (UnorderedList x1 t1) : xs) | x1 > x = [OrderedList x (t ++ [UnorderedList x1 t1])]
combineLists ((UnorderedList x t) : (OrderedList x1 t1) : xs) | x1 > x = [UnorderedList x (t ++ [OrderedList x1 t1])]
combineLists (ListItem t : xs) = [ListItem t] ++ combineLists xs 
combineLists xs = xs

--parsed markdown needs to be reversed before being converted, comes out reversed from parser
--bold and italic tags should be 
converter :: String -> [Token] -> String 
converter s [] = s
converter s (PMD (H1 t) : xs) = s ++ "<h1>" ++ t ++ "</h1>\n" ++ converter s xs
converter s (PMD (H2 t) : xs) = s ++ "<h2>" ++ t ++ "</h2>\n" ++ converter s xs
converter s (PMD (H3 t) : xs) = s ++ "<h3>" ++ t ++ "</h3>\n" ++ converter s xs
converter s (PMD (H4 t) : xs) = s ++ "<h4>" ++ t ++ "</h4>\n" ++ converter s xs
converter s (PMD (H5 t) : xs) = s ++ "<h5>" ++ t ++ "</h5>\n" ++ converter s xs
converter s (PMD (H6 t) : xs) = s ++ "<h6>" ++ t ++ "</h6>\n" ++ converter s xs
--converter s (PMD (Para t) : PMD (Italic t1) : PMD (Para t2) : xs) = s ++ "<p>" ++ t ++ "<i>" ++ t1 ++ " " ++ "</i>" ++ t2 ++ "</p>" ++ converter s xs
--converter s (PMD (Para t) : PMD (Bold t1) : PMD (Para t2) : xs) = s ++ "<p>" ++ t ++ "<b>" ++ t1 ++ " " ++ "</b>" ++ t2 ++ "</p>" ++ converter s xs
--converter s (PMD (Para t) : PMD (BoldAndItalic t1) : PMD (Para t2) : xs) = s ++ "<p>" ++ t ++ "<i><b>" ++ t1 ++ " " ++ "</b></i>" ++ t2 ++ "</p>" ++ converter s xs
--converter s (PMD (Para t) : xs) = s ++ "<p>" ++ t ++ "</p>\n" ++ converter s xs
converter s (PMD (Para t) : xs) = s ++ x ++ converter s (drop y xs) where
    (x,y) = convertPara (PMD (Para t) : xs)
converter s (PMD (LnBreak) : xs) = s ++ "<br>\n" ++ converter s xs 
converter s (PMD (Bold t) : xs) = s ++ "<b>" ++ t ++ "</b>" ++ converter s xs 
converter s (PMD (Italic t) : xs) = s ++ "<i>" ++ t ++ "</i>" ++ converter s xs 
converter s (PMD (BoldAndItalic t) : xs) = s ++ "<i><b>" ++ t ++ "</b></i>" ++ converter s xs 
converter s (PMD (Blockquote t) : xs) = s ++ "<blockquote>" ++ t ++ "</blockquote>" ++converter s xs 
converter s (PMD (Code t) : xs) = s ++ "<code>" ++ t ++ "</code>" ++ converter s xs 
converter s (PMD (UnorderedList x elements) : xs) = converter (s ++ convertUnorderedList x elements) xs
converter s (PMD (OrderedList x elements) : xs) = converter (s ++ convertOrderedList x elements) xs
converter s (PMD (Link tx t) : xs) = s ++ "<a href=\"" ++ t ++ "\">" ++ tx ++ "</a>" ++ converter s xs
converter s (PMD (Image tx t) : xs) = s ++ "<img src=\"" ++ t ++ "\" alt=\"" ++ tx ++ "\">" ++ "</img>" ++ converter s xs
converter s (PMD (HorizontalRule) : xs) = s ++ "<hr>\n" ++ converter s xs
converter s (NewLine : xs) = converter s xs
converter s (SpaceChar : xs) = converter s xs
converter s (Ast : xs) = converter s xs
converter s (Dot : xs) = converter s xs
converter s (UndScore : xs) = converter s xs

--creates the outside braces that indicate this is an unordered list
convertUnorderedList :: Int -> [Elements] -> String
convertUnorderedList x elements = "<ul>\n" ++ convertListElement x elements ++ createTabs x ++ "</ul>\n"

--creates the outside braces that indicate this is an ordered list
convertOrderedList :: Int -> [Elements] -> String
convertOrderedList x elements = "<ol>\n" ++ convertListElement x elements ++ createTabs x ++ "</ol>\n"

--traverses through the list and marks each element as a part of the list within the undordered/ordered list braces
convertListElement :: Int -> [Elements] -> String
convertListElement x1 (ListItem t : OrderedList x t1 : ts) = (createTabs x1 ++ "<li>" ++ t ++ "\n" ++ createTabs x ++ convertOrderedList x t1) 
    ++ createTabs x1 ++ "</li>\n" ++ convertListElement x1 ts
convertListElement x1 (ListItem t : UnorderedList x t1 : ts) = (createTabs x1 ++ "<li>" ++ t ++ "\n" ++ createTabs x ++ convertUnorderedList x t1) 
    ++ createTabs x1 ++ "</li>\n" ++ convertListElement x1 ts
convertListElement x (ListItem t : ts) = createTabs x ++ "<li>" ++ t ++ "</li>\n" ++ convertListElement x ts
convertListElement x1 (OrderedList x t1 : ts) = convertOrderedList x t1 ++ convertListElement x1 ts
convertListElement x1 (UnorderedList x t1 : ts) = convertUnorderedList x t1 ++ convertListElement x1 ts
convertListElement _ [] = []
-- convertListElement (ListItem t) = "<li>" ++ t ++ "</li>\n"
-- convertListElement (OrderedList _ nestedElements) = convertOrderedList nestedElements
-- convertListElement (UnorderedList _ nestedElements) = convertUnorderedList nestedElements

createTabs :: Int -> String
createTabs x = ['\t' | x <- [1..x]]

convertPara :: [Token] -> (String, Int)
convertPara (PMD (Para t) : xs) = ("<p>" ++ t ++ x, 1 + y) where 
    (x,y) = convertPara xs
convertPara (PMD (Bold t) : xs) = ("<b>" ++ t ++ "</b>" ++ x, 1 + y) where 
    (x, y) = convertPara xs
convertPara (PMD (Italic t) : xs) = ("<i>" ++ t ++ "</i>" ++ x, 1 + y) where 
    (x,y) = convertPara xs
convertPara (PMD (BoldAndItalic t) : xs) = ("<i><b>" ++ t ++ "</b></i>" ++ x, 1 + y) where 
    (x,y) = convertPara xs
convertPara (ContentText t : xs) = (t ++ x, 1 + y) where 
    (x,y) = convertPara xs
convertPara (SpaceChar : xs) = ("" ++ x, 1 + y) where
    (x,y) = convertPara xs
convertPara (Ast : xs) = ("" ++ x, 1 + y) where
    (x,y) = convertPara xs
convertPara (UndScore : xs) = ("" ++ x, 1 + y) where
    (x,y) = convertPara xs
convertPara _ = ("</p>", 0)

runConverter :: String -> String
runConverter s = 
    let par = parser (lexer s) in 
        case par of 
            Left x -> converter "" (reverse x) 
            Right err -> err