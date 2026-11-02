{-# LANGUAGE NoImplicitPrelude #-}

-- | A page as runes, its pieces and its tree, built lazily.
--   Tracks unbound/PLAN9: wsn/html.wsn (the grammar of a page's pieces),
--   wsn/html.rules (what a context-free grammar cannot say), in/views/pieces.mjs.
--
--   A page is its bytes; a 'Rune' the bytes of one UTF-8 sequence (a lead
--   byte and the continuation bytes it calls for, as chartorune reads). The
--   page is read into 'Piece's (a comment, the doctype, a start or end tag,
--   text, raw text), each its span of the page's own runes, and the pieces
--   into a tree of 'Node's by html.rules' nesting rule: a start tag opens an
--   element unless it is void or closed by "/>"; an end tag closes the nearest
--   open element of its name and every element opened after it; an end tag
--   with no open element of its name is a leaf; an element open at the end
--   stays open. Nothing is read before it is asked for, and nothing twice: a
--   node's children are a list built as they are reached (Haskell's own
--   call-by-need, the two rules OMI.WSN's 'accepts' keeps by its table). The
--   page comes back by 'flatten', byte for byte.
--
--   'cast' gives a rune its symbol in html.wsn's alphabet, where WSN leaves
--   letter and character to the language: a letter its lower case, a digit
--   itself, each of < > / = " ' - _ : . # & ; ! itself, a space, tab, line
--   feed, carriage return or form feed a space, any other rune "~". The cast
--   is for questions only; the runes stay as written.
module OMI.View
  ( Rune
  , Kind(..)
  , Piece
  , Node(..)
  , End(..)
  , runes
  , runeBytes
  , cast
  , pieces
  , pieceKind
  , pieceRunes
  , pieceBytes
  , pieceCast
  , pieceName
  , production
  , tree
  , flatten
  , outline
  ) where

import OMI.Kernel
import OMI.WSN (Answer(..))

newtype Rune = Rune [Byte]

data Kind = Comment | Doctype | StartTag | EndTag | Text | RawText

-- | A piece: its kind, its runes, and for a tag its name cast (empty otherwise).
data Piece = Piece Kind [Rune] [Byte]

data End = Closed Piece | Unclosed

data Node = Leaf Piece | Element Piece [Node] End

-- ---------------------------------------------------------------- bytes

x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xA, xB, xC, xD, xE, xF :: Nibble
x0 = N O O O O
x1 = N O O O I
x2 = N O O I O
x3 = N O O I I
x4 = N O I O O
x5 = N O I O I
x6 = N O I I O
x7 = N O I I I
x8 = N I O O O
x9 = N I O O I
xA = N I O I O
xB = N I O I I
xC = N I I O O
xD = N I I O I
xE = N I I I O
xF = N I I I I

both :: Answer -> Answer -> Answer
both Yes Yes = Yes
both _ _ = No

either' :: Answer -> Answer -> Answer
either' No No = No
either' _ _ = Yes

not' :: Answer -> Answer
not' Yes = No
not' No = Yes

eqBit :: Bit -> Bit -> Answer
eqBit O O = Yes
eqBit I I = Yes
eqBit _ _ = No

eqNibble :: Nibble -> Nibble -> Answer
eqNibble (N a b c d) (N e f g h) = both (both (eqBit a e) (eqBit b f)) (both (eqBit c g) (eqBit d h))

eqByte :: Byte -> Byte -> Answer
eqByte (B a b) (B c d) = both (eqNibble a c) (eqNibble b d)

eqBytes :: [Byte] -> [Byte] -> Answer
eqBytes [] [] = Yes
eqBytes (a : as) (b : bs) = both (eqByte a b) (eqBytes as bs)
eqBytes _ _ = No

member :: [Byte] -> [[Byte]] -> Answer
member _ [] = No
member w (v : vs) = either' (eqBytes w v) (member w vs)

-- | at most 9 (1001), and at most A (1010)
le9, leA :: Nibble -> Answer
le9 (N O _ _ _) = Yes
le9 (N I O O _) = Yes
le9 _ = No
leA (N I O I O) = Yes
leA n = le9 n

nonzero :: Nibble -> Answer
nonzero n = not' (eqNibble n x0)

lt, gt, slash, bang, dash, eq, dq, sq, under, colon, dot, hash, amp, semi, space, tilde :: Byte
lt = B x3 xC
gt = B x3 xE
slash = B x2 xF
bang = B x2 x1
dash = B x2 xD
eq = B x3 xD
dq = B x2 x2
sq = B x2 x7
under = B x5 xF
colon = B x3 xA
dot = B x2 xE
hash = B x2 x3
amp = B x2 x6
semi = B x3 xB
space = B x2 x0
tilde = B x7 xE

kept :: [Byte]
kept = [lt, gt, slash, eq, dq, sq, dash, under, colon, dot, hash, amp, semi, bang]

isIn :: Byte -> [Byte] -> Answer
isIn _ [] = No
isIn b (c : cs) = either' (eqByte b c) (isIn b cs)

-- | an ASCII letter (A-Z, a-z), a digit, white space
upper, lower', digit, white :: Byte -> Answer
upper (B h l) = either' (both (eqNibble h x4) (nonzero l)) (both (eqNibble h x5) (leA l))
lower' (B h l) = either' (both (eqNibble h x6) (nonzero l)) (both (eqNibble h x7) (leA l))
digit (B h l) = both (eqNibble h x3) (le9 l)
white b = isIn b [space, B x0 x9, B x0 xA, B x0 xD, B x0 xC]

letter :: Byte -> Answer
letter b = either' (upper b) (lower' b)

lowerOf :: Byte -> Byte
lowerOf b@(B (N p q _ s) l) = case upper b of
  Yes -> B (N p q I s) l
  No -> b

-- | the name characters: a letter, a digit, - _ : .
nameChar :: Byte -> Answer
nameChar b = either' (either' (letter b) (digit b)) (isIn b [dash, under, colon, dot])

-- ---------------------------------------------------------------- lists

mapL :: (a -> b) -> [a] -> [b]
mapL _ [] = []
mapL f (a : as) = f a : mapL f as

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

concatL :: [[a]] -> [a]
concatL [] = []
concatL (x : xs) = append x (concatL xs)

-- ---------------------------------------------------------------- runes

-- | The page's runes: a lead byte under 0x80 alone; 110xxxxx with one more,
--   1110xxxx with two, 11110xxx with three; any other byte alone.
runes :: [Byte] -> [Rune]
runes [] = []
runes (b : bs) = case more b of
  n -> let (r, rest) = takeN n bs in Rune (b : r) : runes rest
  where
    more (B (N I I O _) _) = [()]
    more (B (N I I I O) _) = [(), ()]
    more (B (N I I I I) (N O _ _ _)) = [(), (), ()]
    more _ = []
    takeN [] xs = ([], xs)
    takeN (_ : ns) (x : xs) = let (a, r) = takeN ns xs in (x : a, r)
    takeN _ [] = ([], [])

runeBytes :: Rune -> [Byte]
runeBytes (Rune bs) = bs

-- | The rune's symbol in html.wsn's alphabet.
cast :: Rune -> Byte
cast (Rune [b]) = case letter b of
  Yes -> lowerOf b
  No -> case either' (digit b) (isIn b kept) of
    Yes -> b
    No -> case white b of
      Yes -> space
      No -> tilde
cast _ = tilde

castAll :: [Rune] -> [Byte]
castAll = mapL cast

is :: Byte -> Rune -> Answer
is b (Rune [c]) = eqByte b c
is _ _ = No

-- | the runes begin with these symbols, case aside
begins :: [Byte] -> [Rune] -> Answer
begins [] _ = Yes
begins (b : bs) (r : rs) = both (eqByte b (cast r)) (begins bs rs)
begins _ [] = No

-- ---------------------------------------------------------------- pieces

pieceKind :: Piece -> Kind
pieceKind (Piece k _ _) = k

pieceRunes :: Piece -> [Rune]
pieceRunes (Piece _ rs _) = rs

pieceBytes :: Piece -> [Byte]
pieceBytes p = concatL (mapL runeBytes (pieceRunes p))

pieceCast :: Piece -> [Byte]
pieceCast p = castAll (pieceRunes p)

pieceName :: Piece -> [Byte]
pieceName (Piece _ _ n) = n

-- | The production of html.wsn a piece belongs to.
production :: Kind -> [Byte]
production Comment = [B x4 x3, B x4 xF, B x4 xD, B x4 xD, B x4 x5, B x4 xE, B x5 x4]
production Doctype = [B x4 x4, B x4 xF, B x4 x3, B x5 x4, B x5 x9, B x5 x0, B x4 x5]
production StartTag = [B x5 x3, B x5 x4, B x4 x1, B x5 x2, B x5 x4, B x5 x4, B x4 x1, B x4 x7]
production EndTag = [B x4 x5, B x4 xE, B x4 x4, B x5 x4, B x4 x1, B x4 x7]
production Text = [B x5 x4, B x4 x5, B x5 x8, B x5 x4]
production RawText = [B x5 x2, B x4 x1, B x5 x7, B x5 x4, B x4 x5, B x5 x8, B x5 x4]

commentOpen, commentClose, doctypeOpen :: [Byte]
commentOpen = [lt, bang, dash, dash]
commentClose = [dash, dash, gt]
doctypeOpen = [lt, bang, B x6 x4, B x6 xF, B x6 x3, B x7 x4, B x7 x9, B x7 x0, B x6 x5]

-- script style title textarea: their text is raw, to the first end of their name
rawNames :: [[Byte]]
rawNames =
  [ [B x7 x3, B x6 x3, B x7 x2, B x6 x9, B x7 x0, B x7 x4]
  , [B x7 x3, B x7 x4, B x7 x9, B x6 xC, B x6 x5]
  , [B x7 x4, B x6 x9, B x7 x4, B x6 xC, B x6 x5]
  , [B x7 x4, B x6 x5, B x7 x8, B x7 x4, B x6 x1, B x7 x2, B x6 x5, B x6 x1]
  ]

-- area base br col embed hr img input link meta param source track wbr: void
voidNames :: [[Byte]]
voidNames =
  [ [B x6 x1, B x7 x2, B x6 x5, B x6 x1], [B x6 x2, B x6 x1, B x7 x3, B x6 x5], [B x6 x2, B x7 x2]
  , [B x6 x3, B x6 xF, B x6 xC], [B x6 x5, B x6 xD, B x6 x2, B x6 x5, B x6 x4], [B x6 x8, B x7 x2]
  , [B x6 x9, B x6 xD, B x6 x7], [B x6 x9, B x6 xE, B x7 x0, B x7 x5, B x7 x4], [B x6 xC, B x6 x9, B x6 xE, B x6 xB]
  , [B x6 xD, B x6 x5, B x7 x4, B x6 x1], [B x7 x0, B x6 x1, B x7 x2, B x6 x1, B x6 xD]
  , [B x7 x3, B x6 xF, B x7 x5, B x7 x2, B x6 x3, B x6 x5], [B x7 x4, B x7 x2, B x6 x1, B x6 x3, B x6 xB]
  , [B x7 x7, B x6 x2, B x7 x2]
  ]

-- | Runes up to and including the first occurrence of these symbols, when
--   found; the whole rest when not.
data Upto = Found [Rune] [Rune] | Ends [Rune]

upto :: [Byte] -> [Rune] -> Upto
upto pat rs = case begins pat rs of
  Yes -> let (a, b) = splitN pat rs in Found a b
  No -> case rs of
    [] -> Ends []
    r : rest -> case upto pat rest of
      Found a b -> Found (r : a) b
      Ends a -> Ends (r : a)
  where
    splitN [] xs = ([], xs)
    splitN (_ : ps) (x : xs) = let (a, b) = splitN ps xs in (x : a, b)
    splitN _ [] = ([], [])

-- | Runes before the first occurrence of the symbols (left in the rest).
before :: [Byte] -> [Rune] -> ([Rune], [Rune])
before pat rs = case begins pat rs of
  Yes -> ([], rs)
  No -> case rs of
    [] -> ([], [])
    r : rest -> let (a, b) = before pat rest in (r : a, b)

-- | A tag's runes from after its name to its ">", outside a quoted value: a
--   quote opens a value when the nearest rune before it, spaces aside, is "=".
--   The rune before is carried as the scan goes ('prev', the last non-space).
tagRest :: [Rune] -> Upto
tagRest = go No
  where
    go _ [] = Ends []
    go afterEq (r : rest) = case is gt r of
      Yes -> Found [r] rest
      No -> case either' (is dq r) (is sq r) of
        Yes -> case afterEq of
          Yes -> case quoted (cast r) rest of
            Found a b -> prefix (r : a) (go No b)
            Ends a -> Ends (r : a)
          No -> prefix [r] (go No rest)
        No -> prefix [r] (go (next afterEq r) rest)
    next afterEq r = case white (cast r) of
      Yes -> afterEq
      No -> is eq r
    quoted _ [] = Ends []
    quoted q (r : rest) = case eqByte q (cast r) of
      Yes -> Found [r] rest
      No -> case quoted q rest of
        Found a b -> Found (r : a) b
        Ends a -> Ends (r : a)
    prefix a (Found b c) = Found (append a b) c
    prefix a (Ends b) = Ends (append a b)

nameRunes :: [Rune] -> ([Rune], [Rune])
nameRunes [] = ([], [])
nameRunes (r : rest) = case nameChar (cast r) of
  Yes -> let (a, b) = nameRunes rest in (r : a, b)
  No -> ([], r : rest)

-- | A tag at the head of the runes: its kind, runes, name, the rest.
data Tag = Tag Kind [Rune] [Byte] [Rune] | NoTag

tagAt :: [Rune] -> Tag
tagAt (l : rest) = case is lt l of
  No -> NoTag
  Yes -> case rest of
    (s : after) -> case is slash s of
      Yes -> named EndTag [l, s] after
      No -> named StartTag [l] rest
    [] -> NoTag
  where
    named k open (c : cs) = case letter (cast c) of
      Yes -> let (nm, more) = nameRunes (c : cs) in case tagRest more of
        Found tail' r -> Tag k (append open (append nm tail')) (castAll nm) r
        Ends _ -> NoTag
      No -> NoTag
    named _ _ [] = NoTag
tagAt [] = NoTag

selfClosed :: [Rune] -> Answer
selfClosed rs = case reverse' rs of
  (_ : s : _) -> is slash s
  _ -> No
  where
    reverse' = go []
    go acc [] = acc
    go acc (x : xs) = go (x : acc) xs

-- | A piece begins here: a comment, the doctype, or a tag that ends.
pieceAt :: [Rune] -> Answer
pieceAt rs = case begins commentOpen rs of
  Yes -> Yes
  No -> case begins doctypeOpen rs of
    Yes -> Yes
    No -> case tagAt rs of
      Tag _ _ _ _ -> Yes
      NoTag -> No

-- | The page's pieces, in order, read as they are asked for (html.rules 1, 2).
pieces :: [Byte] -> [Piece]
pieces bs = go (runes bs)
  where
    go [] = []
    go rs = case begins commentOpen rs of
      Yes -> case upto commentClose (drop4 rs) of
        Found a r -> Piece Comment (append (take4 rs) a) [] : go r
        Ends a -> Piece Text (append (take4 rs) a) [] : []
      No -> case begins doctypeOpen rs of
        Yes -> case upto [gt] rs of
          Found a r -> Piece Doctype a [] : go r
          Ends a -> Piece Doctype a [] : []
        No -> case tagAt rs of
          Tag k a name r -> Piece k a name : raw k a name r
          NoTag -> text rs
    raw StartTag a name r = case both (member name rawNames) (not' (selfClosed a)) of
      Yes -> let (t, rest) = before (lt : slash : name) r in case t of
        [] -> go rest
        _ -> Piece RawText t [] : go rest
      No -> go r
    raw _ _ _ r = go r
    -- text runs to the next rune where a piece begins
    text (r : rest) = let (t, more) = textRun rest in Piece Text (r : t) [] : go more
    text [] = []
    textRun [] = ([], [])
    textRun rs@(r : rest) = case both (is lt r) (pieceAt rs) of
      Yes -> ([], rs)
      No -> let (a, b) = textRun rest in (r : a, b)
    take4 (a : b : c : d : _) = [a, b, c, d]
    take4 xs = xs
    drop4 (_ : _ : _ : _ : xs) = xs
    drop4 _ = []

-- ---------------------------------------------------------------- the tree

-- | The tree of a page by html.rules 3, built lazily: an element's children
--   are a list built as they are reached.
tree :: [Byte] -> [Node]
tree bs = case forest [] (pieces bs) of (ns, _) -> ns

forest :: [[Byte]] -> [Piece] -> ([Node], [Piece])
forest _ [] = ([], [])
forest open ps@(p : rest) = case pieceKind p of
  EndTag -> case member (pieceName p) open of
    Yes -> ([], ps)
    No -> let (ns, r) = forest open rest in (Leaf p : ns, r)
  StartTag -> case either' (member (pieceName p) voidNames) (selfClosed (pieceRunes p)) of
    Yes -> let (ns, r) = forest open rest in (Leaf p : ns, r)
    No ->
      let (kids, r1) = forest (pieceName p : open) rest
          (end, r2) = closing (pieceName p) r1
          (ns, r3) = forest open r2
      in (Element p kids end : ns, r3)
  _ -> let (ns, r) = forest open rest in (Leaf p : ns, r)
  where
    closing name (q : r) = case pieceKind q of
      EndTag -> case eqBytes name (pieceName q) of
        Yes -> (Closed q, r)
        No -> (Unclosed, q : r)
      _ -> (Unclosed, q : r)
    closing _ [] = (Unclosed, [])

-- | The page again, from its tree: each piece's runes in order.
flatten :: [Node] -> [Byte]
flatten ns = concatL (mapL node ns)
  where
    node (Leaf p) = pieceBytes p
    node (Element p kids end) = append (pieceBytes p) (append (flatten kids) (endBytes end))
    endBytes (Closed q) = pieceBytes q
    endBytes Unclosed = []

-- | The tree, a line a piece: two spaces a level, the production, a space, the
--   piece's bytes (a line feed written \n, a backslash \\); an element left
--   open is marked by a line "OPEN" at its children's level.
outline :: [Node] -> [Byte]
outline = level []
  where
    level ind ns = concatL (mapL (one ind) ns)
    one ind (Leaf p) = line ind p
    one ind (Element p kids end) = append (line ind p) (append (level (two : ind) kids) (close (two : ind) ind end))
    close _ ind (Closed q) = line ind q
    close kidInd _ Unclosed = append (concatL kidInd) (append [B x4 xF, B x5 x0, B x4 x5, B x4 xE] [nl])
    line ind p = append (concatL ind) (append (production (pieceKind p)) (space : append (escape (pieceBytes p)) [nl]))
    two = [space, space]
    nl = B x0 xA
    bs = B x5 xC
    escape [] = []
    escape (b : r) = case eqByte b nl of
      Yes -> bs : B x6 xE : escape r
      No -> case eqByte b bs of
        Yes -> bs : bs : escape r
        No -> b : escape r
