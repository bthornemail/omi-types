{-# LANGUAGE NoImplicitPrelude #-}

-- | Wirth syntax notation, typed, and its execution under a zeroth's cast.
--   Tracks unbound/PLAN9: wsn/stack.wsn, src/zcast.c, src/zlazy.c.
--
--   WSN defined in itself (Wirth, 1977;
--   https://en.wikipedia.org/wiki/Wirth_syntax_notation):
--
--   > SYNTAX     = { PRODUCTION } .
--   > PRODUCTION = IDENTIFIER "=" EXPRESSION "." .
--   > EXPRESSION = TERM { "|" TERM } .
--   > TERM       = FACTOR { FACTOR } .
--   > FACTOR     = IDENTIFIER | LITERAL | "[" EXPRESSION "]"
--   >            | "(" EXPRESSION ")" | "{" EXPRESSION "}" .
--   > IDENTIFIER = letter { letter } .
--   > LITERAL    = """" character { character } """" .
--
--   letter and character are left undefined, for the language defined to
--   settle. Here an identifier is upper case (A-Z) and a character any byte;
--   an identifier with no production matches nothing. 'cast' settles a
--   letter by a zeroth, through boxedPINE src/terms.mjs's feature pipeline:
--   a consonant stays upper, a vowel (AEIOU) is lowered 0x20, a digit stays.
--   YHWH casts no vowel; YAHWEH casts "a" and "e". No integers, no strings:
--   text is a list of 'Byte'.
module OMI.WSN
  ( Answer(..)
  , Read(..)
  , Syntax
  , parse
  , render
  , extend
  , accepts
  , cast
  ) where

import OMI.Kernel

data Answer = Yes | No

data Read a = Read a | NoRead

data Syntax = Syntax [Production]

data Production = Production [Byte] Expression

data Expression = Expression [Term]

data Term = Term [Factor]

data Factor
  = Ident [Byte]
  | Lit [Byte]
  | Opt Expression
  | Group Expression
  | Rep Expression

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

eqBit :: Bit -> Bit -> Answer
eqBit O O = Yes
eqBit I I = Yes
eqBit _ _ = No

both :: Answer -> Answer -> Answer
both Yes Yes = Yes
both _ _ = No

either' :: Answer -> Answer -> Answer
either' No No = No
either' _ _ = Yes

eqNibble :: Nibble -> Nibble -> Answer
eqNibble (N a b c d) (N e f g h) = both (eqBit a e) (both (eqBit b f) (both (eqBit c g) (eqBit d h)))

eqByte :: Byte -> Byte -> Answer
eqByte (B a b) (B c d) = both (eqNibble a c) (eqNibble b d)

eqBytes :: [Byte] -> [Byte] -> Answer
eqBytes [] [] = Yes
eqBytes (a : as) (b : bs) = both (eqByte a b) (eqBytes as bs)
eqBytes _ _ = No

member :: Byte -> [Byte] -> Answer
member _ [] = No
member b (c : cs) = either' (eqByte b c) (member b cs)

-- | 0x20, the supervisor bit: the case of a letter.
flip20 :: Byte -> Byte
flip20 (B (N a b c d) lo) = B (N a b (notBit c) d) lo
  where
    notBit O = I
    notBit I = O

-- | The right fold: each cons replaced by f, the nil by z. Lazy in the rest,
--   so a function that answers without it stops the fold (a short circuit).
foldrL :: (a -> b -> b) -> b -> [a] -> b
foldrL _ z [] = z
foldrL f z (x : xs) = f x (foldrL f z xs)

-- | The left fold: the order of the list, carried in the accumulator.
foldlL :: (b -> a -> b) -> b -> [a] -> b
foldlL _ z [] = z
foldlL f z (x : xs) = foldlL f (f z x) xs

append :: [a] -> [a] -> [a]
append xs ys = foldrL (\x r -> x : r) ys xs

concatL :: [[a]] -> [a]
concatL = foldrL append []

mapL :: (a -> b) -> [a] -> [b]
mapL f = foldrL (\x r -> f x : r) []

-- The characters the notation spells with.
quote, space, eq, dot, bar, lbrack, rbrack, lparen, rparen, lbrace, rbrace, newline, tab, cr :: Byte
quote = B x2 x2
space = B x2 x0
eq = B x3 xD
dot = B x2 xE
bar = B x7 xC
lbrack = B x5 xB
rbrack = B x5 xD
lparen = B x2 x8
rparen = B x2 x9
lbrace = B x7 xB
rbrace = B x7 xD
newline = B x0 xA
tab = B x0 x9
cr = B x0 xD

upper, lower, digits, vowels :: [Byte]
upper = append (mapL (B x4) [x1, x2, x3, x4, x5, x6, x7, x8, x9, xA, xB, xC, xD, xE, xF])
               (mapL (B x5) [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xA])
lower = mapL flip20 upper
digits = mapL (B x3) [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9]
vowels = [B x4 x1, B x4 x5, B x4 x9, B x4 xF, B x5 x5]

-- Names and literals the cast writes.
consonantN, vowelN, digitN, letterN, emptyN, heldN, outL, bangL :: [Byte]
consonantN = [B x4 x3, B x4 xF, B x4 xE, B x5 x3, B x4 xF, B x4 xE, B x4 x1, B x4 xE, B x5 x4]
vowelN = [B x5 x6, B x4 xF, B x5 x7, B x4 x5, B x4 xC]
digitN = [B x4 x4, B x4 x9, B x4 x7, B x4 x9, B x5 x4]
letterN = [B x4 xC, B x4 x5, B x5 x4, B x5 x4, B x4 x5, B x5 x2]
emptyN = [B x4 x5, B x4 xD, B x5 x0, B x5 x4, B x5 x9]
heldN = [B x4 x8, B x4 x5, B x4 xC, B x4 x4]
outL = [B x6 xF, B x7 x5, B x7 x4]
bangL = [B x2 x1]

-- ---------------------------------------------------------------- parse

data Step a = Step a [Byte] | Fail

skip :: [Byte] -> [Byte]
skip [] = []
skip (c : cs) = case member c [space, newline, tab, cr] of
  Yes -> skip cs
  No -> c : cs

-- | One expected character, after any white space.
sym :: Byte -> [Byte] -> Step ()
sym c s = case skip s of
  (d : ds) -> case eqByte c d of
    Yes -> Step () ds
    No -> Fail
  [] -> Fail

identifier :: [Byte] -> Step [Byte]
identifier s = case skip s of
  (c : cs) -> case member c upper of
    Yes -> let (n, rest) = letters cs in Step (c : n) rest
    No -> Fail
  [] -> Fail
  where
    letters (d : ds) = case member d upper of
      Yes -> let (n, rest) = letters ds in (d : n, rest)
      No -> ([], d : ds)
    letters [] = ([], [])

-- | A literal: a quote, at least one character (a quote inside written twice), a quote.
literal :: [Byte] -> Step [Byte]
literal s = case skip s of
  (q : cs) -> case eqByte q quote of
    Yes -> case chars cs of
      Step [] _ -> Fail
      r -> r
    No -> Fail
  [] -> Fail
  where
    chars (d : ds) = case eqByte d quote of
      Yes -> case ds of
        (e : es) -> case eqByte e quote of
          Yes -> case chars es of
            Step t rest -> Step (quote : t) rest
            Fail -> Fail
          No -> Step [] ds
        [] -> Step [] []
      No -> case chars ds of
        Step t rest -> Step (d : t) rest
        Fail -> Fail
    chars [] = Fail

factor :: [Byte] -> Step Factor
factor s = case identifier s of
  Step n r -> Step (Ident n) r
  Fail -> case literal s of
    Step l r -> Step (Lit l) r
    Fail -> bracketed lbrack rbrack Opt (bracketed lparen rparen Group (bracketed lbrace rbrace Rep Fail s) s) s
  where
    bracketed open close k orElse t = case sym open t of
      Step () r -> case expression r of
        Step e r' -> case sym close r' of
          Step () r'' -> Step (k e) r''
          Fail -> Fail
        Fail -> Fail
      Fail -> orElse

term :: [Byte] -> Step Term
term s = case factor s of
  Step f r -> let (fs, r') = more r in Step (Term (f : fs)) r'
  Fail -> Fail
  where
    more t = case factor t of
      Step f r -> let (fs, r') = more r in (f : fs, r')
      Fail -> ([], t)

expression :: [Byte] -> Step Expression
expression s = case term s of
  Step t r -> let (ts, r') = more r in Step (Expression (t : ts)) r'
  Fail -> Fail
  where
    more u = case sym bar u of
      Step () r -> case term r of
        Step t r' -> let (ts, r'') = more r' in (t : ts, r'')
        Fail -> ([], u)
      Fail -> ([], u)

production :: [Byte] -> Step Production
production s = case identifier s of
  Step n r -> case sym eq r of
    Step () r' -> case expression r' of
      Step e r'' -> case sym dot r'' of
        Step () r''' -> Step (Production n e) r'''
        Fail -> Fail
      Fail -> Fail
    Fail -> Fail
  Fail -> Fail

-- | A whole text as SYNTAX = { PRODUCTION } ., nothing left over but white space.
parse :: [Byte] -> Read Syntax
parse s = go s
  where
    go t = case skip t of
      [] -> Read (Syntax [])
      u -> case production u of
        Step p r -> case go r of
          Read (Syntax ps) -> Read (Syntax (p : ps))
          NoRead -> NoRead
        Fail -> NoRead

-- ---------------------------------------------------------------- render

-- | The text, one production a line, as zcast and zwsn write it.
render :: Syntax -> [Byte]
render (Syntax ps) = concatL (mapL prod ps)
  where
    prod (Production n e) = concatL [n, [space, eq, space], expr e, [space, dot, newline]]
    expr (Expression ts) = sepBy [space, bar, space] (mapL trm ts)
    trm (Term fs) = sepBy [space] (mapL fac fs)
    fac (Ident n) = n
    fac (Lit l) = quote : append (escape l) [quote]
    fac (Opt e) = concatL [[lbrack, space], expr e, [space, rbrack]]
    fac (Group e) = concatL [[lparen, space], expr e, [space, rparen]]
    fac (Rep e) = concatL [[lbrace, space], expr e, [space, rbrace]]
    escape [] = []
    escape (c : cs) = case eqByte c quote of
      Yes -> quote : quote : escape cs
      No -> c : escape cs
    sepBy _ [] = []
    sepBy _ [x] = x
    sepBy s (x : xs) = append x (append s (sepBy s xs))

-- | One grammar after another, as cast.rc concatenates the files.
extend :: Syntax -> Syntax -> Syntax
extend (Syntax a) (Syntax b) = Syntax (append a b)

-- ---------------------------------------------------------------- accepts

-- | Where a match may end, and whether it took anything.
data Reach = Reach [Byte] Answer

-- | Does the sentence belong to the production, as zlazy asks? An identifier
--   with no production matches nothing. An identifier met again before any
--   character is taken is not expanded again (it would only repeat).
accepts :: Syntax -> [Byte] -> [Byte] -> Answer
accepts g name s = foldrL whole No (factorR g [] (Ident name) s)
  where
    -- a right fold: the first reach that takes the whole sentence answers,
    -- and the reaches after it are never built
    whole (Reach [] _) _ = Yes
    whole _ rest = rest

lookupP :: Syntax -> [Byte] -> Read Expression
lookupP (Syntax []) _ = NoRead
lookupP (Syntax (Production n e : ps)) m = case eqBytes n m of
  Yes -> Read e
  No -> lookupP (Syntax ps) m

expressionR :: Syntax -> [[Byte]] -> Expression -> [Byte] -> [Reach]
expressionR g seen (Expression ts) s = concatL (mapL (\t -> termR g seen t s) ts)

termR :: Syntax -> [[Byte]] -> Term -> [Byte] -> [Reach]
termR _ _ (Term []) s = [Reach s No]
termR g seen (Term (f : fs)) s = concatL (mapL next (factorR g seen f s))
  where
    next (Reach r took) =
      let seen' = case took of
            Yes -> []
            No -> seen
      in mapL (\(Reach r' took') -> Reach r' (either' took took')) (termR g seen' (Term fs) r)

factorR :: Syntax -> [[Byte]] -> Factor -> [Byte] -> [Reach]
factorR g seen (Ident n) s = case seenBefore seen of
  Yes -> []
  No -> case lookupP g n of
    Read e -> expressionR g (n : seen) e s
    NoRead -> []
  where
    seenBefore [] = No
    seenBefore (m : ms) = either' (eqBytes m n) (seenBefore ms)
factorR _ _ (Lit l) s = prefix l s
  where
    prefix [] r = [Reach r Yes]
    prefix (c : cs) (d : ds) = case eqByte c d of
      Yes -> prefix cs ds
      No -> []
    prefix _ [] = []
factorR g seen (Opt e) s = Reach s No : expressionR g seen e s
factorR g seen (Group e) s = expressionR g seen e s
factorR g seen (Rep e) s = Reach s No : concatL (mapL again (expressionR g seen e s))
  where
    again (Reach r Yes) = mapL (\(Reach r' _) -> Reach r' Yes) (factorR g [] (Rep e) r)
    again (Reach _ No) = []

-- ---------------------------------------------------------------- cast

-- | A zeroth's type cast, as zcast writes it: each class that holds a
--   character becomes a production of its characters in the order they
--   first appear, with its held step for the stack element; a class that
--   holds none gets no production.
cast :: [Byte] -> Syntax
cast z = Syntax (concatL
  [ classP consonantN cs, classP vowelN vs, classP digitN ds
  , [Production letterN (Expression [Term [Ident consonantN], Term [Ident vowelN]])]
  , heldP consonantN cs, heldP vowelN vs, heldP digitN ds ])
  where
    (cs, vs, ds) = classify z ([], [], [])
    classP _ [] = []
    classP n xs = [Production n (Expression (mapL (\c -> Term [Lit [c]]) xs))]
    heldP _ [] = []
    heldP n xs = [Production (append heldN n) (Expression (mapL held xs))]
    held c = Term [Lit [c], Opt (Expression [Term [Lit outL, Lit bangL, Lit [c], Ident emptyN]])]

-- | terms.mjs classify: upper case first, then a vowel lowered 0x20; a digit
--   stays. A left fold over the zeroth, so each class keeps the order its
--   characters first appear in. foldl, not foldl': seq would force only the
--   triple's constructor, and a zeroth is a handful of bytes.
classify :: [Byte] -> ([Byte], [Byte], [Byte]) -> ([Byte], [Byte], [Byte])
classify z acc = foldlL one acc z

one :: ([Byte], [Byte], [Byte]) -> Byte -> ([Byte], [Byte], [Byte])
one (cs, vs, ds) c = next
  where
    up = case member c lower of
      Yes -> flip20 c
      No -> c
    add x xs = case member x xs of
      Yes -> xs
      No -> append xs [x]
    next = case member c digits of
      Yes -> (cs, vs, add c ds)
      No -> case member up upper of
        No -> (cs, vs, ds)
        Yes -> case member up vowels of
          Yes -> (cs, add (flip20 up) vs, ds)
          No -> (add up cs, vs, ds)
