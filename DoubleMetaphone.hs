import Data.List

data HVal = HInt Int | HStr String deriving (Eq, Ord, Show, Read)
instance Num HVal where
    HInt a + HInt b = HInt (a + b)
    fromInteger = fromInteger

type SHash = [(String, HVal)]

doubleMetaphone :: String -> SHash
doubleMetaphone input = conc input [
                          ("primary",   HStr ""),
                          ("secondary", HStr ""),
                          ("skip",      HInt 0 ),
                          ("original",  HStr input ),
                          ("position",  HInt 0 )
                        ]

conc :: String -> SHash -> SHash
conc [] c = c
conc full@(x:xs) context

-- Meta-cases for managing the recursion
  -- Keep going while either of our accumlators are smaller than four
  | (length primary) > 3 || (length secondary) > 3 = context

  -- Skip over characters if we're meant to
  | skip > 0 = conc xs (hInc "skip" (0-1) c)

-- Things that occur at the beginning of a name
  -- There are a funny set of consonants at the beginning of names where we want
  -- to pretend we didn't see the first letter. Matching without doing anything
  -- does that
  | isStart && x2 `elem` ["GN", "KN", "PN", "WR", "PS"]
    = conc xs c
  -- Initial 'X' is pronounced 'Z' e.g. 'Xavier'
  | isStart && x == 'X'
    = conc xs (addS "S" c)

  -- Match any starting vowels, and translate them directly to A
  | isStart && isVowel x
    = conc xs (addS "A" c)

-- Everything else...
  -- B
  | x2 == "BB" = conc xs (hInc "skip" 1 (addS "P" c))
  | x  == 'B'  = conc xs (addS "P" c)

  -- D
  | x2 == "DG" && x3 `elem` ["DGI","DGE","DGY"] -- edge
    = conc xs (hInc "skip" 2 (addS "J" c))
  | x2 == "DG" -- edgar
    = conc xs (hInc "skip" 1 (addS "TK" c))
  | x2 == "DD" || x2 == "DT"
    = conc xs (hInc "skip" 1 (addS "T" c))
  | x == 'D'
    = conc xs (addS "T" c)

  -- F
  | x2 == "FF"
    = conc xs (hInc "skip" 1 (addS "F" c))
  | x  == 'F'
    = conc xs (addS "F" c)

  -- H - only show it if it's at the start of between two vowels
  | ( x == 'H' && isStart ) || (isVowel xp1 && isVowel xm1)
    = conc xs (hInc "skip" 1 (addS "H" c))

  -- Default case, do nothing!
  | otherwise = conc xs (hApp "primary" ['*',x] c)

-- Here we unpack the context
        -- We build up the main and alternative spelling in these
  where primary   = hStr (hLookup "primary"   context)
        secondary = hStr (hLookup "secondary" context)
        -- How many letters to skip
        skip      = hInt (hLookup "skip"      context)
        -- The original string for reference
        original  = hStr (hLookup "original"  context)
        -- The current position
        position  = hInt (hLookup "position"  context)
        -- Create a copy of the context where the position is already
        -- incremented
        c         = hInc "position" 1 context
        -- Some shortcuts to make our life a little easier
        x2        = take 2 full -- this character and the next
        x3        = take 3 full -- this character and the next two
        xm1       = if (position == 0) then "" else original !! (position - 1)
        xp1       = take 1 xs -- The next character, alone
        isStart   = position == 0

isVowel :: Char -> Bool
isVowel x = x `elem` "AEIOUY"

hInt (HInt a) = a
hStr (HStr a) = a

-- Add a letter to primary and secondary
addS :: String -> SHash -> SHash
addS x c = (hApp "primary" x (hApp "secondary" x c))

-- Join two hstrings
hStrConcat :: HVal -> HVal -> HVal
hStrConcat (HStr a) (HStr b) = HStr (a ++ b)

-- Combine two hashes
hMerge :: SHash -> SHash -> SHash
hMerge left right = unionBy (\x y -> (fst x) == (fst y)) right left

-- Short-hand to setting a single value in the hash
hSet :: String -> HVal -> SHash -> SHash
hSet key val from = hMerge from [(key, val)]

-- Increment an HVal in the hash
hInc :: String -> Int -> SHash -> SHash
hInc key val from = hSet key ((HInt val) + (hLookup key from)) from

-- Add to an HStr in the hash
hApp :: String -> String -> SHash -> SHash
hApp key val from = hSet key (hStrConcat (hLookup key from) (HStr val)) from

-- This implementation requires that the key actually exist, or it'll throw,
-- but that's good enough for us, and no messin' about with Maybe
hLookup :: String -> SHash -> HVal
hLookup key (x:xs)
    | fst(x) == key = snd(x)
    | otherwise = (hLookup key xs)