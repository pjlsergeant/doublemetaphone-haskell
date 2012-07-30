
module Text.DoubleMetaphone (doubleMetaphone, doubleMetaphoneDebug)
where

import Prelude
import Data.Map
import Data.Char

-- Temporary test cases go here, until it's time to split them out
runTests :: [(String,[String],[String])]
runTests =  Prelude.map (\x -> ((fst x),(snd x),(doubleMetaphone (fst x)))) (
            Prelude.filter (\x -> (doubleMetaphone (fst x)) /= (snd x) ) [
               ("Edge",  ["AJ"]),
               ("X",     ["S"]),
               ("BB",    ["P"]),
               ("Canary",["Canard"] )
            ])
-- End tests...

-- This holds the current state of the world
type Context = (Map String Int, Map String String)
defaultContext =  (
                    -- Integer values
                    fromList([
                      ( "skip",     0 :: Int ),
                      ( "position", 0 :: Int )
                    ]),
                    -- String values
                    fromList([
                      ( "primary",   "" ),
                      ( "secondary", "" ),
                      ( "original",  "" )
                    ])
                  )

--
-- These are the interface functions that we actually use to interact with the
-- context
--

-- Skip the next input letter
skip :: Context -> Context
skip context = cInc "skip" 1 context

-- Add a string to primary and secondary outputs
add :: String -> Context -> Context
add str context = addPrimary str (addSecondary str context)

-- Add a string to just the primary output
addPrimary :: String -> Context -> Context
addPrimary str context = cApp "primary" str context

-- Add a string to just the secondary output
addSecondary :: String -> Context -> Context
addSecondary str context = cApp "secondary" str context

isVowel :: Char -> Bool
isVowel x = x `elem` "AEIOUY"

--
-- These are the helper functions for the interface functions above
--

-- Increment one of the integer values
cInc :: String -> Int -> Context -> Context
cInc key increment context = (adjust (+ increment) key (fst context), snd context)

-- Append to a string value
cApp :: String -> String -> Context -> Context
cApp key value context = (fst context, (adjust (++ value) key (snd context)) )

-- Lookup an integer from the context
cLookupInt :: String -> Context -> Int
cLookupInt key context = findWithDefault (error "Unknown int") key (fst context)

-- Lookup a string from the context
cLookupStr :: String -> Context -> String
cLookupStr key context = findWithDefault (error "Unknown str") key (snd context)

--
-- Here is our external interface
--

-- Return a list of one or two strings with the metaphone equivalent(s) of the
-- input string.
doubleMetaphone :: String -> [String]
doubleMetaphone input = if primary /= secondary && (length secondary) > 0
                          then [primary,secondary]
                          else [primary]
                        where context   = doubleMetaphoneDebug input
                              primary   = cLookupStr "primary"   context
                              secondary = cLookupStr "secondary" context

-- Run the algorithm, but return the whole context instead of just the hashed
-- strings
doubleMetaphoneDebug :: String -> Context
doubleMetaphoneDebug input = m (Prelude.map (toUpper) input) defaultContext

-- This is the recursive function that does the transform. We pull out some
-- very useful pieces of data in the 'where' clause at the bottom, and it's
-- between those and the interface functions above where we do the magic...
m :: String -> Context -> Context
m [] c = c
m full@(x:xs) context -- Use 'c', not 'context', as per the 'where' section

  -- When we've found enough characters, we're all done
  | any (\s -> (length (cLookupStr s context) > 3) ) [ "primary", "secondary" ] = c

  -- Skip over characters if we're meant to
  | toSkip > 0 = next (cInc "skip" (0-1))

  -- There are a funny set of consonants at the beginning of names where we want
  -- to pretend we didn't see the first letter. Matching without doing anything
  -- does that
  | isStart && xx `elem` ["GN", "KN", "PN", "WR", "PS"] = next (id)

  -- Initial 'X' is pronounced 'Z' e.g. 'Xavier'
  | isStart && x == 'X' = next (add "S")

  -- Match any starting vowels, and translate them directly to A
  | isStart && isVowel x = next (add "A")

  -- B
  | xx == "BB" = next (add "P" . skip) -- BB -> P
  | x  == 'B'  = next (add "P")        -- B  -> P

  -- Combinations like 'edge'
  | xxx `elem` ["DGI","DGE","DGY"] = next (add "J" . skip . skip)

  -- Combinations like 'edgar'
  | xx == "DG" = next (add "TK" . skip)

  -- D
  | (xx == "DD" || xx == "DT") = next (add "T" . skip)
  | x == 'D' = next (add "T")

  -- -- F
  -- | x2 == "FF"
  --  = conc xs (hInc "skip" 1 (addS "F" c))
  -- | x  == 'F'
  --  = conc xs (addS "F" c)

  -- Default case, do nothing!
  | otherwise = next (add "*")

-- Here we unpack the context
  where
        -- Number of letters we're meant to be skipping
        toSkip = cLookupInt "skip" context

        -- The original string, for reference
        original = cLookupInt "original" context

        -- The current position
        position = cLookupInt "position" context

        -- Create a copy of the context where the position is already
        -- incremented
        c         = cInc "position" 1 context

        -- Some shortcuts to make our life a little easier
        xx        = take 2 full -- this character and the next
        xxx       = take 3 full -- this character and the next two
        -- xm1       = if (position == 0) then "" else original !! (position - 1)
        xp1       = take 1 xs -- The next character, alone
        isStart   = position == 0

        -- Apply the context to whatever expression we were given, and recurse
        next = ( \expression -> (m xs (expression c)) )

