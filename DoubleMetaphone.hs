
-- module Text.DoubleMetaphone (doubleMetaphone, doubleMetaphoneDebug)
-- where

import Prelude
import Data.Map
import Data.Char
import Data.List

-- Temporary test cases go here, until it's time to split them out
runTests :: [(String,[String],[String])]
runTests =  Prelude.map (\x -> ((fst x),(snd x),(doubleMetaphone (fst x)))) (
            Prelude.filter (\x -> (doubleMetaphone (fst x)) /= (snd x) ) [
               ("Edge",  ["AJ"]),
               ("HA",    ["H"]),
               ("XHA",   ["S"]),
               ("AHA",   ["AH"]),
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
doubleMetaphoneDebug input = m bigInput (cApp "original" bigInput defaultContext)
  where bigInput = Prelude.map (toUpper) input

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
  | isFirst && starts ["GN", "KN", "PN", "WR", "PS"] = next (id)

  -- Initial 'X' is pronounced 'Z' e.g. 'Xavier'
  | isFirst && starts ["X"] = next (add "S")

  -- Match any starting vowels, and translate them directly to A. Other vowels
  -- are skipped
  | isFirst && isVowel x = next (add "A")
  | isVowel x = next (id)

  -- B
  | starts["BB"] = next (add "P" . skip) -- BB -> P
  | starts["B"]  = next (add "P")        -- B  -> P

  -- D
  -- Combinations like 'edge'
  | starts ["DGI","DGE","DGY"] = next (add "J" . skip . skip)
  -- Combinations like 'edgar'
  | starts ["DG"] = next (add "TK" . skip)
  -- Just a simple starting D
  | starts ["DD","DT"] = next (add "T" . skip)
  | starts ["D"]       = next (add "T")

  -- F
  | starts["FF"] = next (add "F" . skip)
  | starts["F"]  = next (add "F")

  -- H - We include an H when it's followed by a vowel, and preceeded either by
  --     a vowel or the start of the string
  | starts["H"] && (isFirst || isVowel (relChar (-1)) ) && isVowel (relChar 1)
    = next (add "H")
  | starts["H"] = next (id)

  -- K
  | starts["KK"] = next (add "K" . skip)
  | starts["K"]  = next (add "K")

  -- Default case, do nothing!
  | otherwise = next (add "*")

-- Here we unpack the context
  where
        -- Number of letters we're meant to be skipping
        toSkip = cLookupInt "skip" context

        -- The original string, for reference
        original = cLookupStr "original" context

        -- The current position
        position = cLookupInt "position" context

        -- Create a copy of the context where the position is already
        -- incremented (for the next iteration)
        c = cInc "position" 1 context

        -- A mashup of isPrefixOf and any
        starts = ( \prefixes -> any (\prefix -> isPrefixOf prefix full ) prefixes )

        -- Are we at the beginning of the string?
        isFirst   = position == 0

        -- Apply the context to whatever expression we were given, and recurse
        next = ( \expression -> (m xs (expression c)) )

        -- Get the character at the relative index to position, where -1 is the
        -- character before, and 1 is the next character. Returns a literal '*'
        -- if we don't have that character
        relChar = (\idx -> relCharOrStar original position idx)

--
-- Functions supporting the 'where' clause
--

-- Returns the char x places away from the current position, or a '*'
relCharOrStar :: String -> Int -> Int -> Char
relCharOrStar input currPos relPos =
  if ( ( absPos < 0 ) || ( absPos >= strLen ) ) then '*' else input !! absPos
  where absPos = currPos + relPos
        strLen = length input

