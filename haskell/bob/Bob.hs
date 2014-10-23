--module Bob (responseFor) where

--import Data.Char
--import Data.List

--isQuestion, isShouting, isSilence :: String -> Bool

--isQuestion ins = "?" `isSuffixOf` ins
--isShouting ins = (any isUpper ins) && (not $ any isLower ins)
--isSilence  ins = all isSpace ins

--responseFor :: String -> String
--responseFor input | isShouting input = "Woah, chill out!"
--                  | isQuestion input = "Sure."
--                  | isSilence  input = "Fine. Be that way."
--                  | otherwise        = "Whatever."

module Bob (responseFor) where
import Data.Char(isUpper, isAlpha, isSpace)

data PromptType = Silence | Shout | Question | Other

responseFor :: String -> String
responseFor = respondTo . promptType

promptType :: String -> PromptType
promptType p | all isSpace p = Silence
             | any isAlpha p && (allAlphaIsUpper p)  = Shout
             | last p == '?' = Question
             | otherwise = Other

respondTo :: PromptType -> String
respondTo Shout = "Woah, chill out!"
respondTo Question = "Sure."
respondTo Silence = "Fine. Be that way."
respondTo Other = "Whatever."

allAlphaIsUpper :: String -> Bool
allAlphaIsUpper p = all isUpper $ filter isAlpha p