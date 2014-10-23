module SpaceAge (Planet(..), ageOn) where

data Planet = Earth   | Mercury | Venus  | Mars 
            | Jupiter | Saturn  | Uranus | Neptune 

secsPerYear = 31557600 :: Integer

orbitalPeriod planet = case planet of
    Earth   -> 1 :: Float
    Mercury -> 0.2408467
    Venus   -> 0.61519726
    Mars    -> 1.8808158
    Jupiter -> 11.862615
    Saturn  -> 29.447498
    Uranus  -> 84.016846
    Neptune -> 164.79132

ageOn :: Planet -> Integer -> Float
ageOn planet secs = 
	fromIntegral secs 
  / fromIntegral secsPerYear
  / orbitalPeriod planet