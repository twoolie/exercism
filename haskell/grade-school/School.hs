module School (
	sorted, add, grade, School.empty, School(), 
) where 

import Data.Maybe
import Data.Set as Set hiding (map)
import Data.Map as Map hiding (map)

type Grade = Int
type Student = String
newtype School = School {
	getGrades :: Map Grade (Set Student)
} deriving Show

liftSchool func = School . func . getGrades

empty :: School
empty = School Map.empty

add :: Grade -> Student -> School -> School
add grade name = liftSchool $ Map.insertWith Set.union grade (Set.singleton name)

grade :: Grade -> School -> [Student]
grade g = maybe [] Set.toList . Map.lookup g . getGrades

sorted :: School -> [(Grade, [Student])]
sorted = map (fmap Set.toList) . Map.toList . getGrades