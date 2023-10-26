-- Function that return the reverse of a string with all its alphabetic elements capitalized. 
--Use function toUpper :: Char -> Char
import Data.Char

reverseCaps :: [Char] -> [Char]
reverseCaps xs = map toUpper (reverse xs)
