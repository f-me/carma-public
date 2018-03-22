-- For russian language features
module Utils.Sex
     ( Sex (..)
     , class GetSex
     , getSex
     , sexyShow
     ) where


data Sex = MaleSex | MiddleSex | FemaleSex

class GetSex a where
  getSex :: a -> Sex


sexyShow :: String -> String -> String -> Sex -> String
sexyShow male middle female sex =
  case sex of
       MaleSex   -> male
       MiddleSex -> middle
       FemaleSex -> female
